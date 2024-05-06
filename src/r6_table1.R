library(here)
here <- here()
here
library(tidyverse)
library(lubridate)
library(fs)
library(xts)
library(dygraphs)
library(readxl)
library(scales)
library(patchwork)


#import precip data
ppt <- readr::read_csv(paste0(here, "/data/W9_Streamflow_Precipitation.csv")) %>% 
  mutate(.after = datetime_EST, #indicates where the new column is placed
         datetime_EST2 = as.POSIXct(datetime_EST, format = "%m/%d/%Y %H:%M")) %>% 
  filter(minute(datetime_EST2) == 0) %>% #remove subhourly timestamps
  select(-W9_Streamflow_mm_hr) %>% 
  arrange(datetime_EST2) #Order from earliest to latest timestamp


#import precip interval and convert timezone to EST
ppt_interval <-readr::read_csv(paste0(here, "/data/hobo_ppt_interval_file.csv"))
tz(ppt_interval$Start_dt_EST)
ppt_interval2 <- ppt_interval%>% 
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(.after = End_dt_EST,
         datetime_interval_EST = lubridate::interval(start = Start_dt_EST,
                                                     end = End_dt_EST,
                                                     tz = "EST"),
         event_dur_sec = dseconds(datetime_interval_EST))

saveRDS(ppt_interval2, paste0(here, "/output/ppt_interval_final.Rds"))
write_csv(ppt_interval2, paste0(here, "/output/ppt_interval_final.csv"))


#load API function
source(paste0(here, "/src/API.R"))


#API for 30 days
ppt_api <- ppt %>% 
  arrange(datetime_EST2) %>% 
  mutate(api_30d = getApi(W9_Precipitation_mm, k = 0.9, n = 30, finite = TRUE))



#Filtering 30d API within ppt events
ppt_api_events <- slice(ppt_api, 0) 

for (i in 1:length(ppt_interval2$Event)) {
  interval <- ppt_api %>%
    filter(datetime_EST2 %within% ppt_interval2$datetime_interval_EST[i]) %>% 
    mutate(Event = ppt_interval2$Event[i]) 
  
  
  ppt_api_events  <- bind_rows(ppt_api_events, interval)
}


#saving precip events
saveRDS(ppt_api_events, paste0(here, "/output/ppt_events_with_API.Rds"))
write_csv(ppt_api_events, paste0(here, "/output/ppt_events_with_API.csv"))


#nesting API 30 days events
ppt_api_events2 <- ppt_api_events%>%
  group_by(Event)%>%
  nest() %>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x)))%>%
  mutate(data = map(data, ~summarise(.x, across(contains("api"), mean))))%>%
  unnest_wider(data) %>% 
  ungroup
  # rename("Event" = "event_n")
  


#Event summary of precipitation
event_summary<- inner_join(ppt_api_events, ppt_interval2,
                              by = c( "Event"))%>%
  mutate(event_dur_num = as.numeric(event_dur_sec))%>%
  group_by(Event)%>%
  mutate(max_ppt = max(W9_Precipitation_mm),
         total_ppt = sum(W9_Precipitation_mm),
            event_intensity = total_ppt/event_dur_num)%>%
  distinct(across(Event), .keep_all = TRUE)%>%
  select(datetime_interval_EST, event_dur_sec, event_intensity, total_ppt, Event,max_ppt )



all_events<-inner_join(ppt_api_events2, event_summary,
                       by = c( "Event"))

saveRDS(all_events, paste0(here, "/output/table1.Rds"))
write_csv(all_events, paste0(here, "/output/table1.csv"))






#Filtering ppt events
# ppt_events <- slice(ppt, 0)
# 
# for (i in 1:length(ppt_interval2$Event)) {
#   interval <- ppt %>%
#     filter(datetime_EST2 %within% ppt_interval2$datetime_interval_EST[i]) %>%
#     mutate(Event = ppt_interval2$Event[i])
# 
# 
#   ppt_events  <- bind_rows(ppt_events, interval)
# }
