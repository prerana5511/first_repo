
library(here)
here <- here()
here
library(tidyverse)
library(lubridate)
library(fs)
library(xts)
library(dygraphs)
library(readxl)
# library(ggplot2)
library(scales)
library(patchwork)



ppt <- readr::read_csv(paste0(here, "/data/W9_Streamflow_Precipitation.csv")) %>% 
  mutate(.after = datetime_EST, #indicates where the new column is placed
         datetime_EST2 = as.POSIXct(datetime_EST, format = "%m/%d/%Y %H:%M")) %>% 
  filter(minute(datetime_EST2) == 0) %>% #remove subhourly timestamps
  select(-W9_Streamflow_mm_hr) %>% 
  arrange(datetime_EST2) #Order from earliest to latest timestamp



ppt_interval <-readr::read_csv(paste0(here, "/data/ppt_interval_fomatted.csv"))
tz(ppt_interval$Start_dt_EST)
ppt_interval2 <- ppt_interval%>% 
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(.after = End_dt_EST,
         datetime_interval_EST = lubridate::interval(start = Start_dt_EST,
                                                     end = End_dt_EST,
                                                     tz = "EST"),
         event_dur_sec = dseconds(datetime_interval_EST))


#load API function
source(paste0(here, "/src/API.R"))

# #Daily API
# ppt_daily <- ppt %>%
#   mutate(date = as.Date(datetime_EST2)) %>%
#   group_by(date)%>%
#   nest() %>%
#   mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x)))%>%
#   mutate(data = map(data, ~summarise(.x, across(where(is.numeric), sum))))%>%
#   unnest_wider(data) %>% 
#   ungroup()%>%
#   mutate(date = as.POSIXct(date))
# 
# sapply(ppt_daily, class)


ppt_api <- ppt %>% 
  arrange(datetime_EST2) %>% 
  mutate(api_30d = getApi(W9_Precipitation_mm, k = 0.9, n = 30, finite = TRUE))


# ppt_daily_api <- ppt_daily %>% 
#   arrange(date) %>% 
#   mutate(api_30d = getApi(W9_Precipitation_mm, k = 0.9, n = 30, finite = TRUE),
#          dt_start = force_tz(as_datetime(date), "EST"),
#          dt_end = force_tz(as_datetime(date) + hours(23) + minutes(59) + seconds(59),
#                            "EST"),
#          dt_interval = interval(dt_start, dt_end))


# curve_intervals <- read_csv(paste0(here, "/data/hobo_new_utf.csv"))
# tz(curve_intervals$Start_dt_EST)
# curve_intervals2 <- curve_intervals %>% 
#   mutate(across(.cols = lubridate::is.POSIXct,
#                 ~ lubridate::force_tz(., tzone='EST'))) %>% 
#   mutate(.after = End_dt_EST,
#          datetime_interval_EST = lubridate::interval(start = Start_dt_EST,
#                                                      end = End_dt_EST,
#                                                      tz = "EST"),
#          event_dur_sec = dseconds(datetime_interval_EST))

#Filtering daily API events
ppt_api_events <- slice(ppt_api, 0) 

for (i in 1:length(ppt_interval2$Event)) {
  interval <- ppt_api %>%
    filter(datetime_EST2 %within% ppt_interval2$datetime_interval_EST[i]) %>% 
    mutate(event_n = ppt_interval2$Event[i]) 
  
  
  ppt_api_events  <- bind_rows(ppt_api_events, interval)
}


ppt_api_events2 <- ppt_api_events%>%
  group_by(event_n)%>%
  nest() %>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x)))%>%
  mutate(data = map(data, ~summarise(.x, across(contains("api"), mean))))%>%
  unnest_wider(data) %>% 
  ungroup()%>%
  rename("Event" = "event_n")
  
  
  

# 
# #There are multiple SF/TF collector events per one ppt record, so there are only 58 periods in the daily ppt record that correspond to the 89 SF/TF event periods
# 
# ppt_api_events2 <- ppt_daily_api_events %>% 
#   distinct(event_n, .keep_all = TRUE)%>%
#   rename("Event" = "event_n")
#   # select(c(event_n, contains("api")))





#Filtering daily API events
ppt_events <- slice(ppt, 0) 

for (i in 1:length(ppt_interval2$Event)) {
  interval <- ppt %>%
    filter(datetime_EST2 %within% ppt_interval2$datetime_interval_EST[i]) %>% 
    mutate(Event = ppt_interval2$Event[i]) 
  
  
  ppt_events  <- bind_rows(ppt_events, interval)
}


  
  

event_summary<- inner_join(ppt_events, ppt_interval2,
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


