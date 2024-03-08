

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
library(broom)



# ppt <- readr::read_csv(paste0(here, "/data/W9_Streamflow_Precipitation.csv")) %>% 
#   mutate(.after = datetime_EST, #indicates where the new column is placed
#          datetime_EST2 = as.POSIXct(datetime_EST, format = "%m/%d/%Y %H:%M")) %>% 
#   filter(minute(datetime_EST2) == 0) %>% #remove subhourly timestamps
#   select(-W9_Streamflow_mm_hr) %>% 
#   arrange(datetime_EST2) #Order from earliest to latest timestamp
# 
# 
# 
# ppt_interval <-readr::read_csv(paste0(here, "/data/ppt_interval_fomatted.csv"))
# tz(ppt_interval$Start_dt_EST)
# ppt_interval2 <- ppt_interval%>% 
#   mutate(across(.cols = lubridate::is.POSIXct,
#                 ~ lubridate::force_tz(., tzone='EST'))) %>% 
#   mutate(.after = End_dt_EST,
#          datetime_interval_EST = lubridate::interval(start = Start_dt_EST,
#                                                      end = End_dt_EST,
#                                                      tz = "EST"),
#          event_dur_sec = dseconds(datetime_interval_EST))






# all_events<-inner_join(ppt_daily_api_events, nested_hobo_events,
#                        by = c( "recession_n")) %>%
#   group_by(recession_n, dt_interval)%>%
#   mutate(event_dur_num = as.numeric(dt_interval),
#          units="secs") %>% ##24hrs event
#   mutate(event_intensity = W9_Precipitation_mm/event_dur_num) %>% 
#   ungroup() %>% 
#   group_by(event_n) %>% 
#   mutate(cv = sd(m) / mean(m) * 100) %>% 
#   ungroup()







#currently on

ppt_events <-readr::read_csv(paste0(here,  "/output/ppt_events.csv"))%>%
  select(datetime_EST2, W9_Precipitation_mm, Event)%>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST')))
  
tz(ppt_events$datetime_EST2)



rec_values <- read_csv(paste0(here, "/data/rec_values.csv"))%>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::with_tz(., tzone='EST')))%>%
  rename("rec_yield" = "yield_mm")



rec_intervals <- read_csv(paste0(here, "/data/curve_intervals.csv"))%>%
  select(-notes)%>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::with_tz(., tzone='EST')))%>%
  rename("hobo_event_n" = "event_n")%>%
  drop_na()
  
  


rec_model <- read_csv(paste0(here, "/data/rec_model.csv"))%>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::with_tz(., tzone='EST')))%>%
  mutate(site = recession_n)%>%
  mutate(site = case_when(str_detect(site, "SFA") ~ "SF-A",
                          str_detect(site, "SFB") ~ "SF-B",
                          str_detect(site, "SFC") ~ "SF-C",
                          str_detect(site, "SFD") ~ "SF-D",
                          str_detect(site, "TFB") ~ "TF-B",
                          str_detect(site, "TFD") ~ "TF-D"))%>%
  ungroup()%>%
  select(recession_n, hobo_event_n, site, i, m)


  



merged_rec <- full_join(rec_values, rec_intervals,
                         by = c( "recession_n", "hobo_event_n", "site"))



centroid<-ppt_events %>% 
  group_by(Event) %>% 
  summarise(meanX=mean(datetime_EST2), meanY=mean(W9_Precipitation_mm))%>%
  rename("hobo_event_n" = "Event", "centroid" = "meanX")


DeltaT <- full_join(merged_rec , centroid,
                             by = c("hobo_event_n")) %>%
  mutate(Delta_T = lubridate::interval(start = centroid,
                                        end = Start_dt_EST,
                                        tz = "EST"),
         Delta_T_duration = dseconds(Delta_T))%>%
  distinct(recession_n, .keep_all = TRUE)%>%
  select(recession_n, hobo_event_n, site, datetime_interval_EST,
         event_dur_sec, centroid, Delta_T, Delta_T_duration)


Event_stats <- merged_rec%>%
  group_by(recession_n)%>%
  summarise(max_rec_yld = max(rec_yield),
         total_rec_yld = sum(rec_yield))%>%
  ungroup()


cv <- full_join(rec_intervals , Event_stats,
                by = c("recession_n")) %>%
  group_by(hobo_event_n) %>% 
  mutate(cv_dur = sd(event_dur_num) / mean(event_dur_num) * 100,
         cv_max = sd(max_rec_yld) / mean(max_rec_yld) * 100,
         cv_total = sd(total_rec_yld) / mean(total_rec_yld) * 100)%>%
  select(recession_n, hobo_event_n, site, cv_dur, cv_max, cv_total, max_rec_yld,
         total_rec_yld)%>%
  ungroup()
  



table2_r8 <- full_join(cv, DeltaT,
                       by = c("hobo_event_n","recession_n","site"))


table2_r8_2 <- full_join(table2_r8, rec_model,
                         by= c("hobo_event_n","recession_n", "site"))

