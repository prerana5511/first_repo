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
library(broom)

#Import ppt events
ppt_events <-readr::read_csv(paste0(here,  "/output/ppt_events_with_API.csv"))%>%
  mutate(.after = datetime_EST, #indicates where the new column is placed
         datetime_EST2 = as.POSIXct(datetime_EST, format = "%m/%d/%Y %H:%M"))%>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST')))

#Check timezone
tz(ppt_events$datetime_EST2)


#Import recession values
rec_values <- read_csv(paste0(here, "/output/rec_values.csv"))%>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::with_tz(., tzone='EST')))%>%
  rename("rec_yield" = "yield_mm")


#Import recession intervals
rec_intervals <- read_csv(paste0(here, "/output/curve_intervals.csv"))%>%
  select(-notes)%>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST')))%>%
  rename("hobo_event_n" = "event_n")%>%
  drop_na()
  
  

#Import recession models and coefficients
rec_model <- read_csv(paste0(here, "/output/rec_model.csv"))%>%
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



centroid1<-ppt_events %>% 
  drop_na %>%
  group_by(Event) %>%
  arrange(datetime_EST2) %>% 
  mutate(midpoint = sum(W9_Precipitation_mm)/2) %>%
  mutate(cum_ppt = cumsum(W9_Precipitation_mm)) %>%
  filter(cum_ppt > midpoint) %>%
  mutate(centroid1 = min(cum_ppt)) %>%
  filter(cum_ppt == centroid1)


centroid2<-ppt_events %>% 
  drop_na%>%
  group_by(Event)%>%
  mutate(midpoint = sum(W9_Precipitation_mm)/2)%>%
  mutate(cum_ppt = cumsum(W9_Precipitation_mm))%>%
  filter(cum_ppt < midpoint)%>%
  mutate(centroid2 = max(cum_ppt))%>%
  filter(cum_ppt == centroid2)


centroid <- full_join(centroid1, centroid2,
                      by = c( "Event", "cum_ppt", "datetime_EST2", "midpoint",
                              "W9_Precipitation_mm"))%>%
  group_by(Event)%>%
  mutate(centroid = median(datetime_EST2)) %>%
  rename("hobo_event_n" = "Event")%>%
  ungroup()
  


DeltaT <- inner_join(merged_rec , centroid,
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


cv_all <- inner_join(rec_intervals , rec_model,
                by = c("recession_n", "hobo_event_n", "site")) %>%
  group_by(hobo_event_n) %>% 
  mutate(cv_i = sd(i) / mean(i) * 100,
         cv_m = sd(m) / mean(m) * 100)%>%
  select(recession_n, hobo_event_n, site, cv_i, cv_m)%>%
  ungroup()
  

cv_SF <- inner_join(rec_intervals , rec_model,
                          by = c("recession_n", "hobo_event_n", "site"))%>%
  filter(!str_detect(site, "TF"))%>%
  group_by(hobo_event_n) %>% 
  mutate(cv_i = sd(i) / mean(i) * 100,
         cv_m = sd(m) / mean(m) * 100)%>%
  select(recession_n, hobo_event_n, site, cv_i, cv_m)%>%
  ungroup()


cv_TF <- inner_join(rec_intervals , rec_model,
                   by = c("recession_n", "hobo_event_n", "site"))%>%
  filter(str_detect(site, "TF"))%>%
  group_by(hobo_event_n) %>% 
  mutate(cv_i = sd(i) / mean(i) * 100,
         cv_m = sd(m) / mean(m) * 100)%>%
  select(recession_n, hobo_event_n, site, cv_i, cv_m)%>%
  ungroup()




table2_r8 <- inner_join(cv_all, DeltaT,
                       by = c("hobo_event_n","recession_n","site"))


table2_r8_2 <- inner_join(table2_r8, rec_model,
                         by= c("hobo_event_n","recession_n", "site"))


write_csv(table2_r8_2, paste0(here, "/output/table2.csv"))



