
library(here)
here <- here()
here
library(tidyverse)
library(lubridate)
library(dygraphs)
library(xts)
library(highcharter)


precip <- readr::read_csv(paste0(here, "/data/W9_Streamflow_Precipitation.csv"))
ppt <- precip %>%
  mutate(.after = datetime_EST, 
         datetime_EST2 = as.POSIXct(datetime_EST, format = "%m/%d/%Y %H:%M"))


ppt2 <- ppt %>% select(datetime_EST2, W9_Precipitation_mm)
precip_xts <- xts(ppt2, order.by=ppt$datetime_EST2)
dygraph(precip_xts) %>% dyAxis("y", valueRange = c(-1, 1)) %>% 
  dyRangeSelector()
all <- ppt2 %>% rename("dt" = datetime_EST2) %>%
  drop_na()


hobo_events <- readRDS(paste0(here, "/output/hobo_events.Rds"))
  
  

all2 <- all %>% full_join(., hobo_events, by = join_by(dt)) %>%
  group_by(dt) %>% 
  arrange(dt) %>%
  ungroup()


all3 <- all2 %>%
  mutate(Date = lubridate::ymd_hms(dt)) %>%
  filter(Date >= as_datetime('2018-07-24') & Date <= as_datetime('2018-11-04'))%>%
  pivot_wider(names_from = "site", values_from = "yield_mm")
  
  
  
ppt3 <- ppt2%>%
  pivot_longer(cols = contains("W9"), names_to = "site", 
               values_to = "yield_mm") %>%
  group_by(site) 


hobo_events2 <- hobo_events %>% 
  mutate(yield_mm_clean = case_when(yield_mm < 0 | 
                                      is.na(yield_mm) ~ 0,
                                    TRUE ~ yield_mm)) %>% 
  ungroup() %>% 
  select(dt, site, hobo_event_n, yield_mm_clean) %>% 
  rename("yield_mm" = "yield_mm_clean")



#interactive plot 

all_xts <- xts(all3 %>% select(dt, W9_Precipitation_mm, SFA_mm, SFB_mm,
                               SFC_mm, SFD_mm,TFB_mm,TFD_mm),
               order.by=all3$dt)

dygraph(all_xts) %>% dyAxis("y", valueRange = c(-1, 100)) %>% 
  dyRangeSelector() %>% dyOptions(useDataTimezone = TRUE) %>%
  dySeries("W9_Precipitation_mm", stepPlot = TRUE, fillGraph = TRUE,
           color = "red") 



ppt_intervals <-readxl::read_excel(paste0(here, "/data/hobo_ppt_interval_file.xlsx"))
tz(ppt_intervals$Start_dt_EST) #imported time zone is UTC


ppt_intervals2 <- ppt_intervals %>% 
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(.after = End_dt_EST,
         datetime_interval_EST = lubridate::interval(start = Start_dt_EST,
                                                     end = End_dt_EST,
                                                     tz = "EST"),
         event_dur_sec = dseconds(datetime_interval_EST))

class(ppt_intervals2$datetime_interval_EST)
tz(ppt_intervals2$datetime_interval_EST) #timezone of interval gives an error
tz(ppt_intervals2$Start_dt_EST) #timezone of start is EST


#To check the interval is in EST, we can pull out the start
start <- int_start( ppt_intervals2$datetime_interval_EST[1])
class(start)
tz(start)


write_csv(ppt_intervals2,
          paste0(here,"/data/ppt_interval_fomatted.csv"))

#create an empty dataframe with same headers
ppt_events <- slice(ppt_intervals2, 0) 



for (i in 1:length(ppt_intervals2$Event)) {
  interval <- ppt2 %>%
    filter(datetime_EST2 %within% ppt_intervals2$datetime_interval_EST[i]) %>% 
    mutate(Event = ppt_intervals2$Event[i])
  
  ppt_events <- bind_rows(ppt_events, interval)
}


#Check and correct timezone
tz(ppt_events$datetime_EST2)
attr(ppt_events$datetime_EST2, "tzone") <- "EST"
tz(ppt_events$datetime_EST2)
#Now the hourly time series record is filtered with event numbers


write_csv(ppt_events, paste0(here, "/output/ppt_events.csv"))


centroid<-ppt_events %>% 
  group_by(Event) %>% 
    summarise(meanX=mean(datetime_EST2), meanY=mean(W9_Precipitation_mm))




#pivot data long for facetwrap
ppt_events2 <- ppt_events %>%

  pivot_longer(cols = contains("_mm"), names_to = "site",
               values_to = "yield_mm") %>%
  group_by(site, Event) %>%
  arrange(datetime_EST2) %>%
  ungroup() %>%
  select(datetime_EST2, site, Event, yield_mm) %>%
  drop_na()




#Calculate event statistics
ppt_event_summary <- ppt_events2 %>% 
  group_by(Event, site) %>% 
  dplyr::summarise(max_yld_mm = max(yield_mm, na.rm=T))

ppt_events_summry2 <- inner_join(ppt_events2, ppt_intervals2,
                          by = c("Event")) %>%
  group_by(datetime_interval_EST, event_dur_sec, Event)%>%
  mutate(event_dur_num = as.numeric(event_dur_sec))%>%
  summarise(total_yield = sum(yield_mm),
            event_intensity = total_yield/event_dur_num)





#Join the max rate with the time series to access the time of max for each event
ppt_events3 <- inner_join(ppt_events2, ppt_event_summary,
                           by = c("site", "Event")) %>% 
  rownames_to_column()

#manually keep the max yield rows and remove the duplicates 
ppt_events4 <- ppt_events3 %>% 
  mutate(to_keep1 = case_when(yield_mm < max_yld_mm ~ FALSE,
                             TRUE ~ TRUE )) %>%
  mutate(to_keep2 = case_when (rowname %in% c(119) ~ FALSE, #two peaks (equal) 
                              TRUE ~ TRUE))%>%
  filter(to_keep1) %>% select(-c( to_keep1)) %>% 
  filter(to_keep2) %>% select(-c( to_keep2,rowname)) %>% 
  rename("dt_max_yield_mm" = "datetime_EST2")

ppt_events5 <- ppt_events2 %>% 
  left_join(., ppt_events4 %>% select(1:3),
            by = c("site", "Event")) %>% 
  group_by(site, Event) %>% 
  mutate(limb = case_when(datetime_EST2 < dt_max_yield_mm ~ "rising",
                          datetime_EST2 > dt_max_yield_mm ~ "falling",
                          datetime_EST2== dt_max_yield_mm ~ "peak")) %>% 
  select(-dt_max_yield_mm)

ggplot(ppt_events5) +
  geom_line(mapping=aes(x=datetime_EST2, y= yield_mm,  color = limb))+
  geom_point(mapping=aes(x=datetime_EST2, y= yield_mm,  color = limb))+
  facet_wrap(~Event, scales = "free")+
  labs(title="Precipitation (R3)",
       x ="Datetime", y = "Yield mm")






hobo_events_limbs_r3<- readRDS(paste0(here, "/output/hobo_limbs.Rds"))%>%
  filter(limb == "peak")%>%
  rename("Event" = "hobo_event_n")%>%
  rownames_to_column()%>%
  mutate(to_keep = case_when (rowname %in% c(26,53,54,27, 28) ~ FALSE,
                               TRUE ~ TRUE))%>%
  filter(to_keep) %>% select(-c( to_keep, rowname))





SF <- hobo_events_limbs_r3%>%
  filter(!str_detect(site, "TF"))%>%
  pivot_wider(names_from = "site", values_from = "roll_yield")%>%
  pivot_longer(cols = c("SFA_mm","SFC_mm") , names_to = "Sugar", 
               values_to = "Sugar_Maple_mm")%>%
  pivot_longer(cols = c("SFB_mm","SFD_mm") , names_to = "Yellow", 
               values_to = "Yellow_Birch_mm")%>%
  pivot_longer(cols = c("Sugar_Maple_mm","Yellow_Birch_mm") , names_to = "Stemflow", 
               values_to = "Stemflow_yield")%>%
  select(-Sugar, -Yellow)%>%
  distinct(across(Stemflow_yield), .keep_all = TRUE)%>%
  na.omit()



TF <- hobo_events_limbs_r3%>%
  filter(str_detect(site, "TF"))%>%
  pivot_wider(names_from = "site", values_from = "roll_yield")%>%
  pivot_longer(cols = c("TFB_mm","TFD_mm") , names_to = "throughfall", 
               values_to = "throughfall_yield")%>%
  #select(-TFB_mm, -Yellow)%>%
  #distinct(across(throughfall_yield), .keep_all = TRUE)%>%
  na.omit()




Flowpath <-hobo_events_limbs_r3%>%
  pivot_wider(names_from = "site", values_from = "roll_yield")%>%
  pivot_longer(cols = c("SFA_mm","SFC_mm") , names_to = "Sugar", 
               values_to = "Sugar_Maple_mm")%>%
  pivot_longer(cols = c("SFB_mm","SFD_mm") , names_to = "Yellow", 
               values_to = "Yellow_Birch_mm")%>%
  pivot_longer(cols = c("TFB_mm","TFD_mm") , names_to = "Throughfall", 
               values_to = "Throughfall_yield")%>%
  select(dt, Sugar_Maple_mm, Yellow_Birch_mm, Throughfall_yield, Event, dt_max_yield_mm,dt)%>%
  pivot_longer(cols = c("Sugar_Maple_mm", "Throughfall_yield", "Yellow_Birch_mm") , names_to = "site", 
               values_to = "path_yield_rate")%>%
  distinct(across(path_yield_rate), .keep_all = TRUE)%>%
  na.omit()
  

#Time lag calculation
time_lag_cal_Flowpath <-inner_join(Flowpath , centroid,
                              by = c("Event")) %>%
  mutate(time_lag = lubridate::interval(start = meanX,
                                        end = dt_max_yield_mm,
                                        tz = "EST"),
         time_lag_duration = dseconds(time_lag))%>%
  #select(Event, dt_max_yield_mm,meanX,time_lag,time_lag_duration, yield_mm, max_yld_mm) %>%
  rename("centroid_time" = "meanX")


ppt_events7 <-inner_join(ppt_events_summry2, time_lag_cal_Flowpath,
                         by = c("Event")) %>%
  group_by(site, Event)%>%
  distinct(across(Event),.keep_all = TRUE)

ggplot(ppt_events7, aes(x= time_lag_duration, y= event_intensity, colour = site)) +
  geom_point()




write_csv(ppt_events7, paste0(here, "/output/time_lag_centroid_peak.csv"))




rm(all,all2,all3,all4,all_xts, hobo_events, interval)
rm(ppt,ppt2,precip,precip_xts, ppt_intervals,ppt_intervals2, centroid)
rm(ppt_events,ppt_events2,ppt_events3,ppt_events4,ppt_events5,ppt_events6, 
   ppt_event_summary)


