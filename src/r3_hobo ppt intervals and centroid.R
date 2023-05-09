
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
  filter(Date >= as_datetime('2018-07-24') & Date <= as_datetime('2018-11-04'))


#for visualizing and comparing

all4 <- all3 %>% 
  pivot_longer(cols = contains("_mm"), names_to = "site", 
               values_to = "yield_mm") %>% 
  group_by(site, dt) %>% 
  arrange(dt)%>% 
  ungroup()%>% 
  select(dt, site,  yield_mm)%>%
  drop_na()



ggplot(all4)+
  geom_line(mapping=aes(x= dt, y= yield_mm, colour = site, group = site),
            linewidth = 1) +
  labs(x =  "\n Datetime", y= "Yield in mm \n", title = " Time vs. Yield \n") +
  theme_bw() + facet_wrap(~site, scales = "free_y", ncol = 1)+#scales optional 
  theme(legend.position="none") 

ggplot( all4, aes(x = dt, y = yield_mm, color = site) ) +
  geom_smooth(se = FALSE, method = "loess", span = .1) +
  scale_x_continuous( breaks = seq(2018-07-24, 2018-11-03, by = 0.04) )+
  theme_bw() + facet_wrap(~site, scales = "free_y", ncol = 1)+#scales optional 
  theme(legend.position="none")




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


#Plot ppt
theme_set(theme_bw())
ggplot(ppt_events2 %>% filter(str_detect(site, "W9"))) +
  geom_line(mapping=aes(x=datetime_EST2, y= yield_mm, color = site))+
  facet_wrap(~Event, scales = "free")

ggplot( ppt_events2 , aes(x = datetime_EST2, y = yield_mm, color = site) ) +
  geom_smooth(se = FALSE, method = "loess", span = 0.5)+
  scale_x_continuous( breaks = seq(2018-07-24, 2018-11-03, by = 0.04) )+
  theme_bw() + facet_wrap(~Event, scales = "free")



#Calculate event statistics
ppt_event_summary <- ppt_events2 %>% 
  group_by(Event,site) %>% 
  dplyr::summarise(max_yld_mm = max(yield_mm, na.rm=T))


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
  facet_wrap(~Event, scales = "free")


ggplot( ppt_events5 , aes(x = datetime_EST2, y = yield_mm, color = limb) ) +
  geom_smooth(se = FALSE, method = "loess", span = 5) +
  scale_x_continuous( breaks = seq(2018-07-24, 2018-11-03, by = 0.04) )+
  theme_bw() + facet_wrap(~Event, scales = "free")
assign("last.warning", NULL, envir = baseenv())



#time lag calculation

ppt_events6 <-inner_join(ppt_events4, centroid,
                         by = c("Event")) %>%
  mutate(time_lag = lubridate::interval(start = ppt_events4$dt_max_yield_mm,
                                        end = centroid$meanX,
                                        tz = "EST"),
         time_lag_duration = dseconds(time_lag))%>%
  select(Event, dt_max_yield_mm,meanX,time_lag,time_lag_duration) %>%
  rename("centroid_time" = "meanX")


write_csv(ppt_events6, paste0(here, "/output/time_lag_centroid_peak.csv"))




rm(all,all2,all3,all4,all_xts, hobo_events, interval)
rm(ppt,ppt2,precip,precip_xts, ppt_intervals,ppt_intervals2, centroid)
rm(ppt_events,ppt_events2,ppt_events3,ppt_events4,ppt_events5,ppt_events6, 
   ppt_event_summary)


