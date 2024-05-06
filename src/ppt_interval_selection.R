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


precip_xts <- xts(ppt%>% select(datetime_EST2, W9_Precipitation_mm), order.by=ppt$datetime_EST2)
dygraph(precip_xts) %>% #dyAxis("y", valueRange = c(-1, 1)) %>%
  dyRangeSelector()
all <- ppt %>% select(datetime_EST2, W9_Precipitation_mm) %>% rename("dt" = datetime_EST2) %>%
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



ppt3 <- ppt%>%
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

all_xts <- xts(all3 %>% select(dt, W9_Precipitation_mm, 'SF-A', 'SF-B',
                               'SF-C', 'SF-D','TF-B','TF-D'),
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
  interval <- ppt %>%
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
