library(here)
here <- here()
here
library(tidyverse)
library(lubridate)
library(fs)
library(xts)
library(dygraphs)
library(readxl)
library(ggplot2)
library(scales)


hobo_events <- readRDS(paste0(here, "/output/hobo_events.Rds"))
SFA <- hobo_events%>%
  filter(site == "SFA_mm") 

SFA_events <- readxl::read_excel(paste0(here, "/data/SFA.xlsx"))
tz(SFA_events$Start_dt_EST) #imported time zone is UTC
sapply(SFA_events, class)


SFA_events2 <- SFA_events %>% 
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(.after = End_dt_EST,
         datetime_interval_EST = lubridate::interval(start = Start_dt_EST,
                                                     end = End_dt_EST,
                                                     tz = "EST"),
         event_dur_sec = dseconds(datetime_interval_EST))



class(SFA_events2$datetime_interval_EST)
tz(SFA_events2$datetime_interval_EST) #timezone of interval gives an error
tz(SFA_events2$Start_dt_EST) #timezone of start is EST
   

#To check the interval is in EST, we can pull out the start
start <- int_start(SFA_events2$datetime_interval_EST[1])
class(start)
tz(start)

#create an empty dataframe with same headers
hobo_SFA_events <- slice(hobo_events, 0) 



for (i in 1:length(SFA_events2$recession_n)) {
  interval <- SFA %>%
    filter(dt %within% SFA_events2$datetime_interval_EST[i]) %>% 
    mutate(recession_n = SFA_events2$recession_n[i])
  
  hobo_SFA_events <- bind_rows(hobo_SFA_events, interval)
}


#Check and correct timezone
tz(hobo_SFA_events $dt)
attr(hobo_SFA_events$dt, "tzone") <- "EST"
tz(hobo_SFA_events $dt)
#Now the hourly time series record is filtered with event numbers


SFA_xts <- xts(hobo_SFA_events %>% select(dt, yield_mm), order.by=hobo_SFA_events$dt)
dygraph(SFA_xts) %>% dyAxis("y", valueRange = c(-1, 1)) %>% 
  dyRangeSelector()


# getApi(x, k = 0.9, n = 5, finite = TRUE)


