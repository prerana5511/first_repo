
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

all2 <- all %>% full_join(., hobo_events ) %>%
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
  select(dt, site, yield_mm)%>%
  drop_na()
  

ggplot(all4) +
  geom_line(mapping=aes(x= dt, y= yield_mm, colour = site, group = site),
            linewidth = 1) +
  labs(x =  "\n Datetime", y= "Yield in mm \n", title = " Time vs. Yield \n") +
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


#Creating interval: 1

interval_ppt <- slice(all3, 0) 

interval1 <- all3 %>%
  filter(Date >= as_datetime('2018-07-25 22:30:00') & 
           Date <= as_datetime('2018-07-26 12:30:00')) %>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(interval = lubridate::interval(start = "2018-07-25 22:30:00",
                                                      end = "2018-07-26 12:30:00",
                                                      tz = "EST"))%>%
  mutate(Event = 1)%>%
  select(interval, Date, Event, W9_Precipitation_mm )%>%
  drop_na()

ggplot(interval1) +
  geom_line(mapping=aes(x= Date, y= W9_Precipitation_mm),
            linewidth = 1) +
  labs(x =  "\n Datetime", y= "Yield in mm \n", title = " Time vs. Yield - Event :1 \n") +
  theme_bw() 



#finding the centroid

flow1 <- structure(interval1$W9_Precipitation_mm, .Dim = c(15L, 1L), 
                   index = structure(interval1$Date, tzone = "EST", 
                                     tclass = c("POSIXct", "POSIXt")), class = c("xts", "zoo"), 
                   .indexCLASS = c("POSIXct", "POSIXt"), 
                   tclass = c("POSIXct", "POSIXt"), .indexTZ = "EST", tzone = "EST", 
                   .Dimnames = list(NULL, structure("Flow", .Dim = c(1L, 1L))))

tz(interval1$Date)

tt1 <- as.numeric(time(flow1))
as.POSIXct(weighted.mean(tt1, flow1), origin = "1970-01-01", tz="EST",usetz=TRUE) 



#Creating interval: 2

interval2 <- all3 %>%
  filter(Date >= as_datetime('2018-09-03 06:30:00') & 
           Date <= as_datetime('2018-09-03 17:30:00')) %>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(interval = lubridate::interval(start = "2018-09-03 06:30:00",
                                        end = "2018-09-03 17:30:00",
                                        tz = "EST"))%>%
  mutate(Event = 2)%>%
  select(interval,Date, Event, W9_Precipitation_mm )%>%
  drop_na()

interval_ppt <- bind_rows(interval1, interval2)


ggplot(interval2) +
  geom_line(mapping=aes(x= Date, y= W9_Precipitation_mm),
            linewidth = 1) +
  labs(x =  "\n Datetime", y= "Yield in mm \n", title = " Time vs. Yield - Event : 2 \n") +
  theme_bw() 

#finding the centroid

flow2 <- structure(interval2$W9_Precipitation_mm, .Dim = c(12L, 1L), 
                   index = structure(interval2$Date, tzone = "EST", 
                                     tclass = c("POSIXct", "POSIXt")), class = c("xts", "zoo"), 
                   .indexCLASS = c("POSIXct", "POSIXt"), 
                   tclass = c("POSIXct", "POSIXt"), .indexTZ = "EST", tzone = "EST", 
                   .Dimnames = list(NULL, structure("Flow", .Dim = c(1L, 1L))))

tt2 <- as.numeric(time(flow2))
as.POSIXct(weighted.mean(tt2, flow2), origin = "1970-01-01", tz="EST",usetz=TRUE) 


#Creating interval: 3

interval3 <- all3 %>%
  filter(Date >= as_datetime('2018-09-10 12:30:00') & 
           Date <= as_datetime('2018-09-11 02:30:00')) %>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(interval = lubridate::interval(start = "	2018-09-10 12:30:00",
                                        end = "2018-09-11 02:30:00",
                                        tz = "EST"))%>%
  mutate(Event = 3)%>%
  select(interval, Date, Event, W9_Precipitation_mm )%>%
  drop_na()

interval_ppt <- bind_rows(interval_ppt, interval3)


ggplot(interval3) +
  geom_line(mapping=aes(x= Date, y= W9_Precipitation_mm),
            linewidth = 1) +
  labs(x =  "\n Datetime", y= "Yield in mm \n", title = " Time vs. Yield - Event : 3\n") +
  theme_bw() 

#finding the centroid

flow3 <- structure(interval3$W9_Precipitation_mm, .Dim = c(15L, 1L), 
                   index = structure(interval3$Date, tzone = "EST", 
                                     tclass = c("POSIXct", "POSIXt")), class = c("xts", "zoo"), 
                   .indexCLASS = c("POSIXct", "POSIXt"), 
                   tclass = c("POSIXct", "POSIXt"), .indexTZ = "EST", tzone = "EST", 
                   .Dimnames = list(NULL, structure("Flow", .Dim = c(1L, 1L))))

tt3 <- as.numeric(time(flow3))
as.POSIXct(weighted.mean(tt3, flow3), origin = "1970-01-01", tz="EST",usetz=TRUE) 



#Creating interval: 4

interval4 <- all3 %>%
  filter(Date >= as_datetime('2018-09-20 19:30:00') & 
           Date <= as_datetime('2018-09-21 01:30:00')) %>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(interval = lubridate::interval(start = "2018-09-20 19:30:00",
                                        end = "2018-09-21 01:30:00",
                                        tz = "EST"))%>%
  mutate(Event = 4)%>%
  select(interval, Date, Event, W9_Precipitation_mm )%>%
  drop_na()

interval_ppt <- bind_rows(interval_ppt, interval4)


ggplot(interval4) +
  geom_line(mapping=aes(x= Date, y= W9_Precipitation_mm),
            linewidth = 1) +
  labs(x =  "\n Datetime", y= "Yield in mm \n", title = " Time vs. Yield - Event : 4\n") +
  theme_bw() 

#finding the centroid

flow4 <- structure(interval4$W9_Precipitation_mm, .Dim = c(7L, 1L), 
                   index = structure(interval4$Date, tzone = "EST", 
                                     tclass = c("POSIXct", "POSIXt")), class = c("xts", "zoo"), 
                   .indexCLASS = c("POSIXct", "POSIXt"), 
                   tclass = c("POSIXct", "POSIXt"), .indexTZ = "EST", tzone = "EST", 
                   .Dimnames = list(NULL, structure("Flow", .Dim = c(1L, 1L))))

tt4 <- as.numeric(time(flow4))
as.POSIXct(weighted.mean(tt4, flow4), origin = "1970-01-01", tz="EST",usetz=TRUE) 


#Creating interval: 5

interval5 <- all3 %>%
  filter(Date >= as_datetime('2018-09-26 03:30:00') & 
           Date <= as_datetime('2018-09-26 11:30:00')) %>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(interval = lubridate::interval(start = "2018-09-26 03:30:00",
                                        end = "2018-09-26 11:30:00",
                                        tz = "EST"))%>%
  mutate(Event = 5)%>%
  select(interval, Date, Event, W9_Precipitation_mm )%>%
  drop_na()

interval_ppt <- bind_rows(interval_ppt, interval5)


ggplot(interval5) +
  geom_line(mapping=aes(x= Date, y= W9_Precipitation_mm),
            linewidth = 1) +
  labs(x =  "\n Datetime", y= "Yield in mm \n", title = " Time vs. Yield - Event : 5\n") +
  theme_bw() 

#finding the centroid

flow5 <- structure(interval5$W9_Precipitation_mm, .Dim = c(9L, 1L), 
                   index = structure(interval5$Date, tzone = "EST", 
                                     tclass = c("POSIXct", "POSIXt")), class = c("xts", "zoo"), 
                   .indexCLASS = c("POSIXct", "POSIXt"), 
                   tclass = c("POSIXct", "POSIXt"), .indexTZ = "EST", tzone = "EST", 
                   .Dimnames = list(NULL, structure("Flow", .Dim = c(1L, 1L))))

tt5 <- as.numeric(time(flow5))
as.POSIXct(weighted.mean(tt5, flow5), origin = "1970-01-01", tz="EST",usetz=TRUE) 


#Creating interval: 6

interval6 <- all3 %>%
  filter(Date >= as_datetime('2018-10-01 13:30:00') & 
           Date <= as_datetime('2018-10-02 10:30:00')) %>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(interval = lubridate::interval(start = "2018-10-01 13:30:00",
                                        end = "	2018-10-02 10:30:00",
                                        tz = "EST"))%>%
  mutate(Event = 6)%>%
  select(interval, Date, Event, W9_Precipitation_mm )%>%
  drop_na()

interval_ppt <- bind_rows(interval_ppt, interval6)


ggplot(interval6) +
  geom_line(mapping=aes(x= Date, y= W9_Precipitation_mm),
            linewidth = 1) +
  labs(x =  "\n Datetime", y= "Yield in mm \n", title = " Time vs. Yield - Event : 6\n") +
  theme_bw() 

#finding the centroid

flow6 <- structure(interval6$W9_Precipitation_mm, .Dim = c(22L, 1L), 
                   index = structure(interval6$Date, tzone = "EST", 
                                     tclass = c("POSIXct", "POSIXt")), class = c("xts", "zoo"), 
                   .indexCLASS = c("POSIXct", "POSIXt"), 
                   tclass = c("POSIXct", "POSIXt"), .indexTZ = "EST", tzone = "EST", 
                   .Dimnames = list(NULL, structure("Flow", .Dim = c(1L, 1L))))

tt6 <- as.numeric(time(flow6))
as.POSIXct(weighted.mean(tt6, flow6), origin = "1970-01-01", tz="EST",usetz=TRUE) 


#Creating interval: 7

interval7 <- all3 %>%
  filter(Date >= as_datetime('2018-10-10 17:30:00') & 
           Date <= as_datetime('2018-10-11 09:30:00')) %>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(interval = lubridate::interval(start = "2018-10-10 17:30:00",
                                        end = "2018-10-11 09:30:00",
                                        tz = "EST"))%>%
  mutate(Event = 7)%>%
  select(interval, Date, Event, W9_Precipitation_mm )%>%
  drop_na()

interval_ppt <- bind_rows(interval_ppt, interval7)



ggplot(interval7) +
  geom_line(mapping=aes(x= Date, y= W9_Precipitation_mm),
            linewidth = 1) +
  labs(x =  "\n Datetime", y= "Yield in mm \n", title = " Time vs. Yield - Event : 7\n") +
  theme_bw() 

#finding the centroid

flow7 <- structure(interval7$W9_Precipitation_mm, .Dim = c(17L, 1L), 
                   index = structure(interval7$Date, tzone = "EST", 
                                     tclass = c("POSIXct", "POSIXt")), class = c("xts", "zoo"), 
                   .indexCLASS = c("POSIXct", "POSIXt"), 
                   tclass = c("POSIXct", "POSIXt"), .indexTZ = "EST", tzone = "EST", 
                   .Dimnames = list(NULL, structure("Flow", .Dim = c(1L, 1L))))

tt7 <- as.numeric(time(flow7))
as.POSIXct(weighted.mean(tt7, flow7), origin = "1970-01-01", tz="EST",usetz=TRUE) 


#Creating interval: 8

interval8 <- all3 %>%
  filter(Date >= as_datetime('2018-10-31 23:30:00') & 
           Date <= as_datetime('2018-11-02 03:30:00')) %>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(interval = lubridate::interval(start = "2018-10-31 23:30:00",
                                        end = "2018-11-02 03:30:00",
                                        tz = "EST"))%>%
  mutate(Event = 8)%>%
  select(interval, Date, Event, W9_Precipitation_mm )%>%
  drop_na()

interval_ppt <- bind_rows(interval_ppt, interval8)


ggplot(interval8) +
  geom_line(mapping=aes(x= Date, y= W9_Precipitation_mm),
            linewidth = 1) +
  labs(x =  "\n Datetime", y= "Yield in mm \n", title = " Time vs. Yield - Event : 8\n") +
  theme_bw() 

#finding the centroid

flow8 <- structure(interval8$W9_Precipitation_mm, .Dim = c(29L, 1L), 
                   index = structure(interval8$Date, tzone = "EST", 
                                     tclass = c("POSIXct", "POSIXt")), class = c("xts", "zoo"), 
                   .indexCLASS = c("POSIXct", "POSIXt"), 
                   tclass = c("POSIXct", "POSIXt"), .indexTZ = "EST", tzone = "EST", 
                   .Dimnames = list(NULL, structure("Flow", .Dim = c(1L, 1L))))

tt8 <- as.numeric(time(flow8))
as.POSIXct(weighted.mean(tt8, flow8), origin = "1970-01-01", tz="EST",usetz=TRUE) 


#Creating interval: 9

interval9 <- all3 %>%
  filter(Date >= as_datetime('2018-11-02 23:30:00') & 
           Date <= as_datetime('2018-11-03 18:30:00')) %>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(interval = lubridate::interval(start = "2018-11-02 23:30:00",
                                        end = "2018-11-03 18:30:00",
                                        tz = "EST"))%>%
  mutate(Event = 9)%>%
  select(interval, Date, Event, W9_Precipitation_mm )%>%
  drop_na()

interval_ppt <- bind_rows(interval_ppt, interval9) 


ggplot(interval9) +
  geom_line(mapping=aes(x= Date, y= W9_Precipitation_mm),
            linewidth = 1) +
  labs(x =  "\n Datetime", y= "Yield in mm \n", title = " Time vs. Yield - Event : 9\n") +
  theme_bw() 

#finding the centroid

flow9 <- structure(interval9$W9_Precipitation_mm, .Dim = c(20L, 1L), 
                   index = structure(interval9$Date, tzone = "EST", 
                                     tclass = c("POSIXct", "POSIXt")), class = c("xts", "zoo"), 
                   .indexCLASS = c("POSIXct", "POSIXt"), 
                   tclass = c("POSIXct", "POSIXt"), .indexTZ = "EST", tzone = "EST", 
                   .Dimnames = list(NULL, structure("Flow", .Dim = c(1L, 1L))))

tt9 <- as.numeric(time(flow9))
as.POSIXct(weighted.mean(tt9, flow9), origin = "1970-01-01", tz="EST",usetz=TRUE) 



#Creating excel with only the interval

only_interval <- interval_ppt %>%
  select(interval, Event)%>%
  distinct(interval, Event)

write_csv(x = only_interval,
          paste0(here,"/output/hobo_ppt_interval_file.csv"))


rm(interval1,interval2,interval3, interval4,
   interval5, interval6,interval7, interval8, interval9, 
   only_interval, interval_ppt)

rm(all,all2,all3,all4,all_xts)

rm(tt1,tt2,tt3,tt4,tt5,tt6,tt7,tt8,tt9)

rm(flow1,flow2, flow3,flow4,flow5,flow6,flow7,flow8, flow9)

rm(ppt,ppt2,precip,precip_xts, hobo_events)


