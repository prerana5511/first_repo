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


#import precip events
ppt_events_r7<- readRDS(paste0(here,"/output/ppt_events_with_API.Rds"))%>%
  select(Event, datetime_EST2, W9_Precipitation_mm)%>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST')))


tz(ppt_events_r7$datetime_EST2) 


#import precip interval and convert timezone to EST
ppt_interval <-readr::read_csv(paste0(here, "/data/hobo_ppt_interval_file.csv"))
tz(ppt_interval$Start_dt_EST)
ppt_intervals2 <- ppt_interval%>% 
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(.after = End_dt_EST,
         datetime_interval_EST = lubridate::interval(start = Start_dt_EST,
                                                     end = End_dt_EST,
                                                     tz = "EST"),
         event_dur_sec = dseconds(datetime_interval_EST))

tz(ppt_intervals2$Start_dt_EST)



hobo_events <- readRDS(paste0(here, "/output/hobo_events.Rds"))%>%
  rename("event_yield" = "yield_mm", )


hobo_events_rec <-readRDS(paste0(here, "/output/rec_values.Rds"))%>%
  rename("recession_yield" = "yield_mm")

hobo_merged <- full_join(hobo_events , hobo_events_rec,
                         by = c(  "dt","hobo_event_n", "site", "site2"))





#Filtering hobo events from ppt intervals
hobo_from_ppt_events <- slice(ppt_interval, 0) 


for (i in 1:length(ppt_intervals2$Event)) {
  interval <- hobo_merged %>%
    filter(dt %within% ppt_intervals2$datetime_interval_EST[i]) %>% 
    mutate(Event = ppt_intervals2$Event[i])
  
  hobo_from_ppt_events <- bind_rows(hobo_from_ppt_events, interval)
}


hobo_event_norm <- hobo_from_ppt_events%>%
  select(-Start_dt_EST, -End_dt_EST, -recession_yield, -Notes)%>%
  group_by(hobo_event_n)%>%
  mutate(Ev_yld_norm = event_yield + abs(min(event_yield)))%>%
  ungroup()

  

hobo_rec_norm<- hobo_from_ppt_events%>%
  select(-Start_dt_EST, -End_dt_EST, -event_yield, -Notes)%>%
  group_by(recession_n)%>%
  mutate(rec_yld_norm = recession_yield + abs(min(recession_yield)))%>%
  ungroup()%>%
  drop_na()

  

#Filtering stemflow from all hobo events
  SF_event <- hobo_event_norm %>%
  filter(!str_detect(site, "TF"))%>%
  select(Ev_yld_norm,site, site2, dt, Event, recession_n)
  

  
#Filtering stemflow from all recession events
  SF_rec <- hobo_rec_norm %>%
    filter(!str_detect(site, "TF"))%>%
    select(rec_yld_norm,site, site2, dt, Event, recession_n)


#Renaming for merging the plots
ppt_events_r7_2 <- ppt_events_r7%>%
  rename("dt" ="datetime_EST2")



#TF

#Filtering throughfall from all hobo events
TF_event <- hobo_event_norm %>%
  filter(str_detect(site, "TF"))


#Filtering throughfall from all recession events
TF_rec <- hobo_rec_norm %>%
  filter(str_detect(site, "TF"))






#EVENT 1

t1 <- ggplot()+
  geom_bar(aes(x= dt, y= W9_Precipitation_mm),
           data =ppt_events_r7_2 %>% 
             filter(Event == 1), 
           stat='identity', colour = alpha( 'green', 0.7))+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-07-26 09:00:00 EST"),
                 as.POSIXct("2018-07-27 06:00:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-07-26 09:00:00 EST"),
      as.POSIXct("2018-07-27 06:00:00 EST")
    )
  )



t2 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = SF_event%>%filter(Event == 1))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = SF_rec %>%filter(Event == 1)) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-07-26 09:00:00 EST"),
                 as.POSIXct("2018-07-27 06:00:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-07-26 09:00:00 EST"),
      as.POSIXct("2018-07-27 06:00:00 EST")
    )
  )




t3 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = TF_event%>%filter(Event == 1))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = TF_rec %>%filter(Event == 1)) +  
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-07-26 09:00:00 EST"),
                 as.POSIXct("2018-07-27 06:00:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-07-26 09:00:00 EST"),
      as.POSIXct("2018-07-27 06:00:00 EST")
    )
  )


t <- t1+ t2 +t3 + plot_layout(ncol=1)
t
ggsave(filename = "Fig_2_Ev_1.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")



#EVENT 2 
t1 <- ggplot()+
  geom_bar(aes(x= dt, y= W9_Precipitation_mm),
           data =ppt_events_r7_2%>%filter(Event == 2),stat='identity', colour = alpha( 'green', 0.7))+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-03 13:30:00 EST"),
                 as.POSIXct("2018-09-04 13:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-03 13:30:00 EST"),
      as.POSIXct("2018-09-04 13:30:00 EST")
    )
  )



t2 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = SF_event%>%filter(Event == 2))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = SF_rec %>%filter(Event == 2)) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-03 13:30:00 EST"),
                 as.POSIXct("2018-09-04 13:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-03 13:30:00 EST"),
      as.POSIXct("2018-09-04 13:30:00 EST")
    )
  )




t3 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = TF_event%>%filter(Event == 2))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = TF_rec %>%filter(Event == 2)) + 
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-03 13:30:00 EST"),
                 as.POSIXct("2018-09-04 13:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-03 13:30:00 EST"),
      as.POSIXct("2018-09-04 13:30:00 EST")
    )
  )


t <- t1+ t2 +t3 + plot_layout(ncol=1)

ggsave(filename = "Fig_2_ev_2.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")




#EVENT 3

t1 <- ggplot()+
  geom_bar(aes(x= dt, y= W9_Precipitation_mm),
           data =ppt_events_r7_2%>%filter(Event == 3),stat='identity', colour = alpha( 'green', 0.7))+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-10 21:30:00 EST"),
                 as.POSIXct("2018-09-11 21:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-10 21:30:00 EST"),
      as.POSIXct("2018-09-11 21:30:00 EST")
    )
  )



t2 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = SF_event%>%filter(Event == 3))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = SF_rec %>%filter(Event == 3)) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-10 21:30:00 EST"),
                 as.POSIXct("2018-09-11 21:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-10 21:30:00 EST"),
      as.POSIXct("2018-09-11 21:30:00 EST")
    )
  )




t3 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = TF_event%>%filter(Event == 3))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = TF_rec %>%filter(Event == 3)) + 
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-10 21:30:00 EST"),
                 as.POSIXct("2018-09-11 21:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-10 21:30:00 EST"),
      as.POSIXct("2018-09-11 21:30:00 EST")
    )
  )


t <- t1+ t2 +t3 + plot_layout(ncol=1)

ggsave(filename = "Fig_2_ev_3.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")





#EVENT 4

t1 <- ggplot()+
  geom_bar(aes(x= dt, y= W9_Precipitation_mm),
           data =ppt_events_r7_2%>%filter(Event == 4),stat='identity', colour = alpha( 'green', 0.7))+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-21 06:30:00 EST"),
                 as.POSIXct("2018-09-22 06:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-21 06:30:00 EST"),
      as.POSIXct("2018-09-22 06:30:00 EST")
    )
  )



t2 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = SF_event%>%filter(Event == 4))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = SF_rec %>%filter(Event == 4)) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-21 06:30:00 EST"),
                 as.POSIXct("2018-09-22 06:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-21 06:30:00 EST"),
      as.POSIXct("2018-09-22 06:30:00 EST")
    )
  )




t3 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = TF_event%>%filter(Event == 4))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = TF_rec %>%filter(Event == 4)) + 
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-21 06:30:00 EST"),
                 as.POSIXct("2018-09-22 06:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-21 06:30:00 EST"),
      as.POSIXct("2018-09-22 06:30:00 EST")
    )
  )


t <- t1+ t2 +t3 + plot_layout(ncol=1)

ggsave(filename = "Fig_2_ev_4.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")





#EVENT 5

t1 <- ggplot()+
  geom_bar(aes(x= dt, y= W9_Precipitation_mm),
           data =ppt_events_r7_2%>%filter(Event == 5),stat='identity', colour = alpha( 'green', 0.7))+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-26 13:30:00 EST"),
                 as.POSIXct("2018-09-27 13:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-26 13:30:00 EST"),
      as.POSIXct("2018-09-27 13:30:00 EST")
    )
  )



t2 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = SF_event%>%filter(Event == 5))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = SF_rec %>%filter(Event == 5)) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-26 13:30:00 EST"),
                 as.POSIXct("2018-09-27 13:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-26 13:30:00 EST"),
      as.POSIXct("2018-09-27 13:30:00 EST")
    )
  )




t3 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = TF_event%>%filter(Event == 5))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = TF_rec %>%filter(Event == 5)) + 
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-26 13:30:00 EST"),
                 as.POSIXct("2018-09-27 13:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-26 13:30:00 EST"),
      as.POSIXct("2018-09-27 13:30:00 EST")
    )
  )


t <- t1+ t2 +t3 + plot_layout(ncol=1)

ggsave(filename = "Fig_2_ev_5.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")



#EVENT 6

t1 <- ggplot()+
  geom_bar(aes(x= dt, y= W9_Precipitation_mm),
           data =ppt_events_r7_2%>%filter(Event == 6),stat='identity', colour = alpha( 'green', 0.7))+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-10-02 14:30:00 EST"),
                 as.POSIXct("2018-10-03 14:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-10-02 14:30:00 EST"),
      as.POSIXct("2018-10-03 14:30:00 EST")
    )
  )



t2 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = SF_event%>%filter(Event == 6))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = SF_rec %>%filter(Event == 6)) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-10-02 14:30:00 EST"),
                 as.POSIXct("2018-10-03 14:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-10-02 14:30:00 EST"),
      as.POSIXct("2018-10-03 14:30:00 EST")
    )
  )




t3 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = TF_event%>%filter(Event == 6))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = TF_rec %>%filter(Event == 6)) + 
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-10-02 14:30:00 EST"),
                 as.POSIXct("2018-10-03 14:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-10-02 14:30:00 EST"),
      as.POSIXct("2018-10-03 14:30:00 EST")
    )
  )


t <- t1+ t2 +t3 + plot_layout(ncol=1)

ggsave(filename = "Fig_2_ev_6.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")




#EVENT 7

t1 <- ggplot()+
  geom_bar(aes(x= dt, y= W9_Precipitation_mm),
           data =ppt_events_r7_2%>%filter(Event == 7),stat='identity', colour = alpha( 'green', 0.7))+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-10-11 14:30:00 EST"),
                 as.POSIXct("2018-10-12 14:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-10-11 14:30:00 EST"),
      as.POSIXct("2018-10-12 14:30:00 EST")
    )
  )



t2 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = SF_event%>%filter(Event == 7))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = SF_rec %>%filter(Event == 7)) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-10-11 14:30:00 EST"),
                 as.POSIXct("2018-10-12 14:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-10-11 14:30:00 EST"),
      as.POSIXct("2018-10-12 14:30:00 EST")
    )
  )




t3 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = TF_event%>%filter(Event == 7))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = TF_rec %>%filter(Event == 7)) + 
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-10-11 14:30:00 EST"),
                 as.POSIXct("2018-10-12 14:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-10-11 14:30:00 EST"),
      as.POSIXct("2018-10-12 14:30:00 EST")
    )
  )


t <- t1+ t2 +t3 + plot_layout(ncol=1)

ggsave(filename = "Fig_2_ev_7.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")




#EVENT 8

t1 <- ggplot()+
  geom_bar(aes(x= dt, y= W9_Precipitation_mm),
           data =ppt_events_r7_2%>%filter(Event == 8),stat='identity', colour = alpha( 'green', 0.7))+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-11-02 04:30:00 EST"),
                 as.POSIXct("2018-11-03 04:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-11-02 04:30:00 EST"),
      as.POSIXct("2018-11-03 04:30:00 EST")
    )
  )



t2 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = SF_event%>%filter(Event == 8))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = SF_rec %>%filter(Event == 8)) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-11-02 04:30:00 EST"),
                 as.POSIXct("2018-11-03 04:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-11-02 04:30:00 EST"),
      as.POSIXct("2018-11-03 04:30:00 EST")
    )
  )




t3 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = TF_event%>%filter(Event == 8))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = TF_rec %>%filter(Event == 8)) + 
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-11-02 04:30:00 EST"),
                 as.POSIXct("2018-11-03 04:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-11-02 04:30:00 EST"),
      as.POSIXct("2018-11-03 04:30:00 EST")
    )
  )


t <- t1+ t2 +t3 + plot_layout(ncol=1)

ggsave(filename = "Fig_2_ev_8.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")





#EVENT 9

t1 <- ggplot()+
  geom_bar(aes(x= dt, y= W9_Precipitation_mm),
           data =ppt_events_r7_2%>%filter(Event == 9),stat='identity', colour = alpha( 'green', 0.7))+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-11-03 08:30:00 EST"),
                 as.POSIXct("2018-11-04 08:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-11-03 08:30:00 EST"),
      as.POSIXct("2018-11-04 08:30:00 EST")
    )
  )



t2 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = SF_event%>%filter(Event == 9))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = SF_rec %>%filter(Event == 9)) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-11-03 08:30:00 EST"),
                 as.POSIXct("2018-11-04 08:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-11-03 08:30:00 EST"),
      as.POSIXct("2018-11-04 08:30:00 EST")
    )
  )




t3 <- ggplot()+
  # geom_point(aes(x=dt, y = Ev_yld_norm),
  #            data = TF_event%>%filter(Event == 9))+
  geom_point(aes(x=dt, y = rec_yld_norm,group = site2, colour = site2),
             data = TF_rec %>%filter(Event == 9)) + 
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-11-03 08:30:00 EST"),
                 as.POSIXct("2018-11-04 08:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-11-03 08:30:00 EST"),
      as.POSIXct("2018-11-04 08:30:00 EST")
    )
  )


t <- t1+ t2 +t3 + plot_layout(ncol=1)

ggsave(filename = "Fig_2_ev_9.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")









# 
# ppt_interval <-readr::read_csv(paste0(here, "/data/ppt_interval_fomatted.csv"))
# tz(ppt_interval$Start_dt_GMT)
# ppt_intervals2 <- ppt_interval%>% 
#   mutate(across(.cols = lubridate::is.POSIXct,
#                 ~ lubridate::with_tz(., tzone='EST'))) %>% 
#   mutate(.after = End_dt_GMT,
#          datetime_interval_EST = lubridate::interval(start = Start_dt_GMT,
#                                                      end = End_dt_GMT,
#                                                      tz = "EST"),
#          event_dur_sec = dseconds(datetime_interval_EST))
# 
# tz(ppt_intervals2$Start_dt_GMT)

