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

ppt_events_r7 <- read_csv(paste0(here, "/output/ppt_events.csv"))%>%
  select(Event, datetime_EST2, W9_Precipitation_mm)%>%
  drop_na()

ppt_interval <- read_csv(paste0(here, "/data/ppt_interval_fomatted.csv"))


tz(ppt_interval$Start_dt_EST) #imported time zone is UTC


ppt_intervals2 <- ppt_interval %>% 
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






hobo_events <- readRDS(paste0(here, "/output/hobo_events.Rds"))%>%
  rename("event_yield" = "yield_mm", )

hobo_events2 <- hobo_events %>%
  mutate(site = case_when(site == "SFA_mm" ~ "SF-A",
                          site == "SFB_mm" ~ "SF-B",
                          site == "SFC_mm" ~ "SF-C",
                          site == "SFD_mm" ~ "SF-D",
                          site == "TFB_mm" ~ "TF-B",
                          site == "TFD_mm" ~ "TF-D"))

hobo_events_rec <-read_csv(paste0(here, "/data/rec_values.csv"))%>%
  rename("recession_yield" = "yield_mm")

hobo_merged <- full_join(hobo_events2 , hobo_events_rec,
                         by = c(  "dt","hobo_event_n", "site"))





#Filtering hobo events from ppt intervals
hobo_from_ppt_events <- slice(ppt_interval, 0) 


for (i in 1:length(ppt_intervals2$Event)) {
  interval <- hobo_merged %>%
    filter(dt %within% ppt_intervals2$datetime_interval_EST[i]) %>% 
    mutate(Event = ppt_intervals2$Event[i])
  
  hobo_from_ppt_events <- bind_rows(hobo_from_ppt_events, interval)
}


hobo_event_norm <- hobo_from_ppt_events%>%
  select(-Start_dt_EST, -End_dt_EST, -datetime_interval_EST,-event_dur_sec,
         -recession_yield, - recession_n, -Notes)%>%
  drop_na()%>%
  group_by(hobo_event_n)%>%
  mutate(Ev_yld_norm = event_yield + abs(min(event_yield)))%>%
  distinct(event_yield, .keep_all = TRUE)%>%
  ungroup()


hobo_rec_norm <- hobo_from_ppt_events%>%
  select(-Start_dt_EST, -End_dt_EST, -datetime_interval_EST,-event_dur_sec,
         -event_yield, -hobo_event_n, -Notes, -site)%>%
  drop_na()%>%
  group_by(recession_n)%>%
  mutate(Rec_yld_norm = recession_yield + abs(min(recession_yield)))%>%
  distinct(recession_yield, .keep_all = TRUE)%>%
  mutate(site = recession_n)%>%
  mutate(site = case_when(str_detect(site, "SFA") ~ "SF-A",
                          str_detect(site, "SFB") ~ "SF-B",
                          str_detect(site, "SFC") ~ "SF-C",
                          str_detect(site, "SFD") ~ "SF-D",
                          str_detect(site, "TFB") ~ "TF-B",
                          str_detect(site, "TFD") ~ "TF-D"))%>%
  ungroup()




#For Recession_n event
  SF_rec <- hobo_rec_norm %>%
  filter(!str_detect(site, "TF"))%>%
  select(Rec_yld_norm,site, recession_n, dt, Event, recession_yield)%>%
    mutate(site = recession_n)%>%
    mutate(site = case_when(str_detect(site, "SFA") ~ "SM",
                            str_detect(site, "SFB") ~ "YB",
                            str_detect(site, "SFC") ~ "SM",
                            str_detect(site, "SFD") ~ "YB"))%>%
  distinct(Rec_yld_norm, .keep_all = TRUE)%>%
  drop_na()


#For hobo_n event
  SF_event <- hobo_event_norm %>%
  filter(!str_detect(site, "TF"))%>%
  select(Ev_yld_norm,site, dt, Event)%>%
    mutate(site = case_when(str_detect(site, "SF-A") ~ "SM",
                            str_detect(site, "SF-B") ~ "YB",
                            str_detect(site, "SF-C") ~ "SM",
                            str_detect(site, "SF-D") ~ "YB"))%>%
  distinct(Ev_yld_norm, .keep_all = TRUE)





ppt_events_r7_2 <- ppt_events_r7%>%
  rename("dt" ="datetime_EST2")


#TF


#For Recession_n event
TF_rec <- hobo_rec_norm %>%
  filter(str_detect(site, "TF"))%>%
  select(Rec_yld_norm,site, recession_n, dt, Event, recession_yield)%>%
  # mutate(site = recession_n)%>%
  # mutate(site = case_when(str_detect(site, "TF-B") ~ "TF",
  #                         str_detect(site, "TF-D") ~ "TF"))%>%
  distinct(Rec_yld_norm, .keep_all = TRUE)



#For hobo_n event
TF_event <- hobo_event_norm %>%
  filter(str_detect(site, "TF"))%>%
  select(Ev_yld_norm,site, dt, Event)%>%
  # mutate(site = case_when(str_detect(site, "TF-B") ~ "TF",
  #                         str_detect(site, "TF-D") ~ "TF"))%>%
  distinct(Ev_yld_norm, .keep_all = TRUE)






#EVENT 1

t1 <- ggplot()+
  geom_bar(aes(x= dt, y= W9_Precipitation_mm),
           data =ppt_events_r7_2%>%filter(Event == 1),stat='identity', colour = alpha( 'green', 0.7))+
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
  geom_point(aes(x=dt, y = Ev_yld_norm,group = site, colour = site),
             data = SF_event%>%filter(Event == 1)) +
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
  geom_point(aes(x= dt, y = Rec_yld_norm, group = site, colour = site),
             data = SF_rec%>%filter(Event == 1) )+
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

ggsave(filename = "Fig_2_Ev_1.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")


p1 <- ggplot()+
  geom_point(mapping=aes(x=dt, y = Ev_yld_norm, group = site, colour = site),
             data = TF_event%>%filter(Event == 1)) +
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




p2 <- ggplot()+
  geom_point(mapping=aes(x= dt, y = Rec_yld_norm, group = site, colour = site),
             data = TF_rec%>%filter(Event == 1) )+
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


p <- p1+ p2 + plot_layout(ncol=1)



f = t + p + plot_layout(ncol=1)





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
  geom_point(aes(x= dt, y = Ev_yld_norm, group = site, colour = site),
             data = SF_event%>%filter(Event == 2)) +
  # geom_point(aes(x= dt, y = Rec_yld_norm, group = site, colour = site),
  #            data = SF_rec%>%filter(Event == 2))+
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
  geom_point(aes(x= dt, y = Rec_yld_norm, group = site, colour = site),
             data = SF_rec%>%filter(Event == 2) )+
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


p1 <- ggplot()+
  geom_point(mapping=aes(x=dt, y = Ev_yld_norm, group = site, colour = site),
             data = TF_event%>%filter(Event == 2)) +
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




p2 <- ggplot()+
  geom_point(mapping=aes(x= dt, y = Rec_yld_norm, group = site, colour = site),
             data = TF_rec%>%filter(Event == 2) )+
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


p <- p1+ p2 + plot_layout(ncol=1)



f = t + p + plot_layout(ncol=1)



#EVENT 3

t1 <- ggplot()+
  geom_bar(aes(x= dt, y= W9_Precipitation_mm),
           data =ppt_events_r7_2%>%filter(Event == 3),stat='identity', colour = alpha( 'green', 0.7))+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-10 17:30:00 EST"),
                 as.POSIXct("2018-09-11 17:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-10 17:30:00 EST"),
      as.POSIXct("2018-09-11 17:30:00 EST")
    )
  )



t2 <- ggplot()+
  geom_point(aes(x= dt, y = Ev_yld_norm, group = site, colour = site),
             data = SF_event%>%filter(Event == 3))+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-10 17:30:00 EST"),
                 as.POSIXct("2018-09-11 17:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-10 17:30:00 EST"),
      as.POSIXct("2018-09-11 17:30:00 EST")
    )
  )




t3 <- ggplot()+
  geom_point(aes(x= dt, y = Rec_yld_norm, group = site, colour = site),
             data = SF_rec%>%filter(Event == 3) )+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-10 17:30:00 EST"),
                 as.POSIXct("2018-09-11 17:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-10 17:30:00 EST"),
      as.POSIXct("2018-09-11 17:30:00 EST")
    )
  )


t <- t1+ t2 +t3 + plot_layout(ncol=1)

ggsave(filename = "Fig_2_ev_3.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")



p1 <- ggplot()+
  geom_point(mapping=aes(x=dt, y = Ev_yld_norm, group = site, colour = site),
             data = TF_event%>%filter(Event == 3)) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-10 17:30:00 EST"),
                 as.POSIXct("2018-09-11 17:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-10 17:30:00 EST"),
      as.POSIXct("2018-09-11 17:30:00 EST")
    )
  )




p2 <- ggplot()+
  geom_point(mapping=aes(x= dt, y = Rec_yld_norm, group = site, colour = site),
             data = TF_rec%>%filter(Event == 3) )+
  scale_x_datetime(
    breaks = seq(as.POSIXct("2018-09-10 17:30:00 EST"),
                 as.POSIXct("2018-09-11 17:30:00 EST"), "3 hours"),
    labels = date_format("%d-%b\n%H:%M", tz = "EST"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2018-09-10 17:30:00 EST"),
      as.POSIXct("2018-09-11 17:30:00 EST")
    )
  )


p <- p1+ p2 + plot_layout(ncol=1)



f = t + p + plot_layout(ncol=1)














