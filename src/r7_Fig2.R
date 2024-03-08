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
  pivot_wider(names_from = "site", values_from = "Rec_yld_norm")%>%
  pivot_longer(cols = c("SF-A", "SF-C") , names_to = "Sugar_Maple",
               values_to = "SM_yield")%>%
  pivot_longer(cols = c("SF-B", "SF-D") , names_to = "Yellow_Birch",
               values_to = "YB_yield")%>%
  pivot_longer(cols = c("SM_yield","YB_yield") , names_to = "stemflow",
               values_to = "SF_yield")%>%
  select(-Sugar_Maple, -Yellow_Birch)%>%
  distinct(SF_yield, .keep_all = TRUE)%>%
  drop_na()


#For hobo_n event
  SF_event <- hobo_event_norm %>%
  filter(!str_detect(site, "TF"))%>%
  select(Ev_yld_norm,site, dt, Event)%>%
  pivot_wider(names_from = "site", values_from = "Ev_yld_norm")%>%
  pivot_longer(cols = c("SF-A", "SF-C") , names_to = "Sugar_Maple",
               values_to = "SM_yield")%>%
  pivot_longer(cols = c("SF-B", "SF-D") , names_to = "Yellow_Birch",
               values_to = "YB_yield")%>%
  pivot_longer(cols = c("SM_yield","YB_yield") , names_to = "stemflow",
               values_to = "SF_yield")%>%
  select(-Sugar_Maple, -Yellow_Birch)%>%
  distinct(SF_yield, .keep_all = TRUE)





ppt_events_r7_2 <- ppt_events_r7%>%
  rename("dt" ="datetime_EST2")




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
  geom_point(aes(x=dt, y = SF_yield,group = stemflow, colour = stemflow),
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
  geom_point(aes(x= dt, y = recession_yield, group = recession_n, colour = recession_n),
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

ggsave(filename = "hobo_n_ppt_SF.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")









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
  geom_point(aes(x=dt, y = SF_yield,group = stemflow, colour = stemflow),
             data = SF_event%>%filter(Event == 2)) +
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
  geom_point(aes(x= dt, y = recession_yield, group = recession_n, colour = recession_n),
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

ggsave(filename = "hobo_n_ppt_SF.png", plot = t, path = paste0(here, "/output/figs/"),
       device = "png")



















# p <-ggplot()+
#   geom_bar(aes(x= dt, y= W9_Precipitation_mm),
#            data =ppt_events_r7_2%>%filter(Event == 9),stat='identity', colour = alpha( 'green', 0.7))+
#   geom_point(aes(x=dt, y = SF_yield, group = stemflow),
#              data = event8_SF_ppt%>%filter(Event == 9), colour = alpha( 'black', 0.7)) +
#   geom_point(aes(x= dt, y = SF_yield, group = stemflow, colour = stemflow), 
#              data = event8_SF%>%filter(Event == 9))

#New

# a <- full_join(event8_SF , event8_SF_ppt,
#                 by = c( "Event", "W9_Precipitation_mm", "dt", "stemflow", "SF_yield"))%>%
#   pivot_wider(names_from = stemflow, values_from = SF_yield)%>%
#   mutate(SM_yield = as.double(SM_yield[[1]]))%>%
#   mutate(YB_yield = as.numeric(YB_yield[[13]]))%>%
#   pivot_longer(cols=c('SM_yield', 'YB_yield'),
#                names_to='SF',
#                values_to='value')
# 
# a1 <- full_join(event8_SF , event8_SF_ppt,
#                by = c( "Event", "W9_Precipitation_mm", "dt", "stemflow", "SF_yield", "datetime_EST2"))%>%
#   pivot_wider(names_from = stemflow, values_from = SF_yield)%>%
#   mutate(SM_yield = as.double(SM_yield[[1]]))%>%
#   mutate(YB_yield = as.numeric(YB_yield[[181]]))%>%
#   pivot_longer(cols=c('SM_yield', 'YB_yield'),
#                names_to='SF',
#                values_to='SF_ev_yield')%>%
#   mutate(stemflow_yield = SF_ev_yield, R_yield = recession_yield, ppt_yield = W9_Precipitation_mm)%>%
#   pivot_longer(cols=c('SF_ev_yield', 'W9_Precipitation_mm', 'recession_yield'),
#                names_to='site',
#                values_to='yield')
#   
# 
# q <-ggplot(a1%>%filter(Event == 1))+
#   geom_bar(aes(x= datetime_EST2, y= ppt_yield),stat='identity', colour = alpha( 'green', 0.7))+
#   geom_point(aes(x=dt, y = stemflow_yield, group = SF), colour = alpha( 'black', 0.7)) +
#   geom_point(aes(x= dt, y = R_yield, group = recession_n, colour = recession_n))
# 
# q <-ggplot(a1%>%filter(Event == 1))+
#   geom_bar(aes(x= datetime_EST2, y= ppt_yield),stat='identity', colour = alpha( 'green', 0.7))+
#   geom_point(aes(x=dt, y = stemflow_yield, group = SF), colour = alpha( 'black', 0.7)) +
#   geom_point(aes(x= dt, y = R_yield, group = recession_n, colour = recession_n))
# 



#highlighting of stemflow done




#Throughfall

event8_TF_rec <- hobo_events_rec3%>%
  filter(str_detect(site, "TF"))%>%
  # filter(Event== 4)%>%
  select(Rec_yld_norm,site, recession_n, dt, Event, Recession_yield, yield_mm)%>%
  pivot_wider(names_from = "site", values_from = "Rec_yld_norm")%>%
  pivot_longer(cols = c("TF-B","TF-D") , names_to = "Throughfall",
               values_to = "TF_yield")%>%
  distinct(TF_yield, .keep_all = TRUE)



event8_TF_event <- hobo_events4%>%
  filter(str_detect(site, "TF"))%>%
  # filter(Event== 4)%>%
  select(Ev_yld_norm,site, dt, Event, Event_yield, yield_mm)%>%
  pivot_wider(names_from = "site", values_from = "Ev_yld_norm")%>%
  pivot_longer(cols = c("TF-B","TF-D") , names_to = "Throughfall",
               values_to = "TF_yield")%>%
  distinct(TF_yield, .keep_all = TRUE)



ppt_events_r7_2 <- ppt_events_r7%>%
  rename("dt" ="datetime_EST2")

event8_TF_ppt <- full_join(ppt_events_r7_2 , event8_TF_event,
                           by = c(  "dt", "Event"))%>%
  select(dt, Event_yield, Throughfall, TF_yield, Event, W9_Precipitation_mm)%>%
  distinct(TF_yield, .keep_all = TRUE)%>%
  drop_na()

event8_TF_ppt = event8_TF_ppt[-1,]  #since the first row was showing event = 1



event8_TF <-inner_join(event8_TF_rec , ppt_events_r7_2,
                       by = c( "dt", "Event"))%>%
  # filter(rec_dt==datetime_EST2)
  distinct(TF_yield, .keep_all = TRUE)


# 
# ev8_ppt_TF <-ggplot()+
#   geom_bar(aes(x= dt, y= W9_Precipitation_mm),
#            data =ppt_events_r7_2%>%filter(Event == 5),stat='identity', colour = alpha( 'green', 0.7))+
#   geom_point(aes(x=dt, y = TF_yield, group = Throughfall), 
#              data = event8_TF_ppt%>%filter(Event == 5), colour = alpha( 'black', 0.7)) +
#   geom_point(aes(x= dt, y = TF_yield, group = Throughfall, colour = Throughfall), 
#              data = event8_TF%>%filter(Event == 5))   

# ggplot()+
#   geom_point(aes(x=datetime_EST2, y = SF_yield, group = stemflow,colour = stemflow),
#              data = event8_SF_ppt ) +
#   geom_point(aes(x= datetime_EST2, y = SF_yield), 
#              data = event8_SF, colour = alpha( 'blue', 0.7)) 



p2 <- ev8_ppt_TF + plot_layout(ncol=1)
p2

ggsave(filename = "hobo_n_ppt_TF.png", plot = p2, path = paste0(here, "/output/figs/"),
       device = "png")


