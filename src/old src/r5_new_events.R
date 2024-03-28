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



hobo_events <- readRDS(paste0(here, "/output/hobo_events.Rds"))

ppt_events_r5 <- read_csv(paste0(here, "/output/ppt_events.csv"))


#to avoid weird issues with Excel formating, use csv for importing. 
#Example csv I created quickly
curve_intervals <- read_csv(paste0(here, "/data/hobo_new_utf.csv"))
tz(curve_intervals$Start_dt_EST)
curve_intervals2 <- curve_intervals %>% 
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::force_tz(., tzone='EST'))) %>% 
  mutate(.after = End_dt_EST,
         datetime_interval_EST = lubridate::interval(start = Start_dt_EST,
                                                     end = End_dt_EST,
                                                     tz = "EST"),
         event_dur_sec = dseconds(datetime_interval_EST),
         event_dur_num = as.numeric(event_dur_sec))

write_csv(curve_intervals2,
          paste0(here,"/data/curve_intervals.csv"))
# tz(curve_intervals2$datetime_interval_EST)
sapply(curve_intervals2, class)

#Filter out multiple intervals 

#edit site ids to match
hobo_events2 <- hobo_events %>%
  mutate(site = case_when(site == "SFA_mm" ~ "SF-A",
                          site == "SFB_mm" ~ "SF-B",
                          site == "SFC_mm" ~ "SF-C",
                          site == "SFD_mm" ~ "SF-D",
                          site == "TFB_mm" ~ "TF-B",
                          site == "TFD_mm" ~ "TF-D"))

#get list of site names to loop through
sites <- unique(curve_intervals2$site)

#create an empty dataframe with same headers
hobo_events_new <- slice(hobo_events2, 0) 

#create a double loop to loop through sites and through curve intervals within each site
for(i in sites) {
 ts <- hobo_events2 %>% filter(site == i) #filter time series
 int <- curve_intervals2 %>% filter(site == i) #filter interval list
  for (j in 1:length(int$recession_n)) {
  interval <- ts %>%
    filter(dt %within% int$datetime_interval_EST[j]) %>% 
    mutate(recession_n = int$recession_n[j])
  
  hobo_events_new <- bind_rows(hobo_events_new, interval)
}
}
rm(ts, int, interval)
#Check and correct timezone
tz(hobo_events_new$dt)


write_csv(hobo_events_new,
          paste0(here,"/data/rec_values.csv"))



xts <- xts(hobo_events_new %>% select(dt, yield_mm), order.by=hobo_events_new$dt)
dygraph(xts) %>% #dyAxis("y", valueRange = c(-1, 1)) %>%
  # dyOptions(connectSeparatedPoints = FALSE)%>%
  dygraphs::dyOptions(drawPoints = T, strokeWidth = 0, pointSize = 5)%>%
  dyRangeSelector()

#connect separated points



#Testing the antecedent precipitation index function
#load and format precipitation data
ppt <- readr::read_csv(paste0(here, "/data/W9_Streamflow_Precipitation.csv")) %>% 
  mutate(.after = datetime_EST, #indicates where the new column is placed
         datetime_EST2 = as.POSIXct(datetime_EST, format = "%m/%d/%Y %H:%M")) %>% 
  filter(minute(datetime_EST2) == 0) %>% #remove subhourly timestamps
  select(-W9_Streamflow_mm_hr) %>% 
  arrange(datetime_EST2) #Order from earliest to latest timestamp

#load API function
source(paste0(here, "/src/API.R"))

#UNDERSTAND EXAMPLES
## the larger n, the closer are the solutions 
x <- rexp(1000)
plot(x) #simulated rain record
#x = rain record; k=decay factor; n=timesteps to use; finite TRUE means use finite number of timesteps indicated by n; finite FALSE means do not use n
api1 <- getApi(x=x,k= 0.9, n=10, finite=TRUE)
plot(api1)
api2 <- getApi(x=x, finite=FALSE)
plot(api2)
plot(api1 ~ api2)
api22 <- getApi(x=x, n=10, finite=FALSE)
plot(api1~api22)

x <- rexp(1000)
api3 <- getApi(x=x,n=300, finite=TRUE) 
plot(api3) #notice first api starts at 300
api4 <- getApi(x=x,finite=FALSE)
plot(api4) #api calculated since first day
plot(api3~api4)

par(mfcol=c(2,1))
plot(x=api1,y=api2)
abline(a=0,b=1,col=2)
plot(x=api3,y=api4)
abline(a=0,b=1,col=2)
dev.off()
## user defined weights
x <- rexp(1000)
plot(x)
k <- 1/(15:1)
plot(k)
api5 <- getApi(x=x,k=k, finite=TRUE)
plot(api5)

ppt_api <- ppt %>% 
  arrange(datetime_EST2) %>% 
  mutate(api_24hr = getApi(W9_Precipitation_mm, k = 0.9, n = 24, finite = TRUE),
         api_10d = getApi(W9_Precipitation_mm, k = 0.9, n = 24*10, finite = TRUE),
         api_30d = getApi(W9_Precipitation_mm, k = 0.9, n = 24*30, finite = TRUE),
         api_inf = getApi(W9_Precipitation_mm, k = 0.9, finite = FALSE))%>%
  select(-datetime_EST)

ggplot(ppt_api) +
  geom_line(mapping = aes(x=datetime_EST2, y=api_24hr)) +
  geom_line(mapping = aes(x=datetime_EST2, y=api_10d), color = "blue") +
  geom_line(mapping = aes(x=datetime_EST2, y=api_30d), color = "green") +
  geom_line(mapping = aes(x=datetime_EST2, y=api_inf), color = "red")

#There's not much difference for long time series at hourly intervals

#Next steps: Calculate API as I've done above using daily precipitation 
#Generate a table of the average hourly API for each of the 9 events and the average daily API for each event

#understand the double for loop and do it to get the api


#Daily API
ppt_daily <- ppt %>%
  mutate(date = as.Date(datetime_EST2)) %>%
  group_by(date)%>%
  nest() %>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x)))%>%
  mutate(data = map(data, ~summarise(.x, across(where(is.numeric), sum))))%>%
  unnest_wider(data) %>% 
  ungroup()%>%
  mutate(date = as.POSIXct(date))

sapply(ppt_daily, class)

ppt_daily_api <- ppt_daily %>% 
  arrange(date) %>% 
  mutate(api_1d = getApi(W9_Precipitation_mm, k = 0.9, n=1, finite = TRUE),
         api_10d = getApi(W9_Precipitation_mm, k = 0.9, n = 10, finite = TRUE),
         api_30d = getApi(W9_Precipitation_mm, k = 0.9, n = 30, finite = TRUE),
         api_inf = getApi(W9_Precipitation_mm, k = 0.9, finite = FALSE),
         dt_start = force_tz(as_datetime(date), "EST"),
         dt_end = force_tz(as_datetime(date) + hours(23) + minutes(59) + seconds(59),
                           "EST"),
         dt_interval = interval(dt_start, dt_end))

ggplot(ppt_daily_api) +
  geom_line(mapping = aes(x=date, y=api_1d)) +
  geom_line(mapping = aes(x=date, y=api_10d), color = "blue") +
  geom_line(mapping = aes(x=date, y=api_30d), color = "green") +
  geom_line(mapping = aes(x=date, y=api_inf), color = "red")
#There is a difference when using daily values
#30d and infinite are not much different

#Filtering daily API events
ppt_daily_api_events <- slice(ppt_daily_api, 0) 

for (i in 1:length(curve_intervals2$event_n)) {
  interval <- ppt_daily_api %>%
    filter(curve_intervals2$datetime_interval_EST[i] %within% dt_interval) %>% 
    mutate(event_n = curve_intervals2$event_n[i]) %>%
    mutate(recession_n = curve_intervals2$recession_n[i])
  
  ppt_daily_api_events  <- bind_rows(ppt_daily_api_events, interval)
}
#There are multiple SF/TF collector events per one ppt record, so there are only 58 periods in the daily ppt record that correspond to the 89 SF/TF event periods

ppt_daily_api_events2 <- ppt_daily_api_events %>% 
  distinct(event_n, .keep_all = TRUE) %>% 
  select(c(event_n, contains("api")))




# hourly_ppt_events <- slice(ppt, 0)
# 
# for (i in 1:length(curve_intervals2$event_n)) {
#   interval <- ppt %>%
#     filter( datetime_EST2 %within% curve_intervals2$datetime_interval_EST[i]) %>%
#     mutate(event_n = curve_intervals2$event_n[i]) %>%
#     mutate(recession_n = curve_intervals2$recession_n[i])
#   
#   hourly_ppt_events <- bind_rows(hourly_ppt_events, interval)
# }







#models
hobo_events_new2 <- hobo_events_new %>%
  mutate(sec = seconds(dt) - min(seconds(dt)),
         sec_norm = sec/max(sec),
         yld_mm_norm1 = yield_mm - min(yield_mm),
         yld_mm_norm2 = yld_mm_norm1/max(yld_mm_norm1),
         power_yield = 10^yld_mm_norm2)

hobo_events_new2[c('power_yield')][sapply(hobo_events_new2[c('power_yield')], is.infinite)] <- NA

nested_hobo_events <- hobo_events_new2 %>%
  drop_na()%>%
  group_by(recession_n) %>%
  nest() %>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x))) %>%
  mutate(mod = map(data, ~lm(power_yield ~ sec_norm, data= .)),
         r = map_dbl(.x = data, .f = ~cor(y=(.x$power_yield), x = .x$sec_norm,
                                          use = "na.or.complete")),
         m = map_dbl(data, ~lm(power_yield~ sec_norm, data = .)$coefficients[[2]]),
         i = map_dbl(data, ~lm(power_yield ~ sec_norm, data = .)$coefficients[[1]]),
         r2 = r^2) %>% 
  unnest(data) %>%
  ungroup() %>%
  # mutate(cv = sd(m) / mean(m) * 100) %>% #cv is same for all
  distinct(across(recession_n), .keep_all = TRUE)

ggplot(nested_hobo_events) + 
  geom_jitter(mapping = aes(x=as.factor(hobo_event_n), y= m, colour = site))


all_events<-inner_join(ppt_daily_api_events, nested_hobo_events,
                       by = c( "recession_n")) %>%
  group_by(recession_n, dt_interval)%>%
  mutate(event_dur_num = as.numeric(dt_interval),
         units="secs") %>% ##24hrs event
  mutate(event_intensity = W9_Precipitation_mm/event_dur_num) %>% 
  ungroup() %>% 
  group_by(event_n) %>% 
  mutate(cv = sd(m) / mean(m) * 100) %>% 
  ungroup()
  
sapply(all_events, class)


t1 <-ggplot(all_events %>% filter(str_detect(site, "TF"))) + 
  geom_jitter(mapping = aes(x=cv , y= api_1d, colour = site))
t2 <-ggplot(all_events %>% filter(str_detect(site, "TF"))) + 
  geom_jitter(mapping = aes(x=cv , y= api_10d, colour = site))
t3<-ggplot(all_events %>% filter(str_detect(site, "TF"))) + 
  geom_jitter(mapping = aes(x=cv , y= api_30d, colour = site))
t4<-ggplot(all_events %>% filter(str_detect(site, "TF"))) + 
  geom_jitter(mapping = aes(x=cv , y= api_inf, colour = site))

ggsave(filename = "TF_cv_api_1d.png", plot = t1, path = paste0(here, "/output/figs/"),
       device = "png")
ggsave(filename = "TF_cv_api_10d.png", plot = t2, path = paste0(here, "/output/figs/"),
       device = "png")
ggsave(filename = "TF_cv_api_30d.png", plot = t3, path = paste0(here, "/output/figs/"),
       device = "png")
ggsave(filename = "TF_cv_api_inf.png", plot = t4, path = paste0(here, "/output/figs/"),
       device = "png")



s1 <-ggplot(all_events %>% filter(!str_detect(site, "TF"))) + 
  geom_jitter(mapping = aes(x=cv , y= api_1d, colour = site))
s2 <-ggplot(all_events %>% filter(!str_detect(site, "TF"))) + 
  geom_jitter(mapping = aes(x=cv , y= api_10d, colour = site))
s3 <-ggplot(all_events %>% filter(!str_detect(site, "TF"))) + 
  geom_jitter(mapping = aes(x=cv , y= api_30d, colour = site))
s4 <-ggplot(all_events %>% filter(!str_detect(site, "TF"))) + 
  geom_jitter(mapping = aes(x=cv , y= api_inf, colour = site))


ggsave(filename = "SF_cv_api_1d.png", plot = s1, path = paste0(here, "/output/figs/"),
       device = "png")
ggsave(filename = "SF_cv_api_10d.png", plot = s2, path = paste0(here, "/output/figs/"),
       device = "png")
ggsave(filename = "SF_cv_api_30d.png", plot = s3, path = paste0(here, "/output/figs/"),
       device = "png")
ggsave(filename = "SF_cv_api_inf.png", plot = s4, path = paste0(here, "/output/figs/"),
       device = "png")




a1 <- ggplot(all_events %>% filter(str_detect(site, "TF"))) + 
  geom_jitter(mapping = aes(x=cv , y= event_intensity, colour = site))
a1
a2 <- ggplot(all_events %>% filter(!str_detect(site, "TF"))) + 
  geom_jitter(mapping = aes(x=cv , y= event_intensity, colour = site))
a2

ggsave(filename = "cv_ei_TF.png", plot = a1, path = paste0(here, "/output/figs/"),
       device = "png")
ggsave(filename = "cv_ei_SF.png", plot = a2, path = paste0(here, "/output/figs/"),
       device = "png")



p1 <- ggplot(all_events %>% filter(str_detect(site, "TF"))) + 
  geom_jitter(mapping = aes(x=event_intensity, y= m, colour = site),
              size = 5) +
  theme_bw()
p1
p2 <- ggplot(all_events %>% filter(!str_detect(site, "TF"))) + 
  geom_jitter(mapping = aes(x=event_intensity, y= m, colour = site),
              size = 5) +
  theme_bw()

p2

ggsave(filename = "ei_m_TF.png", plot = p1, path = paste0(here, "/output/figs/"),
       device = "png")
ggsave(filename = "ei_m_SF.png", plot = p2, path = paste0(here, "/output/figs/"),
       device = "png")



# hobo_events_new3 <-hobo_events_new%>%
#   rename("Event"="hobo_event_n",)%>%
#   mutate(yld_mm_norm1 = yield_mm - min(yield_mm),
#          yld_mm_norm2 = yld_mm_norm1/max(yld_mm_norm1))%>%
#   distinct(yld_mm_norm2, .keep_all = TRUE)



# 
# all_events2<- inner_join(ppt_events_r5, hobo_events_new3,
#                          by = c( "Event"))%>%
#   select(Event,datetime_EST2,W9_Precipitation_mm, dt, site, yield_mm, recession_n)%>%
#   mutate(yld_mm_norm1 = yield_mm - min(yield_mm),
#          yld_mm_norm2 = yld_mm_norm1/max(yld_mm_norm1))%>%
#   distinct(yld_mm_norm2, .keep_all = TRUE)























# event8_TF_rec <- hobo_events_new3%>%
#   filter(str_detect(site, "TF"))%>%
#   filter(Event== 1)%>%
#   select(Rec_yld_norm,site, rec_dt, Event, Recession_yield)%>%
#   pivot_wider(names_from = "site", values_from = "Rec_yld_norm")%>%
#   pivot_longer(cols = c("TF-B","TF-D") , names_to = "Throughfall",
#                values_to = "TF_yield")%>%
#   distinct(TF_yield, .keep_all = TRUE)
# 
# 
# event8_TF_event <-hobo_events3 %>%
#   filter(str_detect(site, "TF"))%>%
#   filter(Event== 1)%>%
#   select(Ev_yld_norm,site, Event_dt, Event, Event_yield)%>%
#   pivot_wider(names_from = "site", values_from = "Ev_yld_norm")%>%
#   pivot_longer(cols = c("TF-B","TF-D") , names_to = "Throughfall",
#                values_to = "TF_yield")%>%
#   distinct(TF_yield, .keep_all = TRUE)
#   
# 
# 
# 
# 
# 
# event8_TF <- inner_join(event8_TF_rec , event8_TF_event,
#                       by = c(  "Event"))%>%
#   filter(Event_dt == rec_dt)
# 
# 
# 
# ev8_ppt <-ggplot(ppt_events_r5%>%filter(Event == 1), mapping = aes(x= datetime_EST2, y= W9_Precipitation_mm))+ 
#   # geom_point()
#   geom_bar(stat='identity')
# 
# 
# ev8_hobo_SF <-ggplot(event8_SF, mapping = aes(x= rec_dt))+
#   # geom_point(aes(y= Event_yield,color= stemflow.y))+
#   geom_point(aes(y= Event_yield, color = 'red'))+
#   geom_point(aes(y= Recession_yield, colour = 'blue'))
#   
#                                          
# ev8_hobo_SF <-ggplot(event8, mapping = aes(x= dt, y= SF_yield, color=stemflow))+ 
#   geom_point()
# 
# p1 <- ev8_ppt+ ev8_hobo_FP + ev8_hobo_SF + plot_layout(ncol=1) 
# 
# p1
# 
# 
# 
# ggsave(filename = "ppt_time.png", plot = p1, path = paste0(here, "/output/figs/"),
#        device = "png")
# 
# 
# 





 
# #Joining the cumulative hobo events and log linear curve data of APIs
# all_events<-inner_join(hobo_events_new2, nested_hobo_events,
#                     by = c( "recession_n", "log_yield", "time","dt","site","hobo_event_n","yield_mm"))


#Visualizing cumulative hobo yield per event and slope in log linear models
# ggplot(all_events) +
#   geom_point(mapping = aes(x=dt, y= log(yield_mm), colour = recession_n)) +
#   geom_point(mapping = aes(x=dt, y=m), color = "green") +
#   geom_point(mapping = aes(x=dt, y=r), color = "blue")+
#   geom_point(mapping = aes(x=dt, y=i), color = "red")+
#   facet_wrap(~ hobo_event_n, scales = "free")
# 
# #Visualizing cumulative hobo yield per event and r in log linear models
# ggplot(all_events %>% filter(str_detect(site, "TF"))) +
#   geom_point(mapping = aes(x=dt, y=yield_mm, colour = site)) +
#   # geom_point(mapping = aes(x=dt, y=m), color = "green") +
#   # geom_point(mapping = aes(x=dt, y=r), color = "blue")+
#   # geom_point(mapping = aes(x=dt, y=i), color = "red")+
#   facet_wrap(~ hobo_event_n, scales = "free")




# tree <-all_events%>%
#   pivot_wider(names_from = "site", values_from = "log_yield")%>%
#   pivot_longer(cols = c("SF-A","SF-C") , names_to = "Sugar_Maple", 
#                values_to = "SM_yield")%>%
#   pivot_longer(cols = c("SF-B","SF-D") , names_to = "Yellow_Birch", 
#                values_to = "YB_yield")%>%
#   
#   pivot_longer(cols = c("SM_yield","YB_yield") , names_to = "Stemflow", 
#                values_to = "stemflow_yield")%>%
#   pivot_longer(cols = c("TF-B","TF-D") , names_to = "Throughfall", 
#                values_to = "throughfall_yield")%>%
#   pivot_longer(cols = c("stemflow_yield","throughfall_yield") , names_to = "Flowpath", 
#                values_to = "flowpath_yield")
# 
# 
# ggplot(tree) +
#   geom_boxplot(mapping=aes(x=dt, y= flowpath_yield, group = Sugar_Maple)) +
#   geom_jitter(mapping=aes(x=dt, y= flowpath_yield, group = Sugar_Maple, colour= Sugar_Maple))+
#   # geom_point(mapping = aes(x=dt, y= yield_mm, colour = Stemflow)) +
#   geom_point(mapping = aes(x=dt, y=m), color = "green") +
#   facet_wrap(~ hobo_event_n, scales = "free")
#   
  
  