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


hobo_events <- readRDS(paste0(here, "/output/hobo_events.Rds"))


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
         event_dur_sec = dseconds(datetime_interval_EST))
tz(curve_intervals2$Start_dt_EST)
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

#Check and correct timezone
tz(hobo_events_new$dt)


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


daily_ppt <- ppt %>%
  mutate(date = as.Date(datetime_EST2))%>%
  group_by(date)%>%
  nest()%>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x)))%>%
  mutate(data = map(data, ~summarise(.x, across(where(is.numeric), sum))))%>%
  unnest_wider(data) %>% 
  ungroup()%>%
  mutate(date = as.POSIXct(date))

sapply(daily_ppt, class)

daily_ppt_api <- daily_ppt %>% 
  mutate(api_1d = getApi(W9_Precipitation_mm, k = 0.9, n=1, finite = TRUE),
         api_10d = getApi(W9_Precipitation_mm, k = 0.9, n = 10, finite = TRUE),
         api_30d = getApi(W9_Precipitation_mm, k = 0.9, n = 30, finite = TRUE),
         api_inf = getApi(W9_Precipitation_mm, k = 0.9, finite = FALSE))


API_events <- slice(daily_ppt_api, 0) 



for (i in 1:length(curve_intervals2$event_n)) {
  interval <- daily_ppt_api %>%
    filter(date%within% curve_intervals2$datetime_interval_EST[i]) %>% 
    mutate(event_n = curve_intervals2$event_n[i])%>%
    mutate(recession_n = curve_intervals2$recession_n[i])
  
  
  
  API_events  <- bind_rows(API_events , interval)
}





# ppt_events_r5 <- slice(ppt, 0) 
# 
# 
# 
# for (i in 1:length(curve_intervals2$event_n)) {
#   interval <- ppt %>%
#     filter(datetime_EST2 %within% curve_intervals2$datetime_interval_EST[i]) %>% 
#     mutate(event_n = curve_intervals2$event_n[i])%>%
#     mutate(recession_n = curve_intervals2$recession_n[i])
#   
#   
#   
#   ppt_events_r5 <- bind_rows(ppt_events_r5, interval)
# }

