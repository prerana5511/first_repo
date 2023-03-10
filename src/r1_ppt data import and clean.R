#0.0 Setup ----
library(tidyverse) #this is a set of several packages including 'readr'
library(here)
here <- here() #create a filepath object named "here" to use later
library(lubridate) #lubridate needs to be loaded separately
library(fs)



#1.0 Load and Clean Data ----
# adding 4 dashes "----" creates a collapsible code chunk
# specify packages using "::" in bewteen package name and function
events <- readr::read_csv(paste0(here, "/data/W9_Throughfall_Stemflow_Precipitation.csv"))
ppt <- readr::read_csv(paste0(here, "/data/W9_Streamflow_Precipitation.csv"))

#use dplyr::glimpse to browse data
glimpse(events)
glimpse(ppt)

#identifying class
sapply(events, class)
sapply(ppt, class)

#Check the format of the interval column
#"base" refers to the base R packages that come with R
base::class(events$datetime_interval_EST) #loaded as a character string
lubridate::is.interval(events$datetime_interval_EST) 
#this column is not classed as an 'interval'
#It must be classed as an interval before lubridate can use it to filter.


##1.1 Define the interval using start and end times----
events2 <- events %>%
  mutate(.after = datetime_end_GMT, #indicates where the new column is placed
         datetime_interval_EST2 = lubridate::interval(start = datetime_start_GMT,
                                                      end = datetime_end_GMT,
                                                      tz = "EST"))

class(events2$datetime_interval_EST2)
tz(events2$datetime_interval_EST2) #timezone of interval gives an error
tz(events2$datetime_start_GMT) #timezone of start is UTC/GMT
view(events2) #check the new intervals match the character intervals

#To check the interval is in EST, we can pull out the start
start <- int_start(events2$datetime_interval_EST2[1])
class(start)
tz(start)

#Now you should have an set of intervals to use for filtering
intervals <- events2 %>% 
  distinct(datetime_interval_EST2, .keep_all = TRUE) %>% 
  select(Event_Number, datetime_interval_EST2) %>% 
  mutate(event_dur_sec = dseconds(datetime_interval_EST2))


##1.2 Assign event numbers/filter ppt time series based on event intervals ----
ppt2 <- ppt %>% 
  mutate(.after = datetime_EST, #indicates where the new column is placed
         datetime_EST2 = as.POSIXct(datetime_EST, format = "%m/%d/%Y %H:%M")) %>% 
  filter(minute(datetime_EST2) == 0) %>% #remove subhourly timestamps
  select(-W9_Streamflow_mm_hr) %>% 
  arrange(datetime_EST2) #Order from earliest to latest timestamp

#create an empty dataframe with same headers
ppt_events <- slice(ppt2, 0) 

i=1
rm(i)

for (i in 1:length(intervals$Event_Number)) {
  interval <- ppt2 %>%
    filter(datetime_EST2 %within% intervals$datetime_interval_EST2[i]) %>% 
    mutate(Event_Number = intervals$Event_Number[i])
  
  ppt_events <- bind_rows(ppt_events, interval)
}

#Check and correct timezone
tz(ppt_events$datetime_EST2)
attr(ppt_events$datetime_EST2, "tzone") <- "EST"
tz(ppt_events$datetime_EST2)
#Now the hourly time series record is filtered with event numbers
  

#2.0 ---- Create summary statistics for each event
#use tidy R piping and dplyr::group_by and summarize functions

options(dplyr.summarise.inform = TRUE) #I like to see the feedback

#I try to avoid overwriting objects with the same name
#It helps with troubleshooting and you can always remove old objects
events_summary <- ppt_events %>% 
  group_by(Event_Number) %>%
  dplyr::summarise(P_event_mm = sum(W9_Precipitation_mm),
                 P_max_event_mm_hr = max(W9_Precipitation_mm, na.rm = TRUE),
                 P_mean_event_mm_hr = mean(W9_Precipitation_mm, na.rm = TRUE))%>% 
  left_join(intervals, ., by = "Event_Number")




ppt_dt <-  ppt_events %>%
  left_join(events_summary, .,  by = "Event_Number") %>%
  group_by(Event_Number) %>% 
  # filter(W9_Precipitation_mm != 0 )%>%
  mutate(dt_P_mm_hr_event_max = case_when(P_max_event_mm_hr == W9_Precipitation_mm ~ 
                                            datetime_EST2),
         fract_max_P_event = case_when(P_max_event_mm_hr == W9_Precipitation_mm ~ 
                                         W9_Precipitation_mm/P_event_mm),
         seconds_to_max_P = dseconds(interval(start = int_start(datetime_interval_EST2),
                                              end = dt_P_mm_hr_event_max))) %>%
  distinct(Event_Number, dt_P_mm_hr_event_max, fract_max_P_event,
           seconds_to_max_P) %>% 
  drop_na() %>% 
  dplyr::ungroup() %>% 
  left_join(events_summary, ., by = "Event_Number")



#save results
#use write_csv and saveRDS and 'here' to save the events_summary and ppt_events data
#write_csv(x, path, na = "NA", append = FALSE,
#         col_names = !append)


file_create(paste0(here, "/output/W9_Ppt_eventsummary_file.Rds"))

saveRDS(events_summary,
        paste0(here,  "/output/W9_Ppt_eventsummary_file.Rds"))

write_csv(x = events_summary, here,"/output/W9_Ppt_eventsummary_file.csv")


rm(events,events_summary,events2,interval,intervals,ppt,ppt_dt,ppt_events,ppt2)

#clean up object list
# to_remove <- ls() %>% as_tibble()
# 
# rm(ls())




#library(ggplot2)

#p1 <- ggplot(data =  ppt_change)
#p2 <- p1 + geom_line(aes(x = datetime_EST2,
#                         y = P_rate,
#                         color = "blue",
#                         group = 1 ))


#selecting precipitation that are available and greater than zero
# vec <- c(0, NA)
# vec
# ppt <- ppt[! ppt$W9_Precipitation_mm %in% vec,]
# View(ppt)
# 
# 
# #adding duration in hours
# ppt3 <- ppt %>%
#   mutate(Time <- hour(ppt2$datetime_EST) + minute(ppt2$datetime_EST)/60 + second(ppt2$datetime_EST)/3600) 
  



#filtering intervals

# ppt_intervals <- slice(ppt, 0)
# 
#  i=1
#  rm(i)
# 
#  for (i in 1:length(intervals)) {
#  final <- ppt %>%
#      filter(datetime_EST %within% intervals[i]) %>%
#      mutate(Event_number = events2$Event_Number[i])
#      
#   ppt_intervals <- bind_rows(ppt_intervals, final)
#  }
#  
#  tz(ppt_intervals$datetime_EST)
 
 
 
 
 




OlsonNames() #valid timezone names 
# day <- today(tz= "Asia/Kolkata") 
# str(day)
# datetime <- now(tz= "Asia/Kolkata")
# str(datetime)
# dateFormat1 <- "20220922" #systemic format
# ymd(dateFormat1)






#set working directory
# setwd("D:/Internship/R/first_repo/Data")





#finding working directory
getwd() #this will now be the same as 'here'


#filter
#events <- W9_Streamflow_Precipitation %>% 
#              filter(W9_Precipitation_mm != "NA")
#View(events)


max_rate = max(Rate), min_rate = min(Rate)


final


filter(W9_Precipitation_mm[i] == datetime_EST[i]) %>%
  filter(W9_Precipitation_mm != "NA")
group_by(W9_Precipitation_mm) %>%
  
  mutate(Rate = W9_Precipitation_mm/Time) %>%
  
  mutate(sum_Time[i] <- sum(Time[i])) %>%
  summarise( Event_dur = seconds_to_period(sum_Time[i]))



ppt2<- ppt%>% 
  select(datetime_EST,W9_Precipitation_mm)
  