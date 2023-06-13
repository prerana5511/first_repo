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
  

#2.0 Create summary statistics for each event ----
#use tidy R piping and dplyr::group_by and summarize functions

options(dplyr.summarise.inform = TRUE) #I like to see the feedback

#I try to avoid overwriting objects with the same name
#It helps with troubleshooting and you can always remove old objects
events_summary <- ppt_events %>% 
  group_by(Event_Number) %>%
  dplyr::summarise(P_sum_event_mm = sum(W9_Precipitation_mm),
                 P_max_event_mm_hr = max(W9_Precipitation_mm, na.rm = TRUE),
                 P_mean_event_mm_hr = mean(W9_Precipitation_mm, na.rm = TRUE))%>% 
  left_join(intervals, ., by = "Event_Number")%>%
  pivot_longer(cols = contains("_mm"), names_to = "Event", 
               values_to = "yield_mm")


ggplot(events_summary)+
  geom_line(aes(x=event_dur_sec, y = yield_mm, colour = Event))+
  labs(title="Event Summary Precipitation (r1)",
        x ="Event Duration(s)", y = "Precipitation (mm)")
  


ppt_dt <-  ppt_events %>%
  left_join(events_summary, .,  by = "Event_Number") %>%
  group_by(Event_Number) %>% 
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


##1.3 Save ----
saveRDS(events_summary,
        paste0(here,  "/output/W9_Ppt_eventsummary_file.Rds"))
write_csv(x = events_summary,
          paste0(here,"/output/W9_Ppt_eventsummary_file.csv"))


saveRDS(ppt_events,
        paste0(here,  "/output/W9_Ppt_pptevents_file.Rds"))





rm(events,events_summary,events2,interval,intervals,ppt,ppt_dt,ppt_events,ppt2)



##END ----
#clean up object list
# to_remove <- ls() %>% as_tibble()
# 
# rm(ls())




OlsonNames() #valid timezone names 
# day <- today(tz= "Asia/Kolkata") 
# str(day)
# datetime <- now(tz= "Asia/Kolkata")
# str(datetime)
# dateFormat1 <- "20220922" #systemic format
# ymd(dateFormat1)







  