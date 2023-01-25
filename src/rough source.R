#0.0 Setup
library(tidyverse) #this is a set of several packages including 'readr'
library(here)
here <- here() #create a filepath object named "here" to use later
library(lubridate) #lubridate needs to be loaded separately


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
#Check the format of the interval column
#"base" refers to the base R packages that come with R
base::class(events$datetime_interval_EST) #loaded as a character string
lubridate::is.interval(events$datetime_interval_EST) 
#this column is not classed as an 'interval'
#It must be classed as an interval before lubridate can use it to filter.

#converting to numeric class
# events$datetime_interval_EST <- as.numeric(events$datetime_interval_EST)
# class(events$datetime_interval_EST)
# converting a character string to a number creates "NAs"

# Define the interval using start and end times
events2 <- events %>%
  mutate(.after = datetime_end_GMT, #indicates where the new column is placed
         datetime_interval_EST2 = lubridate::interval(start = datetime_start_GMT,
                                                      end = datetime_end_GMT,
                                                      tz = "EST"))
class(events2$datetime_interval_EST2)
tz(events2$datetime_interval_EST2) #timezone of interval gives an error
tz(events2$datetime_start_GMT) #timezone of start is UTC/GMT
view(events2) #check the new intervals match the character intervals

#To ensure the interval is in EST, we can pull out the start
start <- int_start(events2$datetime_interval_EST2[1])
class(start)
tz(start)

#Now you should have an set of intervals to use for filtering
intervals <- unique(events2$datetime_interval_EST2)
intervals

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
