#Setup (I try to load all packages at the top)
library(tidyverse) #this is a set of several packages including 'readr'
library(here)





#here package example

here <- here() #create a filepath object named "here" to use later


# I try to keep object names informative and short
# we often use "ppt" for precipitation

ppt <- read_csv(paste0(here, "/data/W9_Throughfall_Stemflow_Precipitation.csv"))
ppt1 <- read_csv(paste0(here, "/data/W9_Streamflow_Precipitation.csv"))



#you can specify the package that the function belongs to (see below)
#this is a good idea to reminder yourself when you're first learning

ppt <- readr::read_csv(paste0(here, "/data/W9_Throughfall_Stemflow_Precipitation.csv"))
ppt1 <- readr::read_csv(paste0(here, "/data/W9_Streamflow_Precipitation.csv"))



glimpse(ppt)
glimpse(ppt1)

is.interval(ppt$datetime_interval_EST) #this column is not classed as an 'interval'
# It must be classed as an interval before lubridate can use it to filter.


#identifying class
sapply(ppt, class)

#converting to numeric class
ppt$datetime_interval_EST <- as.numeric(ppt$datetime_interval_EST)


#converting numeric to interval by the instruction given in 'Help'
ppt <- ppt %>%
  mutate(datetime_interval_EST <- as.interval(3600, ymd("2018-05-31")))

#Gives error message : could not find function "as.interval"




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
