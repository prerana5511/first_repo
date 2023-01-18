#Setup (I try to load all packages at the top)
library(tidyverse) #this is a set of several packages including 'readr'
library(here)




#importing excel file to R
# library(readr)
# W9_Throughfall_Stemflow_Precipitation <- read_csv("Data/W9_Throughfall_Stemflow_Precipitation.csv", show_col_types = FALSE)
# View(W9_Throughfall_Stemflow_Precipitation)

#here package example
# library(here)
here <- here() #create a filepath object named "here" to use later
# here::i_am("README.md")
# here()
# here("Internship", "R", "first_repo","Data", "W9_Streamflow_Precipitation.csv")
# here("Internship", "R", "first_repo","Data", "W9_Throughfall_Stemflow_Precipitation.csv")

# I try to keep object names informative and short
# we often use "ppt" for precipitation
ppt <- read_csv(paste0(here, "/data/W9_Throughfall_Stemflow_Precipitation.csv"))
#you can specify the package that the function belongs to (see below)
#this is a good idea to reminder yourself when you're first learning
ppt <- readr::read_csv(paste0(here, "/data/W9_Throughfall_Stemflow_Precipitation.csv"))

glimpse(ppt)
is.interval(ppt$datetime_interval_EST) #this column is not classed as an 'interval'
# It must be classed as an interval before lubridate can use it to filter.

#set working directory
# setwd("D:/Internship/R/first_repo/Data")

#finding working directory
getwd() #this will now be the same as 'here'

#filter
library(lubridate)
events <- W9_Streamflow_Precipitation %>% 
              filter(W9_Precipitation_mm != "NA")
View(events)
