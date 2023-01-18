#importing excel file to R
library(readr)
W9_Throughfall_Stemflow_Precipitation <- read_csv("Data/W9_Throughfall_Stemflow_Precipitation.csv", show_col_types = FALSE)
View(W9_Throughfall_Stemflow_Precipitation)

#here package
library(here)
here::i_am("README.md")
here()
here("Internship", "R", "first_repo","Data", "W9_Streamflow_Precipitation.csv")
here("Internship", "R", "first_repo","Data", "W9_Throughfall_Stemflow_Precipitation.csv")

#set working directory
setwd("D:/Internship/R/first_repo/Data")

#finding working directory
getwd() 

#filter
library(lubridate)
events <- W9_Streamflow_Precipitation %>% 
              filter(W9_Precipitation_mm != "NA")
View(events)
