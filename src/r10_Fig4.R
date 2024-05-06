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
library(broom)


#Import table containing API values
api_table <- readRDS(paste0(here, "/output/table1.Rds"))%>%
  rename("hobo_event_n" = "Event")


#Import recession event statistics
rec_table <- readRDS(paste0(here, "/output/table2.Rds"))


#Import table of hobo events model coefficients
event_table <- readRDS(paste0(here, "/output/event_model.Rds"))



#Merging API and hobo events
merged_event_table <- full_join(api_table, event_table,
                                 by = c( "hobo_event_n"))%>%
  select(hobo_event_n, api_30d, event_intensity, m)



#Merging API and rec events
merged_rec_table <- full_join(api_table, rec_table,
                                 by = c( "hobo_event_n"))%>%
  select(hobo_event_n, api_30d, event_intensity, m, Delta_T_duration, recession_n, site, site2)



#Plotting API vs slope
p1 = ggplot(merged_event_table) +
  geom_point(mapping = aes(x=api_30d, y=m,colour = "api"))


#Plotting event intensity vs slope
p2 = ggplot(merged_event_table) +
  geom_point(mapping = aes(event_intensity, y=m,colour = "event_intensity"))



#Plotting DeltaT vs slope
p3 = ggplot(merged_rec_table) +
  geom_point(mapping = aes(Delta_T_duration, y=m,colour = site2))+
  geom_boxplot(mapping = aes(x=Delta_T_duration, y=m))+
  facet_wrap(~ site2)



p1 /p2

(p1 + p2) / p3
