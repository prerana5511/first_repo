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
api_table <- readr::read_csv(paste0(here, "/output/table1.csv"))%>%
  rename("hobo_event_n" = "Event")


#Import recession event statistics
rec_table <- readr::read_csv(paste0(here, "/output/table2.csv"))%>%
  mutate(centroid = as.POSIXct(centroid, format = "%m/%d/%Y %H:%M"))%>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::with_tz(., tzone='EST')))


#Import table of hobo events model coefficients
event_table <- readr::read_csv(paste0(here, "/output/event_model.csv"))%>%
  mutate(dt = as.POSIXct(dt, format = "%m/%d/%Y %H:%M"))%>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::with_tz(., tzone='EST')))


#Merging API and hobo events
merged_event_table <- full_join(api_table, event_table,
                                 by = c( "hobo_event_n"))%>%
  select(hobo_event_n, api_30d, event_intensity, m)



#Merging API and rec events
merged_rec_table <- full_join(api_table, rec_table,
                                 by = c( "hobo_event_n"))%>%
  mutate(site = recession_n)%>%
  mutate(site = case_when(str_detect(site, "SFA") ~ "SM",
                          str_detect(site, "SFB") ~ "YB",
                          str_detect(site, "SFC") ~ "SM",
                          str_detect(site, "SFD") ~ "YB",
                          str_detect(site, "TFB") ~ "TF",
                          str_detect(site, "TFD") ~ "TF"))%>%
  select(hobo_event_n, api_30d, event_intensity, m, Delta_T_duration, recession_n, site)




p1 = ggplot(merged_event_table) +
  geom_point(mapping = aes(x=api_30d, y=m,colour = "api"))

p2 = ggplot(merged_event_table) +
  geom_point(mapping = aes(event_intensity, y=m,colour = "event_intensity"))




p3 = ggplot(merged_rec_table) +
  geom_point(mapping = aes(Delta_T_duration, y=m,colour = site))+
  geom_boxplot(mapping = aes(x=site, y=m))+
  facet_wrap(~ site)

p1 /p2

(p1 + p2) / p3
