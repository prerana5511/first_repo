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

rec_table_1 <- readr::read_csv(paste0(here, "/output/table1.csv"))%>%
  rename("hobo_event_n" = "Event")

rec_table_2 <- readr::read_csv(paste0(here, "/output/table2.csv"))



event_table <- readr::read_csv(paste0(here, "/data/event_model.csv"))

merged_event_table <- full_join(rec_table_1, event_table,
                                 by = c( "hobo_event_n"))%>%
  select(hobo_event_n, api_30d, event_intensity, m)



merged_rec_table <- full_join(rec_table_1, rec_table_2,
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
