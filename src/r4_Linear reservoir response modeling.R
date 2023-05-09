library(here)
here <- here()
here
library(tidyverse)
library(lubridate)
library(dygraphs)
library(xts)


hobo_events <- readRDS(paste0(here, "/output/hobo_events.Rds"))

hobo_events2 <- hobo_events%>%
  pivot_longer(cols = contains("_mm"), names_to = "site", 
               values_to = "yield_mm")%>%
  group_by(site, dt) %>% 
  arrange(dt)%>% 
  ungroup()%>% 
  select(dt, site,  yield_mm, hobo_event_n)%>%
  drop_na()


#Calculate event statistics
hobo_max <- hobo_events2 %>% 
  group_by(site, hobo_event_n) %>% 
  arrange(dt)%>%
  dplyr::summarise(max_yld_mm = max(yield_mm, na.rm=T))



#Join the max rate with the time series to access the time of max for each event
hobo_events3 <- inner_join(hobo_max, hobo_events2,
                          by = c("site", "hobo_event_n")) %>% 
  rownames_to_column()
  

#manually keep the max yield rows and remove the duplicates 
hobo_events4 <- hobo_events3 %>%   
  mutate(to_keep1 = case_when(yield_mm < max_yld_mm ~ FALSE,
                             TRUE ~ TRUE )) %>%
  #mutate(to_keep2 = case_when (rowname %in% c(51,55,220,234,267,
   #                                           1848,1859,1907,2746,
    #                                         2990,4203,4207,4210,
     #                                         5408,5574,5672,5674,
      #                                        5816,5904,7423,7679,
       #                                       7981,8536,8537,8691,
        #                                      9265,9685,9700,9715,
         #                                     9716,9721,11025,11212,
          #                                    11213,11417,11418,11419,
           #                                   12879,12909,12912,13145,
            #                                  13182,13774,13775,13784,
             #                                 13827,14155,15234,15235,15892,
              #                                15901,16518,16531,16533,16536) ~ FALSE, 
               #                TRUE ~ TRUE))%>%
    filter(to_keep1) %>% select(-c( to_keep1,rowname)) %>% 
  #filter(to_keep2) %>% select(-c( to_keep2,rowname)) %>% 
  rename("dt_max_yield_mm" = "dt")



hobo_events5 <- hobo_events2 %>% 
  left_join(., hobo_events4,
            by = c("site", "hobo_event_n")) %>% 
  group_by(site, hobo_event_n) %>% 
  mutate(limb = case_when(dt < dt_max_yield_mm ~ "rising",
                          dt > dt_max_yield_mm ~ "falling",
                          dt== dt_max_yield_mm ~ "peak"))



hobo_events6 <- hobo_events5%>%
  filter(limb == "falling") %>%
  group_by(hobo_event_n,site)%>%
  mutate(time =  seconds((dt - dt_max_yield_mm)*60))


hobo_events7 <- hobo_events6 %>% 
  mutate(yield_mm_clean = case_when(yield_mm.x < 0 | 
                                      is.na(yield_mm.x) ~ 0,
                                    TRUE ~ yield_mm.x)) %>% 
  ungroup() %>% 
  select(dt, site, hobo_event_n, yield_mm_clean, dt_max_yield_mm,max_yld_mm, time, limb) %>% 
  rename("yield_mm" = "yield_mm_clean")
        

mod_lin <- hobo_events7 %>%
  mutate(t = time_length(time, unit = "seconds")) %>% 
  mutate(c = log(max_yld_mm) - log(yield_mm)) %>%
  mutate(k = c/t) 

#Warning : NaNs produced





# mutate( to_keep <- case_when( a == b) ~ FALSE,
#        TRUE ~ TRUE ) %>%
#filter(to_keep) %>% select(-c( to_keep))%>%