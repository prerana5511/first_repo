library(here)
here <- here()
here
library(tidyverse)
library(lubridate)
library(dygraphs)
library(xts)



hobo_events<- readRDS(paste0(here, "/output/hobo_events.Rds"))

hobo_events2 <- hobo_events%>%
  pivot_longer(cols = contains("_mm"), names_to = "site", 
               values_to = "yield_mm")%>%
  group_by(site, dt) %>% 
  arrange(dt)%>% 
  ungroup()%>% 
  select(dt, site,  yield_mm, hobo_event_n)%>%
  drop_na()



#normalization to remove negatives----

hobo_norm<- hobo_events2 %>%
  group_by(site)%>%
  mutate(yield_norm = yield_mm + abs(min(yield_mm)))%>%
  select(-yield_mm)%>%
  rename("yield_mm" = "yield_norm")

#Smoothing----

# sum(is.na(log(hobo_norm$yield_mm)))
# sum(is.infinite(log(hobo_norm$yield_mm)))

# hobo_norm_modified <- hobo_norm %>%
#   mutate(log_values = log(yield_mm))%>%
#   mutate(is.infinite(log_values))%>%
#   rownames_to_column()

# hobo_norm_modified[c('log_values')][sapply(hobo_norm_modified[c('log_values')], is.infinite)] <- NA 

# hobo_norm_filtered <- hobo_norm_modified%>%
#   mutate(to_keep = case_when (rowname %in% c(384, 9021, 12220) ~ FALSE,  TRUE ~ TRUE))%>%
#   filter(to_keep) %>% select(-c( to_keep,rowname)) 
  
# hobo_smooth <- hobo_event2 %>%
#   group_by(site, hobo_event_n) %>%
#   mutate(model <- model = loess(yield_mm~dt, span = 0.1)$fitted)

hobo_norm_wide <- hobo_norm%>%
  pivot_wider(names_from = "site", values_from = "yield_mm")

ggplot(hobo_norm_wide, aes(x= dt)) +
  geom_smooth(aes(y= SFA_mm, col= paste0('SFA_mm')), se = FALSE, method = "loess", span = 0.1) +
  geom_smooth(aes(y= SFB_mm, col= paste0('SFB_mm')), se = FALSE, method = "loess",span = 0.1)+
  geom_smooth(aes(y= SFC_mm, col= paste0('SFC_mm')), se = FALSE, method = "loess",span = 0.1)+
  geom_smooth(aes(y= SFB_mm, col= paste0('SFD_mm')), se = FALSE, method = "loess",span = 0.1)
 

gg1 <- ggplot(hobo_norm_wide, aes(x= dt)) +
  geom_smooth(aes(y= SFA_mm, col= paste0('SFA_mm')), se = FALSE, method = "loess", span = 0.1) 
gg2 <- ggplot(hobo_norm_wide, aes(x= dt))+ 
  geom_smooth(aes(y= SFB_mm, col= paste0('SFB_mm')), se = FALSE, method = "loess",span = 0.1)
gg3<- ggplot(hobo_norm_wide, aes(x= dt))+
  geom_smooth(aes(y= SFC_mm, col= paste0('SFC_mm')), se = FALSE, method = "loess",span = 0.1)
gg4 <-ggplot(hobo_norm_wide, aes(x= dt))+
  geom_smooth(aes(y= SFD_mm, col= paste0('SFD_mm')), se = FALSE, method = "loess",span = 0.1)



SFA_smooth  <-  ggplot_build(gg1)$data[[1]][,c("x","y")] %>%
  mutate(Date = as_datetime(x)) %>%
  rename("SFA_mm" = "y")%>%
  select(-x)
SFB_smooth  <-  ggplot_build(gg2)$data[[1]][,c("x","y")] %>%
  mutate(Date = as_datetime(x)) %>%
  rename("SFB_mm" = "y")%>%
  select(-x)
SFC_smooth  <-  ggplot_build(gg3)$data[[1]][,c("x","y")] %>%
  mutate(Date = as_datetime(x)) %>%
  rename("SFC_mm" = "y")%>%
  select(-x)
SFD_smooth  <-  ggplot_build(gg4)$data[[1]][,c("x","y")] %>%
  mutate(Date = as_datetime(x)) %>%
  rename("SFD_mm" = "y")%>%
  select(-x)

hobo_smooth <- bind_cols(SFA_smooth,SFB_smooth,SFC_smooth,SFD_smooth)%>%
  select(Date...2, SFA_mm, SFB_mm, SFC_mm, SFD_mm)%>%
  rename("dt" = "Date...2")

hobo_smooth_long <- hobo_smooth%>%
  pivot_longer(cols = contains("_mm"), names_to = "site", 
               values_to = "yield_mm")%>%
  group_by(site, dt) %>% 
  arrange(dt)%>% 
  ungroup()%>% 
  drop_na()


hobo_interval <- readxl::read_excel(paste0(here, "/data/hobo/hobo_events.xlsx"))

hobo_interval2 <- hobo_interval %>%
  mutate(dt = lubridate::interval(start = start_time_EST,
                                  end = end_time_EST,
                                  tz = "EST"))
class(hobo_interval2$dt)
tz(hobo_interval2$dt) #timezone of interval gives an error
tz(hobo_interval2$start_time_EST) #timezone of start is UTC/GMT

#To check the interval is in EST, we can pull out the start
start <- int_start(hobo_interval2$dt[1])
class(start)
tz(start)

hobo_join <- slice(hobo_norm, 0) 

for (i in 1:length(hobo_interval2$hobo_event_n)) {
  interval <- hobo_smooth_long %>%
    filter(dt %within% hobo_interval2$dt[i]) %>% 
    mutate(hobo_event_n = hobo_interval2$hobo_event_n[i])
  
  hobo_join <- bind_rows(hobo_join, interval)
}



#Calculating max

hobo_max <- hobo_norm %>% 
  group_by(site, hobo_event_n) %>% 
  arrange(dt)%>%
  dplyr::summarise(max_yld_mm = max(yield_mm, na.rm=T))



#Join the max rate with the time series to access the time of max for each event
hobo_events3 <- inner_join(hobo_max, hobo_norm,
                          by = c("site", "hobo_event_n")) %>% 
  rownames_to_column()
  

#manually keep the max yield rows 
hobo_events4 <- hobo_events3 %>%   
  mutate(to_keep1 = case_when(yield_mm < max_yld_mm ~ FALSE,
                             TRUE ~ TRUE )) %>%
  # mutate(to_keep2 = case_when (rowname %in% c(51,55,220,234,267,
  #                                             1848,1859,1907,2746,
  #                                            2990,4203,4207,4210,
  #                                             5408,5574,5672,5674,
  #                                             5816,5904,7423,7679,
  #                                             7981,8536,8537,8691,
  #                                             9265,9685,9700,9715,
  #                                             9716,9721,11025,11212,
  #                                             11213,11417,11418,11419,
  #                                             12879,12909,12912,13145,
  #                                             13182,13774,13775,13784,
  #                                             13827,14155,15234,15235,15892,
  #                                             15901,16518,16531,16533,16536) ~ FALSE, 
  #                              TRUE ~ TRUE))%>%
    filter(to_keep1) %>% select(-c( to_keep1,rowname)) %>% 
  #filter(to_keep2) %>% select(-c( to_keep2,rowname)) %>% 
  rename("dt_max_yield_mm" = "dt")



hobo_events5 <- hobo_norm %>% 
  left_join(., hobo_events4,
            by = c("site", "hobo_event_n")) %>% 
  group_by(site, hobo_event_n) %>% 
  mutate(limb = case_when(dt < dt_max_yield_mm ~ "rising",
                          dt > dt_max_yield_mm ~ "falling",
                          dt== dt_max_yield_mm ~ "peak"))




hobo_events6 <- hobo_events5%>%
  filter(limb == "falling") %>%
  group_by(hobo_event_n, site)%>%
  mutate(time =  seconds((dt - dt_max_yield_mm)*60),
         time2 = time_length(time, unit = "seconds"))%>%
  mutate(log_yield = log(yield_mm.x))



nested_data <- hobo_events6 %>%
  group_by(site, hobo_event_n) %>%
  nest() %>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x))) %>%
  mutate(r = map_dbl(.x = data, .f = ~cor(y=.x$log_yield, x = .x$time2,
                                      use = "na.or.complete")),
         m = map_dbl(data, ~lm(log_yield ~ time2, data = .)$coefficients[[2]]),
         i = map_dbl(data, ~lm(log_yield ~ time2, data = .)$coefficients[[1]]),
         r2 = r^2)%>%
  rename(hobo_data = data)
   
  
# nd <-nested_data%>%
#   unnest(hobo_data)%>%
#   select(time2,log_yield, site, hobo_event_n)%>%
#   group_by(site, hobo_event_n) %>%
#   ggplot() +
#   geom_line(mapping = aes(x=time2, y =log_yield)) +
#   facet_wrap(~ site)




nested_data_plot <- nested_data %>% 
  mutate(
    map(hobo_data, ~ ggplot(., aes(x = time2, y = log_yield)) + 
          geom_point() +
          geom_smooth(se = TRUE,colour = 'blue')
        
    )
  ) %>% 
  rename(plot_hobo = `map(...)`)


nested_data_plot$plot_hobo[33]


library(grid)
multiplot(nested_data_plot$plot_hobo[1:54], cols = 2)


# nested_data_facet <- hobo_events6%>%
#   group_by(site, hobo_event_n) %>%
#   nest() %>%
#   map(.x = hobo_data(small = 3, medium = 10, large  = 100),
#            .f = ~ sample(rnorm(1000), .x, replace = T)) %>%
#   tibble(sample = ., mean = map_dbl(., mean))
# 
# ## plot
# dtf %>%
#   mutate(group = names(sample)) %>%  # or: group = lengths(sample)
#   unnest(sample) %>%
#   ggplot(data = .) +
#   geom_histogram(mapping = aes(sample)) +
#   facet_wrap(~ group)



# ggplot(hobo_events5 %>% filter(!str_detect(site, "TF"))) +
#   geom_line(mapping=aes(x=dt, y= cumyield_mm, fill = site, color = limb))+
#   facet_wrap(~hobo_event_n, scales = "free")


# nested_data_facet <- nested_data_plot%>%
#   group_by(site,hobo_event_n)%>%
#   mutate(events = levels(as.factor(hobo_event_n)), p = vector("list", length(events)),
#          names(p) <- events)%>%
#   mutate(for(i in 1:length(events)){
#     p[[i]] = ggplot(mydf[mydf$id == events[i],], aes(x= ,y)) +
#       geom_tile() + ggtitle(paste(ids[i])) + 
#       facet_wrap(~day, ncol=1)
#   })
