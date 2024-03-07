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


hobo_rec_events <- read_csv(paste0(here, "/data/rec_values.csv"))


#norm
rec_norm <- hobo_rec_events  %>%
  mutate(sec = seconds(dt) - min(seconds(dt)),
         sec_norm = sec/max(sec),
         yld_mm_norm1 = yield_mm - min(yield_mm),
         yld_mm_norm2 = yld_mm_norm1/max(yld_mm_norm1),
         power_yield = 10^yld_mm_norm2)%>%
  pivot_wider(names_from = "site", values_from = "power_yield")%>%
  pivot_longer(cols = c("TF-B","TF-D") , names_to = "Throughfall",
               values_to = "TF_yield")%>%
  pivot_longer(cols = c("SF-A","SF-C") , names_to = "SM",
               values_to = "SM_yield")%>%
  pivot_longer(cols = c("SF-B","SF-D") , names_to = "YB",
               values_to = "YB_yield")%>%
  pivot_longer(cols = c("TF_yield","SM_yield","YB_yield") , names_to = "site",
               values_to = "power_yield")%>%
  select(-Throughfall, -SM, -YB)%>%
  distinct(across(power_yield), .keep_all = TRUE)%>%
  drop_na()

  



#linear models
nested_rec_events <- rec_norm %>%
  drop_na()%>%
  group_by(recession_n) %>%
  nest() %>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x))) %>%
  mutate(mod = map(data, ~lm(power_yield ~ sec_norm, data= .)),
         r = map_dbl(.x = data, .f = ~cor(y=(.x$power_yield), x = .x$sec_norm,
                                          use = "na.or.complete")),
         m = map_dbl(data, ~lm(power_yield~ sec_norm, data = .)$coefficients[[2]]),
         i = map_dbl(data, ~lm(power_yield ~ sec_norm, data = .)$coefficients[[1]]),
         r2 = r^2) %>% 
  
  unnest(data) %>%
  ungroup() %>%
  distinct(across(recession_n), .keep_all = TRUE)




# newdat <- data.frame(sec_norm=nested_rec_events$sec_norm[1:81])

model_rec_events <- rec_norm %>%
  dplyr::group_by(recession_n)%>%
  nest() %>%
  mutate(m1 = purrr::map(.x = data, .f = ~ lm(power_yield~ sec_norm, data = .))) %>%
  mutate(Pred = purrr::map2(.x = m1, .y = data, ~ predict.lm(object =.x))) %>% #predict on new data
  select(recession_n, Pred) %>%
  unnest(cols = c(Pred))



#Rate for SF-A

# pred_rec_events <- read_csv(paste0(here, "/data/pred.csv"))

norm_rec_events <-inner_join(nested_rec_events, model_rec_events,
                             by = c("recession_n"))%>%
  select(recession_n, sec_norm, Pred, site , dt, hobo_event_n)
  # pivot_wider(names_from = "site", values_from = "Pred")%>%
  # pivot_longer(cols = c("TF-B","TF-D") , names_to = "Throughfall",
  #                values_to = "TF_yield")%>%
  # pivot_longer(cols = c("SF-A","SF-C") , names_to = "SM",
  #              values_to = "SM_yield")%>%
  # pivot_longer(cols = c("SF-B","SF-D") , names_to = "YB",
  #              values_to = "YB_yield")%>%
  # pivot_longer(cols = c("TF_yield","SM_yield","YB_yield") , names_to = "site",
  #              values_to = "Pred")


ggplot(norm_rec_events) +
  geom_point(mapping = aes(x=sec_norm, y=Pred,colour = site))+
  facet_wrap(~ hobo_event_n, scales = "free")

