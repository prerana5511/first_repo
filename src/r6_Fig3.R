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
         power_yield = 10^yld_mm_norm2)


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




newdat <- data.frame(sec_norm=nested_rec_events$sec_norm[1:81])

model_rec_events <- rec_norm %>%
  dplyr::group_by(recession_n)%>%
  nest() %>%
  mutate(m1 = purrr::map(.x = data, .f = ~ lm(power_yield~ sec_norm, data = .))) %>%
  mutate(Pred = purrr::map2(.x = m1, .y = data, ~ predict.lm(object =.x))) %>% #predict on new data
  select(recession_n, Pred) %>%
  unnest(cols = c(Pred))



#Rate for SF-A

pred_rec_events <- read_csv(paste0(here, "/data/pred.csv"))

norm_rec_events <-inner_join(nested_rec_events, pred_rec_events,
                             by = c("recession_n"))%>%
  select(recession_n, sec_norm, Pred, site , dt, hobo_event_n)


ggplot(norm_rec_events) +
  geom_point(mapping = aes(x=sec_norm, y=Pred))

