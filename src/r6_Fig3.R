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


hobo_rec_events <- read_csv(paste0(here, "/data/rec_values.csv"))%>%
  mutate(across(.cols = lubridate::is.POSIXct,
                ~ lubridate::with_tz(., tzone='EST')))


#norm
rec_norm <- hobo_rec_events%>%
  group_by(recession_n)%>%
  mutate(sec = seconds(dt) - min(seconds(dt)),
         sec_norm = sec/max(sec),
         yld_mm_norm1 = yield_mm - min(yield_mm),
         yld_mm_norm2 = yld_mm_norm1/max(yld_mm_norm1),
         power_yield = 10^yld_mm_norm2)%>%
  mutate(site = recession_n)%>%
  mutate(site = case_when(str_detect(site, "SFA") ~ "SM",
                          str_detect(site, "SFB") ~ "YB",
                          str_detect(site, "SFC") ~ "SM",
                          str_detect(site, "SFD") ~ "YB",
                          str_detect(site, "TFB") ~ "TF",
                          str_detect(site, "TFD") ~ "TF"))%>%
  # select(-TF, -SM, -YB)%>%
  ungroup()%>%
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
  # mutate(results = sec_norm * m+ i)%>%
  ungroup() %>%
  distinct(across(recession_n), .keep_all = TRUE)


write_csv(nested_rec_events,
          paste0(here,"/data/rec_model.csv"))



#Pred
model_rec_events <- rec_norm %>%
  dplyr::group_by(recession_n)%>%
  nest() %>%
  mutate(m1 = purrr::map(.x = data, .f = ~ lm(power_yield~ sec_norm, data = .)),
         glance = map(.x = m1, .f = ~broom::glance(.x)),
         preds = map(m1, broom::augment),
         r = map_dbl(.x = data, .f = ~cor(.x$power_yield, .x$sec_norm, use="complete.obs")),
         RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>%
  # unnest(preds)%>%
  unnest(preds)%>%
  ungroup()
  # mutate(Pred = purrr::map2(.x = m1, .y = data, ~ predict.lm(object =.x))) %>% #predict on new data
  # select(recession_n, Pred) %>%
  # unnest(cols = c(Pred))





rate_convertion <- model_rec_events%>%
  # select(-yield_mm, yld_mm_norm1, -yld_mm_norm2, -sec)%>%
  mutate(hobo_event_n = recession_n)%>%
  mutate(hobo_event_n = case_when(str_detect(hobo_event_n, "E1") ~ "1",
                          str_detect(hobo_event_n, "E2") ~ "2",
                          str_detect(hobo_event_n, "E3") ~ "3",
                          str_detect(hobo_event_n, "E4") ~ "4",
                          str_detect(hobo_event_n, "E5") ~ "5",
                          str_detect(hobo_event_n, "E6") ~ "6",
                          str_detect(hobo_event_n, "E7") ~ "7",
                          str_detect(hobo_event_n, "E8") ~ "8",
                          str_detect(hobo_event_n, "E9") ~ "9"))%>%
  mutate(site = recession_n)%>%
  mutate(site = case_when(str_detect(site, "SFA") ~ "SM",
                          str_detect(site, "SFB") ~ "YB",
                          str_detect(site, "SFC") ~ "SM",
                          str_detect(site, "SFD") ~ "YB",
                          str_detect(site, "TFB") ~ "TF",
                          str_detect(site, "TFD") ~ "TF"))%>%
  mutate(rate_yld = .fitted - lag(.fitted))



ggplot(rate_convertion) +
  geom_point(mapping = aes(x=sec_norm, y=rate_yld,colour = site))+
  facet_wrap(~ hobo_event_n, scales = "free")

