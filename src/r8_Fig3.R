library(here)
here <- here()
here
library(tidyverse)
library(lubridate)
library(fs)
library(xts)
library(dygraphs)
library(readxl)
library(scales)
library(patchwork)

#HOBO EVENT CURVES

#Import hobo events
hobo_events <- readRDS(paste0(here, "/output/hobo_events.Rds"))

#Normalize hobo and process events for event summary in r10_fig4 script
event_norm <- hobo_events%>%
  group_by(hobo_event_n)%>%
  mutate(sec = seconds(dt) - min(seconds(dt)),
         sec_norm = sec/max(sec),
         yld_mm_norm1 = yield_mm - min(yield_mm),
         yld_mm_norm2 = yld_mm_norm1/max(yld_mm_norm1),
         power_yield = 10^yld_mm_norm2)%>%
  ungroup()%>%
  distinct(across(power_yield), .keep_all = TRUE)%>%
  drop_na()


#linear models for hobo events
nested_events <- event_norm %>%
  drop_na()%>%
  select(-site, -site2)%>%
  group_by(hobo_event_n) %>%
  nest() %>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x))) %>%
  mutate(mod = map(data, ~lm(power_yield ~ sec_norm, data= .)),
         r = map_dbl(.x = data, .f = ~cor(y=(.x$power_yield), x = .x$sec_norm,
                                          use = "na.or.complete")),
         m = map_dbl(data, ~lm(power_yield~ sec_norm, data = .)$coefficients[[2]]),
         i = map_dbl(data, ~lm(power_yield ~ sec_norm, data = .)$coefficients[[1]]),
         r2 = r^2) %>% 
  unnest(data) %>%
  ungroup()%>%
  distinct(across(hobo_event_n), .keep_all = TRUE)

#Save event models for r10 script
saveRDS(nested_events, paste0(here, "/output/event_model.Rds"))
write_csv(nested_events,
          paste0(here,"/output/event_model.csv"))



#RECESSION CURVES

#Import recession curve values
hobo_rec_events <- readRDS(paste0(here, "/output/rec_values.Rds"))


#Normalizing rec values since hobo values normalization will give different 
#values from rec values normalization and we need both

rec_norm <- hobo_rec_events%>%
  group_by(recession_n)%>%
  mutate(sec = seconds(dt) - min(seconds(dt)),
         sec_norm = sec/max(sec),
         yld_mm_norm1 = yield_mm - min(yield_mm),
         yld_mm_norm2 = yld_mm_norm1/max(yld_mm_norm1),
         power_yield = 10^yld_mm_norm2)%>%
  ungroup()%>%
  distinct(across(power_yield), .keep_all = TRUE)%>%
  drop_na()



#linear models for recession n
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
  ungroup()%>%
  distinct(across(recession_n), .keep_all = TRUE)


#Saving rec models
saveRDS(nested_rec_events, paste0(here, "/output/rec_model.Rds"))
write_csv(nested_rec_events,
          paste0(here,"/output/rec_model.csv"))



#Recession curve model coefficients

model_rec_events <- rec_norm %>%
  dplyr::group_by(recession_n)%>%
  nest() %>%
  mutate(m1 = purrr::map(.x = data, .f = ~ lm(power_yield~ sec_norm, data = .)),
         glance = map(.x = m1, .f = ~broom::glance(.x)),
         preds = map(m1, broom::augment),
         r = map_dbl(.x = data, .f = ~cor(.x$power_yield, .x$sec_norm, use="complete.obs")),
         RMSE = map_dbl(preds, .f = ~sqrt(mean(.x$.resid^2)))) %>%
  unnest(preds)%>%
  ungroup()



ggplot(model_rec_events) +
  geom_line(mapping = aes(x=sec_norm, y=.fitted, colour = recession_n))
  facet_wrap(~ hobo_event_n, scales = "free")


#Assigning hobo events number, species as sites and converting he fitted prediction to rate
rate_convertion <-full_join(model_rec_events, hobo_rec_events,
                            by = c( "recession_n")) %>%
  distinct(across(.fitted), .keep_all = TRUE) %>%
  dplyr::group_by(recession_n) %>%
  mutate(rate_yld = .fitted - lag(.fitted)) %>%
  ungroup()


#Plotting normalized time vs yield rate with columns
ggplot(rate_convertion) +
  geom_col(mapping = aes(x=sec_norm, y=rate_yld,colour = site2))+
  facet_wrap(~ hobo_event_n, scales = "free")

#Plotting normalized time vs yield rate with points
ggplot(rate_convertion) +
  geom_point(mapping = aes(x=sec_norm, y=.fitted, colour = site2),
            size = 3.5)+
  facet_wrap(~ hobo_event_n, scales = "free") +
  theme_bw()

