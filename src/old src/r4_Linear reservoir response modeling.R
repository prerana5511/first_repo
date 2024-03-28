library(here)
here <- here()
here
library(tidyverse)
library(lubridate)
library(dygraphs)
library(xts)
# library(ggplot2)
library(ggpubr)
library(broom)
library(AICcmodavg)


hobo_events_limbs<- readRDS(paste0(here, "/output/hobo_limbs.Rds"))

hobo_events2 <- hobo_events_limbs%>%
  filter(limb == "falling") %>%
  group_by( site, hobo_event_n)%>%
  mutate(time =  seconds((dt - dt_max_yield_mm)*60),
         time2 = time_length(time, unit = "seconds"))%>%
  mutate(log_yield = log(roll_yield))%>%
  ungroup()
    

 hobo_events2[c('log_yield')][sapply(hobo_events2[c('log_yield')], is.infinite)] <- NA
 
 hobo_events3 <-hobo_events2%>%
   group_by(site,hobo_event_n)%>%
   distinct(across(log_yield), .keep_all = TRUE)%>%
   na.omit()%>%
   ungroup()





tree_sp_r4 <- hobo_events3%>%
  pivot_wider(names_from = "site", values_from = "log_yield")%>%
  pivot_longer(cols = c("SFA_mm","SFC_mm") , names_to = "Sugar", 
               values_to = "Sugar_Maple_mm")%>%
  pivot_longer(cols = c("SFB_mm","SFD_mm") , names_to = "Yellow", 
               values_to = "Yellow_Birch_mm")%>%
  
  pivot_longer(cols = c("Yellow_Birch_mm","Sugar_Maple_mm") , names_to = "Stemflow", 
               values_to = "stemflow_yield")%>%
  pivot_longer(cols = c("TFB_mm","TFD_mm") , names_to = "Throughfall", 
               values_to = "throughfall_yield")%>%
  select(-cumyield_mm, -yield_mm, -Sugar, -Yellow)

 



stemflow_r4 <- tree_sp_r4%>%
  group_by(hobo_event_n, Stemflow)%>%
  filter(!is.na(stemflow_yield))%>%
  distinct(across(stemflow_yield), .keep_all = TRUE)%>%
  select(-throughfall_yield, -Throughfall)%>%
  ungroup()




throughfall_r4 <- tree_sp_r4%>%
  group_by(hobo_event_n, Throughfall)%>%
  filter(!is.na(throughfall_yield))%>%
  distinct(across(throughfall_yield), .keep_all = TRUE)%>%
  select(-stemflow_yield, -Stemflow)%>%
  ungroup()







tr_r4 <-hobo_events3%>%
  pivot_wider(names_from = "site", values_from = "log_yield")%>%
  pivot_longer(cols = c("SFA_mm","SFC_mm") , names_to = "Sugar", 
               values_to = "Sugar_Maple_mm")%>%
  pivot_longer(cols = c("SFB_mm","SFD_mm") , names_to = "Yellow", 
               values_to = "Yellow_Birch_mm")%>%
  pivot_longer(cols = c("TFB_mm","TFD_mm") , names_to = "Throughfall", 
               values_to = "Throughfall_yield")%>%
  select(dt, Sugar_Maple_mm, Yellow_Birch_mm, Throughfall_yield, hobo_event_n, time2)%>%
  
  pivot_longer(cols = c("Sugar_Maple_mm", "Throughfall_yield", "Yellow_Birch_mm") , names_to = "flowpath2", 
               values_to = "path_yield_rate")%>%
  na.omit()
  #distinct(across(path_yield_rate), .keep_all = TRUE)


tr1_r4 <-hobo_events3%>%
  pivot_wider(names_from = "site", values_from = "log_yield")%>%
  pivot_longer(cols = c("SFA_mm","SFC_mm") , names_to = "Sugar", 
               values_to = "Sugar_Maple_mm")%>%
  pivot_longer(cols = c("SFB_mm","SFD_mm") , names_to = "Yellow", 
               values_to = "Yellow_Birch_mm")%>%
  select(Sugar,Sugar_Maple_mm,Yellow, Yellow_Birch_mm, hobo_event_n, dt, time2)

yb <-tr1_r4 %>%
  group_by(Yellow, hobo_event_n)%>%
  select(-Sugar_Maple_mm, -Sugar, -dt)%>%
  distinct(across(Yellow_Birch_mm), .keep_all = TRUE)%>%
  na.omit()%>%
  ungroup()



sm <-tr1_r4 %>%
  group_by( Sugar,hobo_event_n)%>%
  select(-Yellow_Birch_mm, -Yellow, -dt)%>%
  distinct(across(Sugar_Maple_mm), .keep_all = TRUE)%>%
  na.omit()%>%
  ungroup()



mix_r4 <-inner_join(stemflow_r4, throughfall_r4,
                 by = c( "hobo_event_n", "time2")) %>%
  select(-Stemflow, -Throughfall)%>%
  pivot_longer(cols = c("stemflow_yield","throughfall_yield") , 
               names_to = "flowpath", values_to = "path_yield_rate")%>%
  select(hobo_event_n, path_yield_rate,time2, flowpath)%>%
  distinct(across(path_yield_rate), .keep_all = TRUE)


mix2_r4 <- inner_join(mix_r4, tr_r4,
                      by = c( "hobo_event_n", "time2", "path_yield_rate"))
  #mutate(log_flowpath_yield_rate = log(path_yield_rate))
  


mix_sp <- inner_join(sm, yb,
                     by = c( "hobo_event_n", "time2"))%>%
  pivot_longer(cols = c("Sugar_Maple_mm","Yellow_Birch_mm") , 
               names_to = "Species", values_to = "path_yield_rate")


throughfall_from_site <- hobo_events3 %>%
  group_by(site,hobo_event_n)%>%
  filter(str_detect(site, "TF"))%>%
  pivot_wider(names_from = "site", values_from = "log_yield")%>%
  pivot_longer(cols = c("TFB_mm", "TFD_mm") , names_to = "throughfall", 
               values_to = "Yield_rate")%>%
  na.omit()%>%
  ungroup()
  
  

nested_flowpath <- mix2_r4 %>%
  group_by(flowpath,hobo_event_n) %>%
  nest() %>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x)))%>%
  mutate(r = map_dbl(.x = data, .f = ~cor(y=.x$path_yield_rate, x = .x$time2,
                                          use = "na.or.complete")),
         m = map_dbl(data, ~lm(path_yield_rate~ time2, data = .)$coefficients[[2]]),
         i = map_dbl(data, ~lm(path_yield_rate ~ time2, data = .)$coefficients[[1]]),
         r2 = r^2)%>%
  ungroup()




nested_species <- mix_sp %>%
  group_by(Species,hobo_event_n) %>%
  select(-Yellow, -Sugar)%>%
  # filter(flowpath2!= "Throughfall_yield")%>%
  nest() %>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x)))%>%
  mutate(r = map_dbl(.x = data, .f = ~cor(y=.x$path_yield_rate, x = .x$time2,
                                          use = "na.or.complete")),
         m = map_dbl(data, ~lm(path_yield_rate~ time2, data = .)$coefficients[[2]]),
         i = map_dbl(data, ~lm(path_yield_rate ~ time2, data = .)$coefficients[[1]]),
         r2 = r^2)%>%
  ungroup()




nested_SM <- sm %>%
  group_by(Sugar,hobo_event_n) %>%
  nest() %>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x)))%>%
  mutate(r = map_dbl(.x = data, .f = ~cor(y=.x$Sugar_Maple_mm, x = .x$time2,
                                          use = "na.or.complete")),
         m = map_dbl(data, ~lm(Sugar_Maple_mm~ time2, data = .)$coefficients[[2]]),
         i = map_dbl(data, ~lm(Sugar_Maple_mm ~ time2, data = .)$coefficients[[1]]),
         r2 = r^2)%>%
  ungroup()



nested_YB <- yb %>%
  group_by(Yellow,hobo_event_n) %>%
  nest() %>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x)))%>%
  mutate(r = map_dbl(.x = data, .f = ~cor(y=.x$Yellow_Birch_mm, x = .x$time2,
                                          use = "na.or.complete")),
         m = map_dbl(data, ~lm(Yellow_Birch_mm~ time2, data = .)$coefficients[[2]]),
         i = map_dbl(data, ~lm(Yellow_Birch_mm~ time2, data = .)$coefficients[[1]]),
         r2 = r^2)%>%
  ungroup()




nested_TF <- throughfall_from_site %>%
  group_by(throughfall,hobo_event_n) %>%
  nest() %>%
  mutate(nobs = map_dbl(.x = data, .f = ~nrow(.x)))%>%
  mutate(r = map_dbl(.x = data, .f = ~cor(y=.x$Yield_rate, x = .x$time2,
                                          use = "na.or.complete")),
         m = map_dbl(data, ~lm(Yield_rate~ time2, data = .)$coefficients[[2]]),
         i = map_dbl(data, ~lm(Yield_rate~ time2, data = .)$coefficients[[1]]),
         r2 = r^2)%>%
  ungroup()



anova_flowpath <- aov(m ~ flowpath, data = nested_flowpath)
summary(anova_flowpath)
anova_flowpath <- aov(i ~ flowpath, data = nested_flowpath)
summary(anova_flowpath)
anova_flowpath <- aov(r ~ flowpath, data = nested_flowpath)
summary(anova_flowpath)



anova_Species <- aov(m ~ Species, data = nested_species)
summary(anova_Species)
anova_Species <- aov(i ~ Species, data = nested_species)
summary(anova_Species)
anova_Species <- aov(r ~ Species, data = nested_species)
summary(anova_Species)



anova_SM<- aov(m ~ Sugar, data = nested_SM)
summary(anova_SM)
anova_SM<- aov(i ~ Sugar, data = nested_SM)
summary(anova_SM)
anova_SM<- aov(r ~ Sugar, data = nested_SM)
summary(anova_SM)



anova_YB <- aov(m ~ Yellow, data = nested_YB)
summary(anova_YB )
anova_YB <- aov(i ~ Yellow, data = nested_YB)
summary(anova_YB )
anova_YB <- aov(r ~ Yellow, data = nested_YB)
summary(anova_YB )



anova_throughfall <- aov(m ~ throughfall, data = nested_TF )
summary(anova_throughfall)
anova_throughfall <- aov(i ~ throughfall, data = nested_TF )
summary(anova_throughfall)
anova_throughfall <- aov(r ~ throughfall, data = nested_TF )
summary(anova_throughfall)





































#   
# library(dplyr)
# library(plyr)
# nd <-nested_flowpath%>%
#   unnest(data)%>%
#   distinct(across(log_flowpath_yield_rate), .keep_all = TRUE)
# 
# nd1 <- nd %>%
#   group_by(hobo_event_n,flowpath)%>%
#   mutate(log_mod = mean(log_flowpath_yield_rate))
# 
# 
# ggplot(nd, mapping = aes(x=time2, y =log_flowpath_yield_rate, group = flowpath, colour = flowpath)) +
#   geom_point()+
#   stat_smooth(method = "lm", col = "red")+
#   # scale_y_log10()+
#   # geom_line(mapping = aes(x=time2, y =log_flowpath_yield_rate,
#   #                         group = flowpath, colour = flowpath)) +
#   
#   facet_wrap(~ hobo_event_n, scales = "free")+
# 
#   labs(title="Log-linear Models",
#        x ="Time (s)", y = "Log Yield (mm)")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #plot the data
# plot(mix2_r4$flowpath_yield_rate ~mix2_r4$time2)
# 
# 
# # Creates a linear model
# my_loglin_model <- lm(log(flowpath_yield_rate) ~time2, data = mix2_r4)
# summary(my_loglin_model)
# 
# time2=seq(
#   from=0,to=60000,length.out=60000)
# 
# pred_flowpath_yield_rate=predict(my_loglin_model,newdata=list(time2=seq(
#   from=0,to=60000,length.out=60000)),
#   interval="confidence")
# 
# 
# plot(mix2_r4$flowpath_yield_rate ~mix2_r4$time2, pch = 19,
#      col = "lightgray", 
#      lty = 1, xlab="time", ylab="Log Yield Rate") 
#   matlines( time2,pred_flowpath_yield_rate, lwd=2, col = "red")
# title('Stemflow vs Throughfall ')
# 
# 
# lines(time2, pred_flowpath_yield_rate, col = "red",
#       lwd = 2, lty = 1)
# 
# 
# 
# 
# matplot(pred_flowpath_yield_rate, type = "l",
#         col = "lightgray",
#         lty = 1)
# 
# 
# 
# 
# #Stemflow (SM vs YB)-----
# 
# #plot the data
# plot(stemflow_r4$stemflow_yield ~stemflow_r4$time2)
# 
# 
# # Creates a linear model
# my_loglin_model <- lm(stemflow_yield~log(time2), data = stemflow_r4)
# summary(my_loglin_model)
# 
# 
# time2=seq(from=0,to=120000,length.out=120000)
# 
# stemflow_yield=predict(my_loglin_model,newdata=list(time2=seq(
#   from=0,to=120000,length.out=120000)),
#   interval="confidence")
# # matplot(stemflow_yield, type = "l",
# #         col = "gray",
# #         lty = 1)
# 
# 
# plot(stemflow_r4$stemflow_yield ~stemflow_r4$time2, pch = 19,
#      col = "lightgray", 
#      lty = 1, xlab="time", ylab=" Yield Rate") +
#   matlines(time2,stemflow_yield, lwd=2, col = "red")
# title('Stemflow')
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #SM-----
# 
# #plot the data
# plot(sm$Sugar_Maple_mm ~sm$time2)
# 
# 
# # Creates a linear model
# my_loglin_model <- lm(Sugar_Maple_mm~log(time2), data = sm)
# summary(my_loglin_model)
# 
# 
# time2=seq(from=0,to=80000,length.out=80000)
# 
# Sugar_Maple_mm=predict(my_loglin_model,newdata=list(time2=seq(
#   from=0,to=80000,length.out=80000)),
#   interval="confidence")
# 
# 
# # matplot(Sugar_Maple_mm, type = "l",
# #         col = "lightgray",
# #         lty = 1)
# # matlines(time2,Sugar_Maple_mm, lwd=2)
# 
# plot(sm$Sugar_Maple_mm ~sm$time2, pch = 19,
#      col = "lightgray", 
#      lty = 1, xlab="time", ylab="Yield Rate",) +
#   matlines(time2,Sugar_Maple_mm, lwd=2, col = "red")
# title('Sugar Maple')
# 
# 
# 
# 
# 
# #YB-----
# 
# #plot the data
# plot(yb$Yellow_Birch_mm ~yb$time2)
# 
# 
# # Creates a linear model
# my_loglin_model <- lm(Yellow_Birch_mm ~log(time2), data = yb)
# summary(my_loglin_model)
# 
# 
# time2=seq(from=0,to=120000,length.out=120000)
# 
# Yellow_Birch_mm=predict(my_loglin_model,newdata=list(time2=seq(
#   from=0,to=120000,length.out=120000)),
#   interval="confidence")
# 
# # matplot(Yellow_Birch_mm, type = "l",
# #         col = "lightgray",
# #         lty = 1)
# # matlines(time2,Yellow_Birch_mm, lwd=2)
# 
# 
# plot(yb$Yellow_Birch_mm ~yb$time2, pch = 19,
#      col = "lightgray", 
#      lty = 1, xlab="time", ylab="Yield Rate",) +
#   matlines(time2,Yellow_Birch_mm, lwd=2, col = "red")
# title('Yellow Birch')
# 
# 
# 
# 
# 
# 
# 
# 
# #Throughfall-----
# 
# #plot the data
# plot(throughfall_r4$throughfall_yield ~throughfall_r4$time2)
# 
# 
# # Creates a linear model
# my_loglin_model <- lm(throughfall_yield ~log(time2), data = throughfall_r4)
# summary(my_loglin_model)
# 
# 
# time2=seq(from=0,to=120000,length.out=120000)
# 
# throughfall_yield=predict(my_loglin_model,newdata=list(time2=seq(
#   from=0,to=120000,length.out=120000)),
#   interval="confidence")
# 
# # matplot(throughfall_yield, type = "l",
# #         col = "lightgray",
# #         lty = 1)
# # matlines(time2,throughfall_yield, lwd=2)
# 
# plot(throughfall_r4$throughfall_yield ~throughfall_r4$time2, pch = 19,
#      col = "lightgray", 
#      lty = 1, xlab="time", ylab="Yield Rate",) +
#   matlines(time2,throughfall_yield, lwd=2, col = "red")
# title('Throughfall')
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #Stemflow vs Throughfall-----
# 
# 
# 
# 
# 
# #plot the data
# plot(mix_r4$flowpath_yield_rate ~mix_r4$time2)
# 
# 
# # Creates a linear model
# my_loglin_model <- lm(log_flowpath_yield_rate ~time2, data = mix_r4)
# summary(my_loglin_model)
# 
# 
# 
# time2=seq(
#   from=0,to=60000,length.out=60000)
# 
# pred_flowpath_yield_rate=predict(my_loglin_model,newdata=list(time2=seq(
#   from=0,to=60000,length.out=60000)),
#   interval="confidence")
# 
# matplot(time2,cbind(pred_flowpath_yield_rate),pch=c(16,1),xlab="x",ylab="X^2 and X^2-2*X")
# 
# 
# 
# 
# 
# 
# 
# pred_flowpath_yield_rate=predict(my_loglin_model,newdata=list(time2=seq(
#   from=0,to=60000,length.out=60000)),
#   interval="confidence")
# 
# plot(mix_r4$flowpath_yield_rate ~mix_r4$time2, pch = 19,
#      col = "lightgray", 
#      lty = 1, xlab="time", ylab="Log Yield Rate") +
#   matlines( time2,pred_flowpath_yield_rate, lwd=2 )
# title('Stemflow vs Throughfall ')
# 
# 
# lines(time2, pred_flowpath_yield_rate, col = "red",
#       lwd = 2, lty = 1)
# 












