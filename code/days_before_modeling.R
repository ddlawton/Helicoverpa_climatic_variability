rm(list=ls())

library(data.table)
library(tidyverse)
library(splitstackshape)
library(ggridges)
library(ggpubr)
library(lubridate)
library(patchwork)
library(mgcv)
library(DHARMa)
library(anytime)

dat <- fread("data/processed/daily_dat_Jan282022.csv") %>% as_tibble() %>%
  mutate(location = factor(location),season = factor(season),
         dates = as.Date(dates),
         diff_time = as.numeric(difftime(anytime(time_end/1000),dates,units="days")),
         num_obs = factor(num_obs))

air <- ggplot(dat,aes(x=diff_time,y=Air_temp,z=sqrt(cew_year))) + stat_summary_hex() + viridis::scale_fill_viridis()

precip <- ggplot(dat,aes(x=diff_time,y=Precip,z=sqrt(cew_year))) + stat_summary_hex() + viridis::scale_fill_viridis()

soil_moist <- ggplot(dat,aes(x=diff_time,y=avg_soil_moist ,z=sqrt(cew_year))) + stat_summary_hex() + viridis::scale_fill_viridis()

soil_temp <- ggplot(dat,aes(x=diff_time,y=avg_soil_temp ,z=sqrt(cew_year))) + stat_summary_hex() + viridis::scale_fill_viridis()

(air + soil_temp) / (precip+soil_moist)


mod <- bam(cew_year ~ te(diff_time, avg_soil_temp,bs=c('tp','tp'),k=25) + 
                      s(season ,bs="re") +
                      s(num_obs,bs="re") +
                      s(location,bs="re"), 
            select=TRUE, family=tw(),data=dat,discrete = TRUE, nthreads = 23)

mod_soil_moisture <- bam(cew_year ~ te(diff_time, avg_soil_moist,bs=c('tp','tp'),k=25) + 
             s(num_obs,bs="re") +
             s(location,bs="re"), 
           select=TRUE, family=tw(),data=dat,discrete = TRUE, nthreads = 23)

mod_precip <- bam(cew_year ~ s(diff_time, Precip,bs=c('tp','tp'),k=25) + 
                           s(num_obs,bs="re") +
                           s(location,bs="re"), 
                         select=TRUE, family=tw(),data=dat,discrete = TRUE, nthreads = 23)

mod_air_temp <- bam(cew_year ~ s(diff_time, Air_temp,bs=c('tp','tp'),k=25) + 
                    s(num_obs,bs="re") +
                    s(location,bs="re"), 
                  select=TRUE, family=tw(),data=dat,discrete = TRUE, nthreads = 23)

mod_both <- bam(cew_year ~ te(diff_time, avg_soil_temp,bs=c('tp','tp'),k=50) + 
                  te(diff_time, avg_soil_moist,bs=c('tp','tp'),k=50) +
                  s(num_obs,bs="re") +
                  s(location,bs="re"), 
                select=TRUE, family=tw(),data=dat,discrete = TRUE, nthreads = 23)

BIC(mod,mod_soil_moisture,mod_precip,mod_air_temp,mod_both)

concurvity(mod_both)

summary(mod)
gratia::draw(mod)
k.check(mod_soil_moisture)


b0 <- coef(mod_soil_moisture)[1]

test <- gratia::smooth_estimates(mod_soil_moisture)

test$adj_est <- test$est + b0


avg_soil_moist  <- test %>% filter(smooth == "s(diff_time,avg_soil_temp)") %>%
  ggplot(aes(y=avg_soil_temp ,x=diff_time, z=adj_est)) + stat_summary_hex() +
  theme_pubr() + viridis::scale_fill_viridis()

num_obs <- test %>% filter(smooth == "s(num_obs)") %>%
  ggplot(aes(x=num_obs,y=adj_est)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se),alpha=.1) +
  geom_line(aes(y = adj_est)) + theme_pubr() + coord_cartesian(ylim=c(0,NA))
