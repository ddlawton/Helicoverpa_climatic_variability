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


coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
}

dat <- read.csv("data/processed/seasonal_averaged_dat_Jan282022.csv") %>% as_tibble() %>%
  mutate(location = factor(location),season = factor(season))


dat %>%
  ggplot(aes(x=Air_temp,y=cew_year,color=season)) +
  geom_smooth() #+ geom_point(pch=21,size=.5,alpha=.25)


dat %>%
  ggplot(aes(x=Precip,y=cew_year,color=season)) +
  geom_smooth() #+ geom_point(pch=21,size=.5,alpha=.25)

dat %>%
  ggplot(aes(x=avg_soil_temp,y=cew_year,color=season)) +
  geom_smooth()

dat %>%
  ggplot(aes(x=avg_soil_moist,y=cew_year,color=season)) +
  geom_smooth()

dat <- dat %>% mutate(num_obs = factor(num_obs))

mod_air_temp_fs <- bam(cew_year ~ s(Air_temp,by=season,bs=("tp"),k=800) + s(num_obs,bs="re") + s(location,bs="re"), select=TRUE, family=tw(),data=dat,
                    discrete = TRUE, nthreads = 23)


k.check(mod_air_temp_fs)
gratia::draw(mod_air_temp_fs)
summary(mod_air_temp_fs)

mod_air_temp_fs <- bam(cew_year ~ s(Air_temp,season,bs="fs",k=500) + s(num_obs,k=25) + s(location,bs="re"), select=TRUE, family=tw(),data=dat,
                       discrete = TRUE, nthreads = 23)



mod_air_temp_lm <- bam(cew_year ~ (Air_temp)*season + s(num_obs,k=25) + s(location,bs="re"), select=TRUE, family=tw(),data=dat,
                    discrete = TRUE, nthreads = 23)







mod_soil_moist_fs <- bam(cew_year ~ s(avg_soil_temp ,season,bs="fs",k=50) + s(num_obs,bs="re") + s(location,bs="re"), select=TRUE, family=tw(),data=dat,
                       discrete = TRUE, nthreads = 23)



mod_soil_moist_by <- bam(cew_year ~ s(avg_soil_temp ,by=season,bs="tp",k=750) + s(num_obs,bs="re") + s(location,bs="re"), select=TRUE, family=tw(),data=dat,
                         discrete = TRUE, nthreads = 23)

mod_soil_moist_by_nb <- bam(cew_year ~ s(avg_soil_temp ,by=season,bs="tp",k=750) + s(num_obs,bs="re") + s(location,bs="re"), select=TRUE, family=nb(),data=dat,
                         discrete = TRUE, nthreads = 23)

linear_dat <- dat %>%
  pivot_wider(
    names_from = season,
    names_sep = ".",
    values_from = c(Air_temp, Precip,avg_soil_temp,avg_soil_moist)
  ) %>%
  group_by(location) %>%
  summarise_all(coalesce_by_column) 


linear_mod <- bam(cew_year ~ avg_soil_temp *season + s(num_obs,bs="re") + s(location,bs="re"),
                  select=TRUE, family=tw(),
                  discrete = TRUE, nthreads = 23,data=dat)

plot(linear_mod,all.terms = TRUE)



summary(linear_mod)

dat %>%
  pivot_longer(cols=c(6:9)) %>%
  ggplot(aes(x=value,y=cew_year,color=season)) + geom_smooth(method = "lm") +
  facet_wrap(~name,scales="free")

dat %>%
  pivot_longer(cols=c(6:9)) %>%
  ggplot(aes(x=value,y=cew_year,color=season)) + geom_smooth(method = "gam") +
  facet_wrap(~name,scales="free")





resid <- simulateResiduals(mod_soil_moist_by_nb)
plot(resid)


AIC(mod_soil_moist_fs,mod_soil_moist_by)

summary(mod_soil_moist_by)
gratia::draw(mod_soil_moist_by_nb)

k.check(mod_soil_moist_by)
mod_Precip <- bam(cew_year ~ s(Precip,season,bs="fs",k=10) + s(num_obs,k=10), select=TRUE, family=tw(),data=dat4)
mod_avg_soil_moist <- bam(cew_year ~ s(avg_soil_moist,season,bs="fs",k=10) + s(num_obs,k=10), select=TRUE, family=tw(),data=dat4)
mod_avg_soil_temp <- bam(cew_year ~ s(avg_soil_temp,season,bs="fs",k=10) + s(num_obs,k=10), select=TRUE, family=tw(),data=dat4)

k.check(mod_air_temp)
resid <- simulateResiduals(mod_air_temp)
plot(mod_air_temp_lm,all.terms = TRUE)


summary(mod_avg_soil_temp)
gratia::draw(mod)
gratia::appraise(mod)
concurvity(mod,full=FALSE)


concurvity(mod_avg_soil_moist,full=FALSE)
b0 <- coef(mod_air_temp)[1]

test <- gratia::smooth_estimates(mod_air_temp)

test$adj_est <- test$est + b0


air_temp <- test %>% filter(smooth == "s(Air_temp,season)") %>%
  ggplot(aes(x=Air_temp,y=adj_est)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=season),alpha=.1) +
  geom_line(aes(y = adj_est, color=season)) + theme_pubr() + facet_grid(~season)

num_obs <- test %>% filter(smooth == "s(num_obs)") %>%
  ggplot(aes(x=num_obs,y=adj_est)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se),alpha=.1) +
  geom_line(aes(y = adj_est)) + theme_pubr() + coord_cartesian(ylim=c(0,NA))

air_temp + num_obs
