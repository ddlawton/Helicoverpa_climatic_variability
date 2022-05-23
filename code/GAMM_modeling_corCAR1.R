#####
# GAM modeling 
#  with climatic variables 
#####



library(tidyverse)
library(splitstackshape)
library(mgcv)


data <- read_csv("data/processed/INLA_data_mesh_20220426.csv") %>%
  filter(type != "mesh") %>%
  mutate(total_count = round(total_count)) %>%
  mutate(species=as.factor(species),
         trap = as.factor(trap),
         sp_trp = factor(paste0(species,"_",trap)))

offseason <- data %>% filter(season=="off_season") # just using one model type since the correlation structure is the same.
growseason <- data %>% filter(season=="growing_season")


lagseason <- data %>%
  filter(season=="growing_season") %>%
  group_by(trap,species) %>%
  mutate(lag_air = lag(Air_temp, order_by=year),
         lag_precip = lag(Precip, order_by=year),) %>%
  drop_na(lag_air,lag_precip)


ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")


offseason_mod <- gamm(total_count ~
               te(Air_temp,species,bs="fs") +
               te(Precip,species,bs="fs") +
               s(year,bs="tp") +
             te(Latitude,Longitude) +
             s(num_yearly_trap_obs) +
             s(species,bs="re") +
             s(trap,bs="re"),
             data=offseason, correlation = corCAR1(value = 0.2,form = ~ year|sp_trp),
             control = ctrl,family=Tweedie(p=1.3))

growingseason_mod <- gamm(total_count ~
                        te(Air_temp,species,bs="fs") +
                        te(Precip,species,bs="fs") +
                        s(year,bs="tp")+
                      te(Latitude,Longitude) +
                      s(num_yearly_trap_obs) +
                      s(species,bs="re") +
                      s(trap,bs="re"),
                      data=growseason, correlation = corCAR1(value = 0.2,form = ~ year|sp_trp),
                      control = ctrl,family=Tweedie(p=1.3))


lagseason_mod <- gamm(total_count ~
                            te(Air_temp,species,bs="fs") +
                            te(Precip,species,bs="fs") +
                            s(year,bs="tp") +
                          te(Latitude,Longitude) +
                          s(num_yearly_trap_obs) +
                          s(species,bs="re") +
                          s(trap,bs="re"),
                          data=lagseason, correlation = corCAR1(value = 0.2,form = ~ year|sp_trp),
                          control = ctrl,family=Tweedie(p=1.3))

saveRDS(offseason_mod,file="models/offseason_mod.rds")
saveRDS(growingseason_mod,file="models/growingseason_mod.rds")
saveRDS(lagseason_mod,file="models/lagseason_mod.rds")

print(AIC(offseason_mod$lme,growingseason_mod$lme,lagseason_mod$lme))
print(BIC(offseason_mod$lme,growingseason_mod$lme,lagseason_mod$lme))


#below is a workspace









mod1 <- gamm(total_count ~
               te(Air_temp,species,bs="fs") +
               te(Precip,species,bs="fs") +
               s(year,bs="tp"),
               te(Latitude,Longitude) +
               s(num_yearly_trap_obs) +
               s(species,bs="re") +
               s(trap,bs="re"),
             family=Tweedie(p=1.3),data=offseason)

mod1 <- gamm(total_count ~
               te(Air_temp,species,bs="fs") +
               te(Precip,species,bs="fs") +
               s(year,bs="tp"),
             te(Latitude,Longitude) +
               s(num_yearly_trap_obs) +
               s(species,bs="re") +
               s(trap,bs="re"),
             family=Tweedie(p=1.3),data=offseason)


summary(mod1$gam)
gratia::appraise(mod1$gam)

acf(resid(mod1$lme,type='normalized'))
pacf(resid(mod1$lme,type='normalized'))

mod2 <- gamm(total_count ~
               te(Air_temp,species,bs="fs") +
               te(Precip,species,bs="fs") +
               s(year,bs="tp"),
               #te(Latitude,Longitude) +
               #s(num_yearly_trap_obs) +
               #s(species,bs="re") +
               #s(trap,bs="re"),
             data=offseason, correlation = corCAR1(value = 0.2,form = ~ year|sp_trp),
             control = ctrl,family=Tweedie(p=1.3))
acf(resid(mod2$lme,type='normalized'))
pacf(resid(mod2$lme,type='normalized'))



modres <- gratia::smooth_estimates(mod1)
modres1 <- gratia::smooth_estimates(mod2)


modres %>%
  filter(smooth == "te(Air_temp,species)") %>%
  ggplot(aes(x=Air_temp)) +
  geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=species),alpha=.2) +
  geom_line(aes(y = est,color=species),size=1) + ggpubr::theme_pubr() +
  ggtitle("Model 1 air temp") + xlab("Air temperature")

modres1 %>%
  filter(smooth == "te(Air_temp,species)") %>%
  ggplot(aes(x=Air_temp)) +
  geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=species),alpha=.2) +
  geom_line(aes(y = est,color=species),size=1) + ggpubr::theme_pubr() +
  ggtitle("Model 1 air temp") + xlab("Air temperature")



gratia::appraise(mod2$gam)
?gamm

mod3 <- gamm(total_count ~
               te(Air_temp,species,bs="fs") +
               te(Precip,species,bs="fs"),
               #te(Latitude,Longitude) +
               #s(num_yearly_trap_obs) +
               #s(species,bs="re") +
               #s(trap,bs="re"),
             data=offseason, correlation = corCAR1(value = 0.4,form = ~ year|sp_trp),
             control = ctrl)

mod4 <- gamm(total_count ~
               te(Air_temp,species,bs="fs") +
               te(Precip,species,bs="fs"),
               #te(Latitude,Longitude) +
               #s(num_yearly_trap_obs) +
               #s(species,bs="re") +
               #s(trap,bs="re"),
             family=tw(),data=offseason, correlation = corCAR1(value = 0.6,form = ~ year|sp_trp),
             control = ctrl)



