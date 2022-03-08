########
# Cleaning up Helicoverpa 
#  RS data
######

rm(list=ls())

library(tidyverse)
library(mgcv)
library(splitstackshape)
library(data.table)
library(anytime)
library(lubridate)
library(patchwork)
library(INLA)

dat <- fread("data/processed/Helicoverpa_global_data_monthly_climate.csv") %>% as_tibble() %>%
  select(!`system:index`) %>% 
  as_tibble() %>%
  mutate(.geo = as.character(regmatches(.geo, gregexpr("\\[\\K[^\\]]+(?=\\])", .geo, perl=TRUE)))) %>%
  separate(.geo,into=c("Longitude","Latitude"),sep=",") %>% rowid_to_column()


dat2 <- cSplit(setDT(dat)[, lapply(.SD, gsub, pattern = "[][]", 
                                   replacement = "")], names(dat), sep=",", fixed = FALSE, "long")



seasons <- c("Fall","Winter","Spring")


dat3 <- dat2 %>% 
  tidyr::fill(rowid,Nmbroyt,Species,Trap,Year,contnnt,mdn_cnt,meancnt,se_cont,time_nd,tm_strt,Longitude,Latitude,.direction = "down") %>%
  as_tibble() %>%
  drop_na("Precip") %>% as_tibble() %>%
  mutate(Precip = Precip * 1000,
         dates = anytime::anytime(dates/1000),
         climate_month = month(dates),
         climate_year = year(dates),
         Air_temp = Air_temp - 273.15,
         soil_temp_1 = soil_temp_1 - 273.15,
         soil_temp_2 = soil_temp_2 - 273.15) %>%
  filter(contnnt != "Australia") %>%
  group_by(rowid) %>%
  filter(Year == climate_year, climate_month %in% 2:9) %>%
  group_by(Trap,Year) %>%
  summarize(rowid = first(rowid),Air_temp_mean = mean(Air_temp),Num_traps = first(Nmbroyt),Precip_mean = mean(Precip),continent = first(contnnt),
            mean_count = first(mdn_cnt),se_count = first(se_cont),soil_moisture1 = mean(soil_moist_1), soil_moisture2 = mean(soil_moist_2),
            soil_temp1 = mean(soil_temp_1), soil_temp2 = mean(soil_temp_2),Longitude = first(Longitude), Latitude = first(Latitude))

AT <- ggplot(dat3,aes(x=Air_temp_mean,y=mean_count)) + 
  geom_smooth() + 
  geom_rug(sides="b") +
  coord_cartesian(ylim=c(0,100)) + 
  ggpubr::theme_pubr() +
  ggtitle("Seasonal mean air temperature")

Pre <- ggplot(dat3,aes(x=Precip_mean,y=mean_count)) + 
  geom_smooth() + 
  geom_rug(sides="b") +
  coord_cartesian(ylim=c(0,200)) + 
  ggpubr::theme_pubr() +
  ggtitle("Seasonal mean precipitation")

SM1 <- ggplot(dat3,aes(x=soil_moisture1,y=mean_count)) + 
  geom_smooth() + 
  geom_rug(sides="b") +
  coord_cartesian(ylim=c(0,100)) + 
  ggpubr::theme_pubr() +
  ggtitle("Seasonal mean soil moisture (0-5cm)")

SM2 <-ggplot(dat3,aes(x=soil_moisture2,y=mean_count)) + 
  geom_smooth() + 
  geom_rug(sides="b") +
  coord_cartesian(ylim=c(0,100)) + 
  ggpubr::theme_pubr()+
  ggtitle("Seasonal mean soil moisture (5-25cm)")

ST1 <- ggplot(dat3,aes(x=soil_temp1,y=mean_count)) + 
  geom_smooth() + 
  geom_rug(sides="b") +
  coord_cartesian(ylim=c(0,100)) + 
  ggpubr::theme_pubr() +
  ggtitle("Seasonal mean soil temperature (0-5cm)")

ST2 <- ggplot(dat3,aes(x=soil_temp2,y=mean_count)) + 
  geom_smooth() + 
  geom_rug(sides="b") +
  coord_cartesian(ylim=c(0,100)) + 
  ggpubr::theme_pubr()  +
  ggtitle("Seasonal mean soil temperature (5-25cm)")

(AT + Pre) / (SM1 + SM2 + ST1 + ST2)

dat3 <- dat3 %>% mutate(Trap = factor(Trap))

mod <- bam(mean_count ~ s(Air_temp_mean,k=20) + s(Precip_mean,k=20) + s(Trap,bs="re"),data=dat3,select=TRUE,discrete = TRUE,nthreads=4)
  

summary(mod)
gratia::draw(mod)
gratia::appraise(mod)
