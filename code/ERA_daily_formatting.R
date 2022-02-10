###
# Daily ERA 
#  formatting
#
####

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

'%!in%' <- function(x,y)!('%in%'(x,y))

dat <- fread("data/processed/ZEA_ERA_DAILY_SOIL_MOISTURE.csv") %>% as_tibble() %>%
  select(!`system:index`) %>% select(!.geo)


dat2 <- cSplit(setDT(dat)[, lapply(.SD, gsub, pattern = "[][]", 
                          replacement = "")], names(dat), sep=",", fixed = FALSE, "long")
names(dat2)


seasons <- c("Fall","Winter","Spring")

dat3 <- dat2 %>% 
  tidyr::fill(ID,Latitude,Longitude,cew_year,location,num_obs,time_end,time_start,trap_type,year,.direction = "down") %>%
  drop_na("Precip") %>% as_tibble() %>%
  mutate(Precip = Precip * 1000,
         Precip_hourly = Precip_hourly * 1000,
         dates = anytime::anytime(dates/1000),
         day = yday(dates),
         season = case_when(
           day >= 265 & day <= 354 ~ "Fall",
           day >= 355 ~ "Winter",
           day >= 1 & day <= 78~ "Winter",
           day >= 79 & day <= 171 ~ "Spring"),
         season = factor(season,levels=seasons),
         soil_temp_1 = soil_temp_1 - 273.15,
         soil_temp_2 = soil_temp_2 - 273.15,
         avg_soil_temp = ((soil_temp_1) + (soil_temp_2)) / 2,
         avg_soil_moist = ((soil_moist_1) + (soil_moist_2)) / 2,
         Air_temp = (Air_temp)-273.15) %>% drop_na(season) %>%
  select(1:10,15:22)

names(dat3)
ggplot(dat3,aes(x=Precip,y=Precip_hourly)) + geom_smooth()
?mean

Precip <- ggplot(dat3,aes(x=Precip,y=cew_year)) + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 25)) +
  geom_smooth(method = "lm") +
  facet_grid(~season) + theme_pubr()

Air_temp <- ggplot(dat3,aes(x=Air_temp,y=cew_year)) + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 25)) +
  geom_smooth(method = "lm") +
  facet_grid(~season) + theme_pubr()

avg_soil_temp <- ggplot(dat3,aes(x=avg_soil_temp,y=cew_year)) + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 25)) +
  geom_smooth(method = "lm") +
  facet_grid(~season) + theme_pubr()

avg_soil_moist <- ggplot(dat3,aes(x=avg_soil_moist,y=cew_year)) + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 25)) +
  geom_smooth(method = "lm") +
  facet_grid(~season) + theme_pubr()

all <- Precip / Air_temp / avg_soil_temp / avg_soil_moist

ggsave(all,file="output/climatic_smooths.png",height=15,width=15,units="in",dpi=600)

dat3 %>% select(cew_year,num_obs,Air_temp,Precip,avg_soil_temp,avg_soil_moist) %>%
  pivot_longer(cols = everything(),names_to = "variable",values_to = "values") %>%
  ggplot(aes(x=values)) + geom_histogram() + facet_wrap(~variable,scales="free") +
  theme_pubr()

dat4 <- dat3 %>% group_by(location,year,season) %>%
  summarise(cew_year = first(cew_year),Air_temp = mean(Air_temp),Precip = mean(Precip),
            avg_soil_temp = mean(avg_soil_temp), avg_soil_moist = mean(avg_soil_moist), num_obs = first(num_obs))

write.csv(dat3,file="data/processed/daily_dat_Jan282022.csv")

write.csv(dat4,file="data/processed/seasonal_averaged_dat_Jan282022.csv")


