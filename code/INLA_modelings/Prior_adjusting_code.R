#Main modeling package
library(INLA)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(patchwork)
library(data.table)


rm(list=ls())

Heli.df = fread("data/processed/INLA_data_mesh_20220426.csv", 
                header = TRUE, sep=",") %>% as_tibble() %>%
  select(trap,season,mean_count,se_count,species,type,year,Latitude,Longitude,Air_temp,Precip,num_yearly_traps,num_yearly_trap_obs) %>%
  pivot_wider(names_from = season,
              values_from = c(Air_temp, Precip)) %>%
  filter(type=="data") %>%
  mutate(Latitude2 = Longitude,
         Longitude2 = Latitude,
         intercept.1 = 1,
         species_rep1 = as.integer(as.factor(species)),
         species_rep2 = as.integer(as.factor(species)),
         range.air_off = inla.group(Air_temp_off_season, n = 50, method = "quantile"),
         range.precip_off = inla.group(Precip_off_season, n = 50, method = "quantile"),
         mean_count = round(mean_count))#filtering out all the mesh data just to look at the actual data


       
    
#Model formula
f1 = mean_count  ~ -1 + intercept.1 + # the -1 species we use a custom intercept (fixed effect)
  f(range.air_off,model="rw1",
    constr=TRUE,
    scale.model=TRUE,
    replicate = species_rep1,
    hyper = list(theta = list(prior="pc.prec", param=c(0.01,0.01))))


f2 = mean_count ~ -1 + intercept.1 + # the -1 species we use a custom intercept (fixed effect)
  f(range.air_off,
    model="rw1",
    constr=TRUE,
    scale.model=TRUE,
    replicate = species_rep1,
    hyper = list(theta = list(prior="pc.prec", param=c(0.05,0.05))))

f3 = mean_count ~ -1 + intercept.1 + # the -1 species we use a custom intercept (fixed effect)
  f(range.air_off,
    model="rw1",
    constr=TRUE,
    scale.model=TRUE,
    replicate = species_rep1,
    hyper = list(theta = list(prior="pc.prec", param=c(1,0.9))))



m1 = inla(f1, 
          data = Heli.df, 
          family = "zeroinflatedpoisson0", 
          #verbose = TRUE,
          control.predictor = list(compute = TRUE, link = 1),
          control.mode = list(restart = TRUE),
          control.compute=list(dic = TRUE, waic = TRUE)) 


m2 = inla(f2, 
          data = Heli.df, 
          family = "zeroinflatedpoisson0", 
          #verbose = TRUE,
          control.predictor = list(compute = TRUE, link = 1),
          control.mode = list(restart = TRUE),
          control.compute=list(dic = TRUE, waic = TRUE)) 


m3 = inla(f3, 
          data = Heli.df, 
          family = "zeroinflatedpoisson0", 
          #verbose = TRUE,
          control.predictor = list(compute = TRUE, link = 1),
          control.mode = list(restart = TRUE),
          control.compute=list(dic = TRUE, waic = TRUE)) 


Indx.length = length(unique(m1$summary.random$range.air_off$ID))

M1_Results = m1$summary.random$range.air_off[,c(1:4,6)] %>%
  mutate(Species = rep(c("H_arm", "H_punc", "H_zea"), each = Indx.length), model = "m1")

M2_Results = m2$summary.random$range.air_off[,c(1:4,6)] %>%
  mutate(Species = rep(c("H_arm", "H_punc", "H_zea"), each = Indx.length), model = "m2")


M3_Results = m2$summary.random$range.air_off[,c(1:4,6)] %>%
  mutate(Species = rep(c("H_arm", "H_punc", "H_zea"), each = Indx.length), model = "m3")

air_temp <- M1_Results %>% rbind(M2_Results,M3_Results)




air_temp %>%
  ggplot(aes(ID, mean, color=model)) +
  geom_point() +
  geom_smooth() + 
  facet_wrap(~Species + model)




