####
# Global modeling
#
##

rm(list=ls())

library(tidyverse)
library(lubridate)
library(ggridges)
library(viridis)
library(ggpubr)
library(patchwork)
library(mgcv)
library(gratia)
library(DHARMa)

us_dat <- read_csv("data/raw/US_dat/Hzea_dat.csv")
Baker_dat <- read_csv("data/processed/AUS_data/Baker_dat.csv")
myron_dat <- read_csv("data/processed/AUS_data/myron_data.csv")

b_cleaned <- Baker_dat %>% 
  drop_na(mean_count) %>%
  select(Week,before,mean_count,Species,dataset) %>%
  rename(Year = before, count=mean_count) %>%
  mutate(Latitude = -30.206009806160633, Longitude = 149.59603761497965, Trap="Baker_dat")

m_cleaned<- myron_dat %>%
  mutate(Week = isoweek(Date),Year = year(Date)) %>%
  pivot_longer(cols=c(H_arm,H_punc),names_to = "Species",values_to = "count") %>%
  drop_na(count) %>%
  select(dataset,Latitude,Longitude,Week,Year,Species,count,Trap)

AUS_dat <- rbind(b_cleaned,m_cleaned) 

US_dat2 <- us_dat %>%
  select(woy,year,CEW_sum,location,longitude,latitude)%>%
  rename(Week = woy, Year = year,count = CEW_sum, Trap = location,Latitude=latitude,Longitude=longitude) %>%
  mutate(Species = "H_zea", dataset="US")

dat <- rbind(AUS_dat,US_dat2) %>% mutate(
  Species = factor(case_when(
    Species == "arm" ~ "H_arm",
    Species == "punc" ~ "H_punc",
    TRUE ~ Species
  )),
  Trap = factor(Trap)
) 


dat2 <- dat %>% filter(Year >= 1990)


mod_0 <- bam(count ~ 1,data=dat2,family=tw(),
             discrete = TRUE,nthreads = 23)

mod <- bam(count ~ 
             te(Week,Species,bs=c("cc","re"),k=20) + 
             te(Year,Species,bs="fs",k=20) + 
             s(Trap,bs="re") + 
             te(Longitude,Latitude,bs=c("gp","gp")),data=dat2,family=tw(),
           discrete = TRUE,nthreads = 23, select=TRUE)

mod_by <- bam(count ~ 
                te(Week,by=Species,bs="cc",k=20) + 
                te(Year,by=Species,bs="tp",k=20) + 
                s(Trap,bs="re") + 
                te(Longitude,Latitude,bs=c("gp","gp")),data=dat2,family=tw(),
              discrete = TRUE,nthreads = 23, select=TRUE)

AIC(mod,mod_by)


summary(mod)


summary(mod_by)

simresid <- simulateResiduals(mod_by)
plot(simresid)
draw(mod)
k.check(mod)

output <- smooth_estimates(mod)

woy <- output %>%
  filter(smooth == "te(Week,Species)") %>%
  ggplot(aes(x=Week,group=Species)) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=Species),alpha=.1) +
  geom_line(aes(y = est, color=Species),size=1.5) + theme_pubr()  +
  scale_color_manual(values=c("#7e2b19", "#7dd9a0", "#285d28")) +
  scale_fill_manual(values=c("#7e2b19", "#7dd9a0", "#285d28"))

year <- output %>%
  filter(smooth == "te(Year,Species)") %>%
  ggplot(aes(x=Year,group=Species)) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=Species),alpha=.1) +
  geom_line(aes(y = est, color=Species),size=1.5) + theme_pubr()  +
  scale_color_manual(values=c("#7e2b19", "#7dd9a0", "#285d28")) +
  scale_fill_manual(values=c("#7e2b19", "#7dd9a0", "#285d28"))


woy_year <- woy + year + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')


output_by <- smooth_estimates(mod_by)

woy <- output_by %>%
  filter(smooth %in% c("te(Week):SpeciesH_arm","te(Week):SpeciesH_punc")) %>%
  ggplot(aes(x=((Week + 24) %% 52),group=smooth)) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=smooth),alpha=.1) +
  geom_line(aes(y = est, color=smooth),size=1.5) + theme_pubr()  +
  scale_color_manual(values=c("#7e2b19", "#7dd9a0", "#285d28")) +
  scale_fill_manual(values=c("#7e2b19", "#7dd9a0", "#285d28"))


year <- output_by %>%
  filter(smooth %in% c("te(Year):SpeciesH_arm","te(Year):SpeciesH_punc")) %>%
  ggplot(aes(x=Year,group=smooth)) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=smooth),alpha=.1) +
  geom_line(aes(y = est, color=smooth),size=1.5) + theme_pubr()   +
  scale_color_manual(values=c("#7e2b19", "#7dd9a0", "#285d28")) +
  scale_fill_manual(values=c("#7e2b19", "#7dd9a0", "#285d28"))


woy_year2 <- woy + year + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')


woy_year / woy_year2
