###
# Australian data
#  formatting
#
####
rm(list=ls())

library(tidyverse)
library(lubridate)

### Data given to me by Myron Zuckli

# HELIC011

dat <- read.csv("data/raw/AUS_dat/myron_data/helic011_catch.csv",header=FALSE) %>% as_tibble() %>%
  rename(Trap = V1, Date = V2, H_arm = V3, H_punc = V4, Catch_status = V5) %>%
  filter(Catch_status != 'x') %>% select(!Catch_status) %>%
  mutate(Trap = factor(Trap), Date = as.Date(Date)) %>%
  mutate(dataset = "helic011",trap_type = "BL_240_V")

dat %>%
pivot_longer(cols=c(H_arm,H_punc),names_to = "Species", values_to = "Count") %>%
ggplot(aes(x=Date,y=Count,color=Species)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50)) +
  geom_point()

#HELIC015

dat2 <- read.csv("data/raw/AUS_dat/myron_data/helic015_catch.csv",header=FALSE) %>% as_tibble() %>%
  rename(Season_code = V1, Trap = V2, Date = V3, Num_nights = V4, H_arm_f = V5,H_arm_m = V6,H_punc_f = V7,H_punc_m = V8) %>%
  mutate(Season_code = factor(Season_code), Trap = factor(Trap), Date = as.Date(Date)) %>%
  mutate(dataset = "helic015", H_arm = H_arm_f + H_arm_m, H_punc = H_punc_f + H_punc_m,trap_type = "BL_12_V") %>%
  select(2:3,9:12)

dat2 %>%
  pivot_longer(cols=c(H_arm,H_punc),names_to = "Species", values_to = "Count") %>%
  ggplot(aes(x=Date,y=Count,color=Species)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50)) +
  geom_point()


# lets combined the data

combined_dat <- rbind(dat,dat2)
unique(levels(factor(combined_dat$Trap)))


write.csv(combined_dat,file="data/processed/AUS_data/myron_data.csv")




combined_dat %>%
  rename(`H. armigera` = H_arm, `H. punctigera` = H_punc) %>%
  mutate(year = year(Date)) %>%
  #filter(year >1975) %>%
  pivot_longer(cols=c(`H. armigera`,`H. punctigera`),names_to = "Species",values_to = "Count") %>%
ggplot(aes(x=Date,y=Count)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 25)) +
  coord_cartesian(ylim=c(0,NA)) + facet_wrap(~Species,scales="free") + ggpubr::theme_pubr()

