#########
# Australian data
#  formatting
#
#########

# I have been provided two datasets of Helicovpera armigera and punctigera population dynamics from 1966 to 2015
# This code formats them into on dataframe and writes it as 'data/processed/combined_dat.csv'


rm(list=ls())

library(tidyverse)
library(lubridate)
library(readxl)

# Data given to me by Myron Zalucki
#####

# HELIC011

dat <- read.csv("data/raw/AUS_dat/myron_data/helic011_catch.csv",header=FALSE) %>% as_tibble() %>%
  rename(Trap = V1, Date = V2, H_arm = V3, H_punc = V4, Catch_status = V5) %>%
  filter(Catch_status != 'x') %>% select(!Catch_status) %>%
  mutate(Trap = factor(Trap), Date = as.Date(Date)) %>%
  mutate(dataset = "helic011",trap_type = "BL_240_V")


#HELIC015

dat2 <- read.csv("data/raw/AUS_dat/myron_data/helic015_catch.csv",header=FALSE) %>% as_tibble() %>%
  rename(Season_code = V1, Trap = V2, Date = V3, Num_nights = V4, H_arm_f = V5,H_arm_m = V6,H_punc_f = V7,H_punc_m = V8) %>%
  mutate(Season_code = factor(Season_code), Trap = factor(Trap), Date = as.Date(Date)) %>%
  mutate(dataset = "helic015", H_arm = H_arm_f + H_arm_m, H_punc = H_punc_f + H_punc_m,trap_type = "BL_12_V") %>%
  select(2:3,9:12)


# lets combined the data

combined_dat <- rbind(dat,dat2)


#####

# Data provided by Geoff Baker
#####


#This data was provided to me in an ugly excel format (yikes!) lets get it into a manageable format that is
# computer friendly without touching the raw datasheet.

#I am assuming that the week column is a week starting on sunday


punc <- read_excel("data/raw/AUS_dat/ACRI Pheromone Trapping Grid data (23 years).xlsx",skip=3)[2:57,1:24] %>%
  pivot_longer(cols=c(!Week),names_to = "years",values_to = "mean_count") %>%
  separate(col=years,sep = "-",into=c("before","after")) %>%
  mutate(before = as.integer(before),
         after = before + 1,
         Species = "punc",
         dataset = "Baker")



arm <- read_excel("data/raw/AUS_dat/ACRI Pheromone Trapping Grid data (23 years).xlsx",skip=3)[60:115,1:24] %>%
pivot_longer(cols=c(!Week),names_to = "years",values_to = "mean_count") %>%
  separate(col=years,sep = "-",into=c("before","after")) %>%
  mutate(before = as.integer(before),
         after = before + 1,
         Species = "arm",
         dataset = "Baker")


Baker_dat <- rbind(punc,arm) %>%
  mutate(Date = as.Date(paste0(before,"-",Week,"-","1"), "%Y-%U-%u"),
         mean_count = as.numeric(mean_count),Week = as.integer(Week))

write.csv(Baker_dat,file="data/processed/AUS_data/Baker_dat.csv")

