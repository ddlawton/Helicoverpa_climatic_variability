####
# Exploring 
#  The Australian
#   Data
#####

library(tidyverse)
library(lubridate)
library(ggridges)
library(viridis)
library(ggpubr)

rm(list=ls())

readcsv <- function(direct) {
  dat <- read.csv(direct) %>% as_tibble()
  return(dat)
}

Myron_data <- readcsv("data/processed/AUS_data/myron_data.csv") %>%
  mutate(Trap = factor(Trap), Date = as.Date(Date), dataset= factor(dataset), trap_type = factor(trap_type))

Baker_data <- readcsv("data/processed/AUS_data/Baker_dat.csv") %>%
  mutate(Species = factor(Species), Date = as.Date(Date), dataset= factor(dataset))

#Myron Data

trap_years_operated <- Myron_data %>% group_by(Trap) %>%
  summarize(min = min(Date),mean=mean(Date), max=max(Date),years_operated=as.numeric(difftime(max,min,units="days"))/365) %>%
  dplyr::arrange(desc(years_operated))


trap_years_operated_graph <- trap_years_operated %>% select(1,5) %>%
  ggplot(aes(x=reorder(Trap, -years_operated),y=years_operated)) + geom_col() +
  theme_pubr() + 
  theme(axis.text.x = element_text(angle = 45))
trap_years_operated_graph


trap_years_operated <- Myron_data %>% group_by(Trap) %>%
  summarize(min = min(Date),mean=mean(Date), max=max(Date),years_operated=as.numeric(difftime(max,min,units="days"))/365) %>%
  dplyr::arrange(desc(years_operated))

Myron_data %>%
  group_by(Trap) %>%
  summarize(Date_min = min(Date,na.rm = TRUE),Date_mean = mean(Date,na.rm = TRUE),Date_max = max(Date,na.rm = TRUE), year_range = as.numeric(difftime(Date_max,Date_min,units="auto")/365),
            punc_count_min = min(H_punc,na.rm = TRUE), punc_count_mean = mean(H_punc,na.rm = TRUE),punc_count_max = max(H_punc,na.rm = TRUE),
            arm_count_min = min(H_arm,na.rm = TRUE), arm_count_mean = mean(H_arm,na.rm = TRUE),arm_count_max = max(H_arm,na.rm = TRUE))  %>%
  dplyr::arrange(desc(year_range))

view(Myron_data %>%
  mutate(year = year(Date)) %>%
  group_by(Trap,year) %>%
  summarise(H_arm_mean = mean(H_arm),H_punc = mean(H_punc), num_obs = length(.),Latitude = first(Latitude), Longitude = first(Longitude)))


#Baker Data

ridgelines <- Baker_data %>% 
  mutate(logged_mean = log1p(mean_count)) %>%
  select(mean_count,logged_mean,Species) %>%
  pivot_longer(cols=!Species,names_to = "variables",values_to = "values") %>%
  ggplot(aes(x=values,y=Species)) + geom_density_ridges() +
  facet_wrap(~variables,scales="free") +
  theme_pubr()
ridgelines

Baker_data %>%
  group_by(Species) %>%
  summarize(Date_min = min(Date,na.rm = TRUE),Date_mean = mean(Date,na.rm = TRUE),Date_max = max(Date,na.rm = TRUE), year_range = as.numeric(difftime(Date_max,Date_min,units="auto")/365),
            Count_min = min(mean_count,na.rm = TRUE), Count_mean = mean(mean_count,na.rm = TRUE),Count_max = max(mean_count,na.rm = TRUE))

Baker_data %>%
  group_by(before,Species) %>%
  summarize(Count_min = min(mean_count,na.rm = TRUE), Count_mean = mean(mean_count,na.rm = TRUE),Count_max = max(mean_count,na.rm = TRUE)) %>%
  ggplot(aes(x=(before),y=Count_mean,color=Species)) + geom_point() +
  geom_line() +
  theme_pubr()
