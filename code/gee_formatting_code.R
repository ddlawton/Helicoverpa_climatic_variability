####
# Formatting data 
#  into GEE friendly format
####

library(data.table)
library(tidyverse)
library(lubridate)

std <- function(x) sd(x)/sqrt(length(x))

dat <- as_tibble(fread("data/raw/dat_with_GS_Jan5.csv"))

dat2 <- dat %>% filter(zone_30y == 2) %>% group_by(location,year) %>%
  summarize(cew_year = sum(CEW_sum), Longitude = first(longitude),Latitude=first(latitude), trap_type = first(trap_type),num_obs = n()) %>%
  mutate(time_end = parse_date_time(paste0(year,"-","06","-","21"," ","23:59:59"), orders="ymd HMS"),
         time_end = as.numeric(time_end) * 1000,
         time_start = (parse_date_time(paste0(year,"-","09","-","22"," ","00:00:00"), orders="ymd HMS") - years(1)),
         time_start = as.numeric(time_start) * 1000,
         longitude = Longitude, latitude = Latitude) %>% rowid_to_column(var="ID")

min(dat2$year)
ggplot(dat2,aes(x=cew_year)) + geom_histogram()
ggplot(dat2,aes(x=year)) + geom_histogram()

write.csv(dat2,file="data/processed/GEE_ready_data.csv")            



