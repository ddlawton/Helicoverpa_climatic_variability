####
# Visualizing trap differences
#
####
rm(list=ls())

library(data.table)
library(tidyverse)
library(lubridate)


dat <- as_tibble(fread("data/raw/dat_with_GS_Jan5.csv"))

dat2 <- dat %>% filter(zone_30y == 2) %>% group_by(location,year) %>%
  summarize(cew_year = sum(CEW_sum), Longitude = first(longitude),Latitude=first(latitude), trap_type = first(trap_type),num_obs = n())  %>%
  filter(num_obs > 5)

length(unique(dat2$location))



dat2 %>% group_by(year) %>%
  summarize(n_traps = length(location), n_obs = sum(nu_obs))


long_term_traps <- dat2 %>% group_by(location) %>%
  summarize(min = min(year), max = max(year), n_obs = sum(num_obs)) %>% filter(min < 2005) %>% filter(max >2015) %>% droplevels()

locs <- long_term_traps$location


dat2 %>% filter(location %in% locs) %>% droplevels() %>%
  ggplot(aes(x=year,y=cew_year,color=location)) + geom_point() + geom_line() + viridis::scale_colour_viridis(discrete=TRUE,option="A")

dat2 %>% 
  ggplot(aes(x=cew_year)) + geom_histogram()


percentiles <- dat2 %>% 
  group_by(year) %>%  
  summarise(enframe(quantile(cew_year, c(0.25, 0.50, 0.75)), "quantile", "cew_year")) %>% pivot_wider(names_from = "quantile",values_from = cew_year)

dat3 <- dat2 %>% left_join(percentiles,by="year") %>%
  mutate(percentile = case_when(
    cew_year < `25%` ~ "0-25",
    cew_year >= `25%` & cew_year < `50%` ~ "25-50",
    cew_year >= `50%` & cew_year < `75%` ~ "50-75",
    cew_year >= `75%` ~ "75-100",
  ))

view(dat3 %>% group_by(location,percentile) %>%
  summarize(n=length(cew_year)) %>% pivot_wider(names_from = percentile,values_from = n))

dat4 <- dat3 %>% group_by(location,percentile) %>%
  summarize(n=length(cew_year)) 

ggplot(dat4,aes(x=location,y=n,fill=percentile))+ geom_col(position="stack")



test <- ggplot(dat2,aes(x=year,y=cew_year,color=location)) + geom_point() + geom_line() + viridis::scale_color_viridis(discrete = TRUE) + theme(legend.position="none")
ggsave(test,file="output/test.png",width=20,height=20,dpi=600)

