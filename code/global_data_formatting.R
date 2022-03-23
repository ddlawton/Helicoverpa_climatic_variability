#########
# Getting the data
#   Into Global format
#    
#
##########

rm(list=ls())
library(tidyverse)
library(lubridate)
library(sf)
library(rnaturalearth)
library(ggridges)
library(janitor)

us_dat <- read_csv("data/raw/US_dat/Hzea_dat.csv")
Baker_dat <- read_csv("data/processed/AUS_data/Baker_dat.csv")
myron_dat <- read_csv("data/processed/AUS_data/myron_data.csv")
sa_dat <- read_csv("data/processed/SA_dat/combined_sa_dat.csv")


se <- function(x) sqrt(var(x)/length(x))

# combining all data together
 
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
  
AUS_dat <- rbind(b_cleaned,m_cleaned) %>% mutate(continent = "Australia")

US_dat2 <- us_dat %>%
  select(woy,year,CEW_sum,location,longitude,latitude)%>%
  rename(Week = woy, Year = year,count = CEW_sum, Trap = location,Latitude=latitude,Longitude=longitude) %>%
  mutate(Species = "H_zea", dataset="US")  %>% mutate(continent = "North_America")


sa_dat2 <- sa_dat %>%
  select(year,h_arm,h_zea,date,Latitude,Longitude,Trap_ID) %>%
  pivot_longer(cols=c(starts_with("h_")),names_to = "Species",values_to = "Count") %>%
  mutate(year = year(date), Week = isoweek(date), dataset="SA", continent = "South_America",
         Species = factor(Species),Trap = (Trap_ID)) %>%
  select(!date) %>%
  group_by(year,Week,Trap,Species) %>%
  summarise(Count = sum(Count,na.rm=TRUE), Longitude = first(Longitude), Latitude = first(Latitude), dataset = first(dataset), continent = first(continent)) %>%
  clean_names()
  



dat <- rbind(AUS_dat,US_dat2) %>% 
  clean_names() %>%
  rbind(sa_dat2) %>%
  mutate(
  species = case_when(
    species == "arm" ~ "H_arm",
    species == "punc" ~ "H_punc",
    species == "h_arm" ~ "H_arm",
    species == "h_punc" ~ "H_punc",
    species == "h_zea" ~ "H_zea",
    TRUE ~ species
  )
) 


unique(factor(dat$species))

unique(factor(dat$continent))


# Plotting to ensure all the points are on land


world <- ne_countries() %>% st_as_sf()

ggplot(data=world) +
  geom_sf(aes(geometry=geometry)) +
  geom_point(data=dat,aes(x=longitude,y=latitude),pch=21,size=0.75) +
  theme_void()

#Everything looks fine. The south american dataset will be added shortly.


# Summarizing to the year

yearly_dat <- dat %>%
  group_by(year,trap,species) %>%
  summarize(`mean count` = mean(count),median_count = median(count),se_count = se(count),Latitude = first(latitude),Longitude = first(longitude), continent = first(continent)) %>%
  group_by(year) %>%
  mutate(`Number of yearly traps` = length(unique(trap)))


summary(yearly_dat)

yearly_dat %>%
  mutate(species = factor(species)) %>%
  ungroup() %>%
  select(year,species,`mean count`,`Number of yearly traps`) %>%
  pivot_longer(cols=c(!species)) %>%
  ggplot(aes(x=value,y=species)) +
  geom_density_ridges(alpha=0.5) +
  facet_wrap(~name,scales = "free") +
  ggpubr::theme_pubr()

#Lets do some quick data exploration

dat %>%
  mutate(species = factor(species),
         logged_count = log1p(count)) %>%
  select(week,year,species,count,logged_count) %>%
  pivot_longer(cols=c(!species)) %>%
  ggplot(aes(x=value,y=species)) +
  geom_density_ridges() +
  facet_wrap(~name,scales = "free") +
  ggpubr::theme_pubr()




Heli.pnts = SpatialPointsDataFrame(yearly_dat[,c("Longitude","Latitude")], yearly_dat) %>% st_as_sf()
plot(Heli.pnts)

dat_sf <- yearly_dat %>%
  st_as_sf(.,coords=c("Longitude","Latitude"),crs=4326)


view(dat_sf %>% filter(continent == "South_America"))

dat_sf2 <- dat_sf %>%
  mutate(before_year = year - 1,
        time_end = paste0(year,"-09-","30"),
         time_start  = paste0(before_year,"-02-","01")) %>%
  filter(before_year >= 1981) %>%
  select(!before_year) 
summary(dat_sf2)


st_write(dat_sf2, "data/processed/ugly_shapefile/Heliocoverpa_data.shp")


write.csv(yearly_dat,"data/processed/US_helicoverpa_data.csv", row.names=FALSE)

yearly_dat2 <- (yearly_dat %>% filter(continent != "Australia"))
summary()
write.csv(yearly_dat2,"data/processed/US_helicoverpa_data.csv", row.names=FALSE)
