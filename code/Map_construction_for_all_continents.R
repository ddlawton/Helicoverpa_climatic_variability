###
# GIF visualization
#  of Moth counts
####
rm(list=ls())

library(tidyverse)
library(gganimate)
library(rnaturalearth)
library(lubridate)
library(sf)
library(ggpubr)
library(ggsn)
library(tmaptools)
library(spatialEco)

#loading in all the data, moths focusing on Australia first


dat_yearly <- as_tibble(read.csv("data/processed/AUS_data/myron_data.csv")) %>% 
  select(!trap_type) %>%
  mutate(year = year(as.Date(Date))) %>%
  group_by(Trap,year) %>%
  summarize(H_arm_mean = mean(H_arm,rm.na=TRUE), H_punc_mean = mean(H_punc,rm.na=TRUE), Latitude = first(Latitude), Longitude = first(Longitude)) %>%
  st_as_sf(.,coords=c("Longitude","Latitude"),crs=4326)



dat_weekly <- as_tibble(read.csv("data/processed/AUS_data/myron_data.csv")) %>% 
  select(!trap_type) %>%
  mutate(year = year(as.Date(Date)), woy = isoweek(as.Date(Date))) %>%
  group_by(Trap,year,woy) %>%
  summarize(H_arm_mean = mean(H_arm,rm.na=TRUE), H_punc_mean = mean(H_punc,rm.na=TRUE), Latitude = first(Latitude), Longitude = first(Longitude)) %>%
  st_as_sf(.,coords=c("Longitude","Latitude"),crs=4326)

AUS_outline <- ne_countries(country="Australia") %>% st_as_sf() # %>% as_tibble() %>% select(geometry)
AUS <- ne_states(country="Australia")  %>% st_as_sf() %>% as_tibble() %>% select(geometry)
dat2 <- dat_yearly %>%
  group_by(Trap,year) %>%
  mutate(Helio = sum(H_arm_mean, H_punc_mean,na.rm=TRUE))

dat3 <- dat_weekly %>%
  group_by(Trap,woy) %>%
  summarize(Helio = mean(sum(H_arm_mean, H_punc_mean,na.rm=TRUE),na.rm=TRUE))

p1 <- ggplot(data=AUS) + 
  geom_sf(aes(geometry=geometry)) + 
  geom_sf(data=dat_yearly,aes(geometry=geometry)) +
  theme_void() +
  #ggsn::scalebar(x.min = 110, x.max = 155, 
  #               y.min = -45, y.max = -9, 
  #               dist = 500, transform = TRUE, 
  #               model = "WGS84", height = 0.05, 
  #               st.color = "black",dist_unit="km",location= "bottomleft",st.bottom	=FALSE,st.dist=0.05) +
  #north(x.min = 110, x.max = 155, 
  #      y.min = -45, y.max = -9) +
  scale_x_continuous(limits=c(110,155),breaks = c(120,130,140,150)) +
  scale_y_continuous(limits=c(-45,-9),breaks = c(-40,-30,-20,-10)) +
  theme(panel.background = element_rect(fill = '#9ac6d7')) + ylab("Latitude") + xlab("Longitude")

p1

?label
yearly_plot <- ggplot(data=AUS) + 
  geom_sf(aes(geometry=geometry)) + 
  geom_sf(data=dat2,aes(geometry=geometry,size=log1p(Helio))) +
  scale_x_continuous(breaks = c(120,130,140,150)) +
  scale_y_continuous(limits=c(-45,-9),breaks = c(-40,-30,-20,-10)) + 
  theme_void() +
  scale_size_continuous(guide = guide_legend(direction = "horizontal", title.position = "top",
                                             label.position="bottom", label.hjust = 0.5, label.vjust = 0.5)) +
  theme(legend.position=c(0.25,0.20)) +
  #ggsn::scalebar(x.min = 110, x.max = 155, 
  #               y.min = -45, y.max = -9, 
  #               dist = 500, transform = TRUE, 
  #               model = "WGS84", height = 0.05, 
  #               st.color = "black",dist_unit="km",location= "bottomleft",st.bottom	=FALSE,st.dist=0.05) +
  #north(x.min = 110, x.max = 155, 
  #      y.min = -45, y.max = -9) +
  theme(panel.background = element_rect(fill = '#9ac6d7')) + 
  guides(size = "none") +
  theme(legend.position = c(0.3, 0.1)) +
  transition_manual(year) +
  labs(title = 'Logged helicoverpa catches during {current_frame} in Australia') +
  theme(text = element_text(size=30),legend.title = element_blank())

weekly_plot <- ggplot(data=AUS) + 
  geom_sf(aes(geometry=geometry)) + 
  geom_sf(data=dat3,aes(geometry=geometry,size=log1p(Helio)))+
  scale_x_continuous(breaks = c(120,130,140,150)) +
  scale_y_continuous(limits=c(-45,-9),breaks = c(-40,-30,-20,-10)) + 
  theme_void() +
  scale_size_continuous(guide = guide_legend(direction = "horizontal", title.position = "top",
                                             label.position="bottom", label.hjust = 0.5, label.vjust = 0.5)) +
  theme(legend.position=c(0.25,0.20)) +
  #ggsn::scalebar(x.min = 110, x.max = 155, 
  #               y.min = -45, y.max = -9, 
  #               dist = 500, transform = TRUE, 
  #               model = "WGS84", height = 0.05, 
  #               st.color = "black",dist_unit="km",location= "bottomleft",st.bottom	=FALSE,st.dist=0.05) +
  #north(x.min = 110, x.max = 155, 
  #      y.min = -45, y.max = -9) +
  theme(panel.background = element_rect(fill = '#9ac6d7')) + 
  transition_manual(woy) +
  labs(title = 'Logged helicoverpa catches for week of year {current_frame} in Australia') +
  theme(text = element_text(size=30),legend.title = element_blank())

plot_ratio <- get_asp_ratio(AUS_outline)
ggsave('p1.png', plot=p1, width = plot_ratio*5, height=5,dpi=600)


anim_save("output/maps/yearly_Australia_Helio_catches.gif",yearly_plot, height = 960, width=960)
anim_save("output/maps/weekly_Australia_Helio_catches.gif",weekly_plot, height = 960, width=960)
ggsave(p1,file="output/maps/Australia_trap_locations.png",dpi=600,width = 10,height=10,units="in")


#Now North America

US_yearly <- as_tibble(read.csv("data/raw/US_dat/Hzea_dat.csv")) %>% 
  select(!c(trap_type,contact,state,zone_30y,date)) %>%
  group_by(location,year) %>%
  summarize(CEW_mean = mean(CEW_sum,rm.na=TRUE), Latitude = first(latitude), Longitude = first(longitude)) %>%
  st_as_sf(.,coords=c("Longitude","Latitude"),crs=4326)

US_weekly <- as_tibble(read.csv("data/raw/US_dat/Hzea_dat.csv")) %>% 
  select(!c(trap_type,contact,state,zone_30y,date)) %>%
  group_by(location,woy) %>%
  summarize(CEW_mean = mean(CEW_sum,rm.na=TRUE), Latitude = first(latitude), Longitude = first(longitude)) %>%
  st_as_sf(.,coords=c("Longitude","Latitude"),crs=4326)

states <- state.name


US_out <- ne_countries(country="United States of America") %>% st_as_sf() %>% select(geometry)
CA_out <- ne_countries(country="Canada") %>%  st_as_sf() %>% select(geometry)
NA_outline <- rbind(US_out, CA_out)
plot(NA_outline)

US_out <- ne_states(country="United States of America") %>% st_as_sf() %>% 
  filter(name %in% state.name) %>% filter(name != "Hawaii") %>% select(geometry)

MX_out <- ne_states(country="Mexico") %>% st_as_sf()  %>% select(geometry)


CA_out <- ne_states(country="Canada") %>%  st_as_sf() %>% filter(name != "Prince Edward Island") %>% select(geometry)


NA_map <- rbind(US_out, CA_out, MX_out)

test_map <- st_crop(NA_map,xmin=-123,xmax = -62,ymin=25,ymax=50)


yearly_filter <- st_intersection(US_yearly,NA_map)
weekly_filter <- st_intersection(US_weekly,NA_map)


p1 <- ggplot(data=test_map) + 
  geom_sf(aes(geometry=geometry)) + 
  geom_sf(data=yearly_filter,aes(geometry=geometry),size=0.65) +
  theme_void() +
  #scale_x_continuous(limits=c(-123,-62),breaks = c(-120,-95,-70)) +
  #scale_y_continuous(limits=c(25,50),breaks = c(30,40,50)) +
  theme(panel.background = element_rect(fill = '#9ac6d7')) + ylab("Latitude") + xlab("Longitude")

p1


yearly_plot <- ggplot(data=NA_map) + 
  geom_sf(aes(geometry=geometry)) + 
  geom_sf(data=yearly_filter,aes(geometry=geometry,size=log1p(CEW_mean),fill=log1p(CEW_mean)),pch=21) +
  scale_x_continuous(limits=c(-123,-62),breaks = c(-120,-95,-70)) +
  scale_y_continuous(limits=c(25,50),breaks = c(30,40,50)) +
  theme_void() +
  scale_size_continuous(guide = guide_legend(direction = "horizontal", title.position = "top",
                                             label.position="bottom", label.hjust = 0.5, label.vjust = 0.5)) +
  viridis::scale_fill_viridis(guide = guide_legend(direction = "horizontal", title.position = "top",
                                             label.position="bottom", label.hjust = 0.5, label.vjust = 0.5)) +
  theme(legend.position=c(0.85,0.20)) +
  
  #ggsn::scalebar(x.min = 110, x.max = 155, 
  #               y.min = -45, y.max = -9, 
  #               dist = 500, transform = TRUE, 
  #               model = "WGS84", height = 0.05, 
  #               st.color = "black",dist_unit="km",location= "bottomleft",st.bottom	=FALSE,st.dist=0.05) +
  #north(x.min = 110, x.max = 155, 
  #      y.min = -45, y.max = -9) +
  theme(panel.background = element_rect(fill = '#9ac6d7')) + 
  transition_manual(year) +
  labs(title = 'Logged Helicoverpa zea catches during {current_frame} in United States') +
  theme(text = element_text(size=20),legend.title = element_blank())

weekly_plot <- ggplot(data=NA_map) + 
  geom_sf(aes(geometry=geometry)) + 
  geom_sf(data=weekly_filter,aes(geometry=geometry,size=log1p(CEW_mean),fill=log1p(CEW_mean)),pch=21)+
  scale_x_continuous(limits=c(-123,-62),breaks = c(-120,-95,-70)) +
  scale_y_continuous(limits=c(25,50),breaks = c(30,40,50)) + 
  theme_void() +
  scale_size_continuous(guide = guide_legend(direction = "horizontal", title.position = "top",
                                             label.position="bottom", label.hjust = 0.5, label.vjust = 0.5)) +
  viridis::scale_fill_viridis(guide = guide_legend(direction = "horizontal", title.position = "top",
                                                   label.position="bottom", label.hjust = 0.5, label.vjust = 0.5)) +
  theme(legend.position=c(0.85,0.20)) +
  #ggsn::scalebar(x.min = 110, x.max = 155, 
  #               y.min = -45, y.max = -9, 
  #               dist = 500, transform = TRUE, 
  #               model = "WGS84", height = 0.05, 
  #               st.color = "black",dist_unit="km",location= "bottomleft",st.bottom	=FALSE,st.dist=0.05) +
  #north(x.min = 110, x.max = 155, 
  #      y.min = -45, y.max = -9) +
  theme(panel.background = element_rect(fill = '#9ac6d7')) + 
  transition_manual(woy) +
  labs(title = 'Logged Helicoverpa catches for week of year {current_frame} in United States') +
  theme(text = element_text(size=25),legend.title = element_blank())

plot_ratio <- get_asp_ratio(NA_outline)
ggsave('p1.png', plot=p1, width = plot_ratio*5, height=5,dpi=600)


anim_save("output/maps/yearly_US_Helio_catches.gif",yearly_plot, height = 613.33, width=1150)
anim_save("output/maps/weekly_US_Helio_catches.gif",weekly_plot, height = 613.33, width=1150)
ggsave(p1,file="output/maps/Australia_trap_locations.png",dpi=600,width = 10,height=10,units="in")

