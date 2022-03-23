#########
# South America data
#  formatting
#
#########

# I have been provided  datasets of Helicovpera armigera and zea population dynamics from 1966 to 2015
# This code formats them into on dataframe and writes it as 'data/processed/combined_dat.csv'

rm(list=ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(sp)
library(MetBrewer)
library(patchwork)
library(anytime)

files <- list.files("data/raw/SA_dat", full.names = TRUE)
files2 <- gsub("\\~","",files)
files3 <- gsub("\\$","",files2)

files4 <- unique(gtools::mixedsort(files3))









dat <- read_excel("data/raw/SA_dat/Helicoverpa Uberaba and Chapadão - Minas Gerais  and Planaltina - Distrito Federal.xlsx",skip=2) %>%
  clean_names() %>%
  fill(month,.direction = "down") %>%
  cbind(lat.long)


dat_list <- list()
overall_dat_list <- list()

for (x in files4) {
  tryCatch({
    #cat("processing", x, "\n")
    #Sys.sleep(0.01)
    #flush.console()
    
    sheetcount <- length(excel_sheets(x))
    
    for (i in 1:sheetcount){
      
      lat.long <- read_excel(x,sheet=i,.name_repair	="minimal")[1,] %>% 
        clean_names() %>%
        select(1:3) %>%
        rename(Latitude = 1,
               Longitude = 2,
               Trap_ID = 3)

      dat <- read_excel(x,sheet=i,skip=2,.name_repair	="minimal") %>%
        clean_names() %>%
        fill(month,.direction = "down") %>%
        cbind(lat.long) %>%
        mutate(data = as.character(data))
      dat_list[[i]] <- dat
    }
    
    overall_dat_list[[x]] <- dat_list
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


dat <- bind_rows(overall_dat_list, .id = "column_label")  %>% as_tibble() %>%
  mutate(data2 = case_when(
    str_detect(data, '4') ~ 'fix',
    TRUE ~ "fine")
  )


datfix <- dat %>% filter(data2 == "fix") %>%
  mutate(data = as.character(as.Date(as.integer(data), origin = "1899-12-30")))

dat_cleaned <- dat %>% filter(data2 != "fix") %>%
  rbind(datfix) %>%
  select(!c(10:13)) %>%
  drop_na(Latitude) %>%
  mutate(Latitude = as.numeric(char2dms(Latitude, chd = "°", chm = "'")),
         Longitude = as.numeric(char2dms(Longitude, chd = "°", chm = "'"))) %>%
  drop_na(data) %>% filter(data != "na") %>%
  mutate(date = as.Date(data), year = year(date)) %>%
  select(!c(data2,data)) 

# adding a few datasheets that are organized differently



lat.long1 <- read_excel("data/raw/SA_dat/new_data/Helicoverpa MT.xlsx",.name_repair	="minimal")[1,1:4] %>% 
  clean_names() %>%
  select(1:3) %>%
  rename(Latitude = 1,
         Longitude = 2,
         Trap_ID = 3)

dat1 <- read_excel("data/raw/SA_dat/new_data/Helicoverpa MT.xlsx",skip=2,.name_repair	="minimal")[,1:4] %>%
  clean_names() %>%
  fill(month,.direction = "down") %>%
  cbind(lat.long) %>%
  mutate(data = as.character(data))


lat.long2 <- read_excel("data/raw/SA_dat/new_data/Helicoverpa MT.xlsx",.name_repair	="minimal")[1,6:9] %>% 
  clean_names() %>%
  select(1:3) %>%
  rename(Latitude = 1,
         Longitude = 2,
         Trap_ID = 3)

dat2 <- read_excel("data/raw/SA_dat/new_data/Helicoverpa MT.xlsx",skip=2,.name_repair	="minimal")[,6:9] %>%
  clean_names() %>%
  fill(month,.direction = "down") %>%
  cbind(lat.long2) %>%
  mutate(data = as.character(data))


lat.long3 <- read_excel("data/raw/SA_dat/new_data/Helicoverpa MT.xlsx",.name_repair	="minimal")[1,11:14] %>% 
  clean_names() %>%
  select(1:3) %>%
  rename(Latitude = 1,
         Longitude = 2,
         Trap_ID = 3)

dat3 <- read_excel("data/raw/SA_dat/new_data/Helicoverpa MT.xlsx",skip=2,.name_repair	="minimal")[,11:14] %>%
  clean_names() %>%
  fill(month,.direction = "down") %>%
  cbind(lat.long3) %>%
  mutate(data = as.character(data))

lat.long4 <- read_excel("data/raw/SA_dat/new_data/Helicoverpa MT.xlsx",.name_repair	="minimal")[1,16:19] %>% 
  clean_names() %>%
  select(1:3) %>%
  rename(Latitude = 1,
         Longitude = 2,
         Trap_ID = 3)

dat4 <- read_excel("data/raw/SA_dat/new_data/Helicoverpa MT.xlsx",skip=2,.name_repair	="minimal")[,16:19] %>%
  clean_names() %>%
  fill(month,.direction = "down") %>%
  cbind(lat.long4) %>%
  mutate(data = as.character(data))

lat.long5 <- read_excel("data/raw/SA_dat/new_data/Helicoverpa MT.xlsx",.name_repair	="minimal")[1,21:24] %>% 
  clean_names() %>%
  select(1:3) %>%
  rename(Latitude = 1,
         Longitude = 2,
         Trap_ID = 3)

dat5 <- read_excel("data/raw/SA_dat/new_data/Helicoverpa MT.xlsx",skip=2,.name_repair	="minimal")[,21:24] %>%
  clean_names() %>%
  fill(month,.direction = "down") %>%
  cbind(lat.long5) %>%
  mutate(data = as.character(data))


lat.long6 <- read_excel("data/raw/SA_dat/new_data/Helicoverpa MT.xlsx",.name_repair	="minimal")[1,26:29] %>% 
  clean_names() %>%
  select(1:3) %>%
  rename(Latitude = 1,
         Longitude = 2,
         Trap_ID = 3)

dat6 <- read_excel("data/raw/SA_dat/new_data/Helicoverpa MT.xlsx",skip=2,.name_repair	="minimal")[,26:29] %>%
  clean_names() %>%
  fill(month,.direction = "down") %>%
  cbind(lat.long6) %>%
  mutate(data = as.character(data))

new_dat <- rbind(dat2,dat3,dat4,dat5,dat6) %>% as_tibble() %>%
  mutate(date = as.Date(data), year = year(date), column_label = "new_data") %>%
  select(!data) %>%
  drop_na(Latitude) %>%
  mutate(Latitude = as.numeric(char2dms(Latitude, chd = "°", chm = "'")),
         Longitude = as.numeric(char2dms(Longitude, chd = "°", chm = "'")))




combined_dat <- dat_cleaned %>% select(!collection) %>% rbind(new_dat)





MetBrewer::colorblind_palettes


combined_dat %>%
  pivot_longer(col=c("h_arm","h_zea")) %>%
  ggplot(aes(x=date,y=value+1,color=name)) +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "tp", fx = TRUE, k = 10)) +
  geom_point() +
  #geom_rug(sides="b") + 
  scale_y_continuous(trans = "log10") +
  scale_color_met_d("Java") +
  ggpubr::theme_pubr() +
  ylab("Logged moth catches") +
  xlab("Date")

combined_dat %>%
  pivot_longer(col=c("h_arm","h_zea")) %>%
  mutate(woy = isoweek(date)) %>%
  ggplot(aes(x=woy,y=value+1,color=name)) +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cc", fx = TRUE, k = 10)) +
  geom_point() +
  #geom_rug(sides="b") + 
  scale_y_continuous(trans = "log10") +
  scale_color_met_d("Java") +
  ggpubr::theme_pubr() +
  ylab("Logged moth catches") +
  xlab("Week of year")

#everything looks okay, lets write.csv it out and put it with the overall dataset.

write.csv(combined_dat,file='data/processed/SA_dat/combined_sa_dat.csv')
