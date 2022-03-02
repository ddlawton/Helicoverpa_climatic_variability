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
library(patchwork)
library(mgcv)
library(gratia)
library(DHARMa)

rm(list=ls())


Myron_data <- read_csv("data/processed/AUS_data/myron_data.csv") %>%
  mutate(Trap = factor(Trap), Date = as.Date(Date), dataset= factor(dataset), trap_type = factor(trap_type),
         Date = as.Date(Date), year = year(Date), woy = isoweek(Date))

Baker_data <- read_csv("data/processed/AUS_data/Baker_dat.csv") %>%
  mutate(Species = factor(Species), Date = as.Date(Date), dataset= factor(dataset))


levels(unique(Baker_data$Species))


b_cleaned <- Baker_data %>% 
  drop_na(mean_count) %>%
  select(Week,before,mean_count,Species,dataset) %>%
  rename(Year = before, count=mean_count) %>%
  mutate(Latitude = -30.206009806160633, Longitude = 149.59603761497965, Trap="Baker_dat")

m_cleaned<- Myron_data %>%
  mutate(Week = isoweek(Date),Year = year(Date)) %>%
  pivot_longer(cols=c(H_arm,H_punc),names_to = "Species",values_to = "count") %>%
  drop_na(count) %>%
  select(dataset,Latitude,Longitude,Week,Year,Species,count,Trap)

AUS_dat <- rbind(b_cleaned,m_cleaned) %>%
  mutate(Trap = factor(Trap),
         Species = factor(case_when(
           Species == "arm" ~ "H_arm",
           Species == "punc" ~ "H_punc",
           TRUE ~ as.character(Species)
         )))


#Myron Data

trap_years_operated <- Myron_data %>% group_by(Trap) %>%
  summarize(min = min(Date),mean=mean(Date), max=max(Date),years_operated=as.numeric(difftime(max,min,units="days"))/365) %>%
  dplyr::arrange(desc(years_operated))


trap_years_operated_graph <- trap_years_operated %>% select(1,5) %>%
  ggplot(aes(x=reorder(Trap, -years_operated),y=years_operated)) + geom_col() +
  theme_pubr() + 
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Years operated") +
  xlab("Traps")
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



catch_mean_year <- Myron_data %>%
  pivot_longer(cols=(starts_with("H_")),names_to = "Species",values_to = "count") %>%
  group_by(year,Species) %>%
  summarize(Count_mean = mean(count,na.rm = TRUE)) %>%
  ggplot(aes(x=(year),y=Count_mean,color=Species)) + geom_point() +
  scale_color_manual(values = c("#68affc", "#26496d"), labels = c("H. armigera", "H. punctigera")) +
  geom_smooth(se=FALSE) +
  theme_pubr() + ylab("Moth yearly mean catch")


catch_median_year <- Myron_data %>%
  pivot_longer(cols=(starts_with("H_")),names_to = "Species",values_to = "count") %>%
  group_by(year,Species) %>%
  summarize(Count_mean = median(count,na.rm = TRUE)) %>%
  ggplot(aes(x=(year),y=Count_mean,color=Species)) + geom_point() +
  geom_smooth(se=FALSE) +
  scale_color_manual(values = c("#68affc", "#26496d"), labels = c("H. armigera", "H. punctigera")) +
  theme_pubr() + ylab("Moth yearly median catch")

catch_sum_year <- Myron_data %>%
  pivot_longer(cols=(starts_with("H_")),names_to = "Species",values_to = "count") %>%
  group_by(year,Species) %>%
  summarize(Count_mean = sum(count,na.rm = TRUE)) %>%
  ggplot(aes(x=(year),y=Count_mean,color=Species)) + geom_point() +
  geom_smooth(se=FALSE) +
  scale_color_manual(values = c("#68affc", "#26496d"), labels = c("H. armigera", "H. punctigera")) +
  theme_pubr() + ylab("Moth yearly sum catch")

layout <- "
AB
C#
"

catch_mean_year + catch_median_year + catch_sum_year + plot_layout(guides = 'collect',design = layout)  & theme(legend.position = 'bottom')



catch_sum_adj <- Myron_data %>%
  pivot_longer(cols=(starts_with("H_")),names_to = "Species",values_to = "count") %>%
  group_by(woy,Species) %>%
  summarize(Count_mean = sum(count,na.rm = TRUE)) %>%
  ggplot(aes(x=(((woy + 24) %% 52)),y=Count_mean,color=Species)) + geom_point() +
  geom_smooth(se=FALSE,method="gam", 
              formula = y ~ s(x, bs = "cc", fx = TRUE, k = 50)) +
  theme_pubr() + ylab("Moth yearly sum catch")  +
  scale_color_manual(values=c("#1c4c5e", "#5ba9b8"))

 
catch_sum <- Myron_data %>%
  pivot_longer(cols=(starts_with("H_")),names_to = "Species",values_to = "count") %>%
  group_by(woy,Species) %>%
  summarize(Count_mean = sum(count,na.rm = TRUE)) %>%
  ggplot(aes(x=woy,y=Count_mean,color=Species)) + geom_point() +
  geom_smooth(se=FALSE,method="gam", 
              formula = y ~ s(x, bs = "cc", fx = TRUE, k = 50)) +
  theme_pubr() + ylab("Moth yearly sum catch") +
  scale_color_manual(values=c("#1c4c5e", "#5ba9b8"))

#layout <- "
#ABC
#DEF
#"

catch_sum_adj +  catch_sum + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')


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
  summarize(mean=mean(mean_count,na.rm=TRUE), median = median(mean_count,na.rm=TRUE))

Histogram_punc <- Baker_data %>% 
  filter(Species == "punc") %>%
  select(mean_count,Species) %>%
  ggplot(aes(x=mean_count)) + geom_histogram() +
  geom_vline(xintercept = 14.9,linetype=2,color="red") +
  geom_vline(xintercept = 8.59,linetype=2,color="orange") +
  theme_pubr() + ggtitle("Helicoverpa punctigera")

Histogram_arm <- Baker_data %>% 
  filter(Species == "arm") %>%
  select(mean_count,Species) %>%
  ggplot(aes(x=mean_count)) + geom_histogram() +
  geom_vline(xintercept = 18.9,linetype=2,color="red") +
  geom_vline(xintercept = 1,linetype=2,color="orange") +
  theme_pubr() + ggtitle("Helicoverpa armigera")

Histogram_punc + Histogram_arm + plot_annotation(
  title = 'Count distribution for both species',
  subtitle = 'Red line is the mean while orange line is the median'
)

Baker_data %>%
  group_by(Species) %>%
  summarize(Date_min = min(Date,na.rm = TRUE),Date_mean = mean(Date,na.rm = TRUE),Date_max = max(Date,na.rm = TRUE), year_range = as.numeric(difftime(Date_max,Date_min,units="auto")/365),
            Count_min = min(mean_count,na.rm = TRUE), Count_mean = mean(mean_count,na.rm = TRUE),Count_max = max(mean_count,na.rm = TRUE))





catch_mean <- Baker_data %>%
  group_by(before,Species) %>%
  summarize(Count_min = min(mean_count,na.rm = TRUE), Count_mean = mean(mean_count,na.rm = TRUE),Count_max = max(mean_count,na.rm = TRUE)) %>%
  ggplot(aes(x=(before),y=Count_mean,color=Species)) + geom_point() +
  geom_smooth(se=FALSE) +
  theme_pubr() + ylab("Moth yearly mean catch")


catch_median <- Baker_data %>%
  group_by(before,Species) %>%
  summarize(Count_min = min(mean_count,na.rm = TRUE), Count_mean = median(mean_count,na.rm = TRUE),Count_max = max(mean_count,na.rm = TRUE)) %>%
  ggplot(aes(x=(before),y=Count_mean,color=Species)) + geom_point() +
  geom_smooth(se=FALSE) +
  theme_pubr() + ylab("Moth yearly median catch")

catch_sum <- Baker_data %>%
  group_by(before,Species) %>%
  summarize(Count_min = min(mean_count,na.rm = TRUE), Count_mean = sum(mean_count,na.rm = TRUE),Count_max = max(mean_count,na.rm = TRUE)) %>%
  ggplot(aes(x=(before),y=Count_mean,color=Species)) + geom_point() +
  geom_smooth(se=FALSE) +
  theme_pubr() + ylab("Moth yearly sum catch")

layout <- "
AB
C#
"

catch_mean + catch_median + catch_sum + plot_layout(guides = 'collect',design = layout)


### Now some exploratory modeling with just the Australian data


model_dat <- Myron_data %>%
  pivot_longer(cols=(starts_with("H_")),names_to = "Species",values_to = "count") %>%
  mutate(Species = factor(Species),
         adj_woy = (woy + 24) %% 52) %>% drop_na(count)
summary(model_dat)

AUS_dat2 <- AUS_dat %>% filter(Year >= 1980)


mod_0 <- gam(count ~ 1,data=AUS_dat2,family=tw())

mod <- bam(count ~ 
             te(Week,Species,bs=c("cc","re"),k=20) + 
             te(Year,Species,bs="fs",k=20) + 
             s(Trap,bs="re") + 
             te(Longitude,Latitude,bs=c("gp","gp")),data=AUS_dat2,family=tw(),
           discrete = TRUE,nthreads = 23, select=TRUE)

mod_by <- bam(count ~ 
                te(Week,by=Species,bs="cc",k=20) + 
                te(Year,by=Species,bs="tp",k=20) + 
                s(Trap,bs="re") + 
                te(Longitude,Latitude,bs=c("gp","gp")),data=AUS_dat2,family=tw(),
              discrete = TRUE,nthreads = 23, select=TRUE)

AIC(mod_0,mod,mod_by)


summary(mod)


summary(mod_by)

simresid <- simulateResiduals(mod_by)
plot(simresid)
draw(mod)
k.check(mod)

output <- smooth_estimates(mod)

woy <- output %>%
  filter(smooth == "te(Week,Species)") %>%
  ggplot(aes(x=((Week + 24) %% 52),group=Species)) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=Species),alpha=.1) +
  geom_line(aes(y = est, color=Species),size=1.5) + theme_pubr()  +
  scale_color_manual(values=c("#1c4c5e", "#5ba9b8")) +
  scale_fill_manual(values=c("#1c4c5e", "#5ba9b8"))

year <- output %>%
  filter(smooth == "te(Year,Species)") %>%
  ggplot(aes(x=Year,group=Species)) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=Species),alpha=.1) +
  geom_line(aes(y = est, color=Species),size=1.5) + theme_pubr()  +
  scale_color_manual(values=c("#1c4c5e", "#5ba9b8")) +
  scale_fill_manual(values=c("#1c4c5e", "#5ba9b8"))

woy_year <- woy + year + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')


output_by <- smooth_estimates(mod_by)

woy <- output_by %>%
  filter(smooth %in% c("te(Week):SpeciesH_arm","te(Week):SpeciesH_punc")) %>%
  ggplot(aes(x=((Week + 24) %% 52),group=smooth)) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=smooth),alpha=.1) +
  geom_line(aes(y = est, color=smooth),size=1.5) + theme_pubr()  +
  scale_color_manual(values=c("#1c4c5e", "#5ba9b8"), labels = c("Helicoverpa armigera","Helicoverpa punctigera")) +
  scale_fill_manual(values=c("#1c4c5e", "#5ba9b8"), labels = c("Helicoverpa armigera","Helicoverpa punctigera"))

year <- output_by %>%
  filter(smooth %in% c("te(Year):SpeciesH_arm","te(Year):SpeciesH_punc")) %>%
  ggplot(aes(x=Year,group=smooth)) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=smooth),alpha=.1) +
  geom_line(aes(y = est, color=smooth),size=1.5) + theme_pubr()   +
  scale_color_manual(values=c("#1c4c5e", "#5ba9b8"), labels = c("Helicoverpa armigera","Helicoverpa punctigera")) +
  scale_fill_manual(values=c("#1c4c5e", "#5ba9b8"), labels = c("Helicoverpa armigera","Helicoverpa punctigera"))

woy_year2 <- woy + year + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')


woy_year / woy_year2

