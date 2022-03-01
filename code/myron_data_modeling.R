#####
# Australian
#  Myron Data
#   Modeling
##

rm(list=ls())

library(tidyverse)
library(lubridate)
library(mgcv)
library(gratia)
library(DHARMa)
library(anytime)
library(itsadug)

dat <- read.csv("data/processed/AUS_data/myron_data.csv") %>% as_tibble() %>%
  mutate(Trap = factor(Trap), Date = as.Date(Date),dataset = factor(dataset),trap_type = factor(trap_type),
         DOY = yday(Date), WOY = isoweek(Date), Year = year(Date),date_numeric = as.numeric(Date), months = month(Date)) %>%
  pivot_longer(cols=c(H_arm,H_punc),names_to = "Species", values_to = "Count") %>% mutate(Species=factor(Species)) %>% drop_na(Count) 

summary((anytime(dat$date_numeric)))


view(dat %>% group_by(Trap,trap_type) %>%
  summarize(n=n()) %>%
  pivot_wider(names_from = trap_type,values_from = n)
)

dat_arm <- dat %>% filter(Species == "H_arm")
dat_punc <- dat %>% filter(Species == "H_punc")






mod <- bam(Count ~ s(date_numeric,by="Species",bs="tp",k=25) + s(DOY,by="Species",bs="cc",k=100) + s(Trap,bs="re"), 
           data=dat,select=TRUE,family = nb(),discrete = TRUE,nthreads=23)


mod_tw <- bam(Count ~ 
                s(date_numeric,by=Species,bs="tp",k=800) + 
                s(DOY,by=Species,bs="cc",k=100) +
                s(Trap,bs="re"), 
           data=dat,select=TRUE,family = tw(),discrete = TRUE,nthreads=23)b


mod_tw_fs <- bam(Count ~ 
                te(Date,Species,bs="fs",m=1,k=250) + 
                te(months ,Species,bs="fs",m=1,k=12) +
                s(Trap,bs="re"), 
              data=dat,select=TRUE,family = tw(),discrete = TRUE,nthreads=23)

AIC(mod_tw,mod_tw_fs)
k.check(mod_tw_fs)
summary(mod_tw)
k.check(mod_tw)
resid_sim <- simulateResiduals(mod)
plot(resid_sim)
draw(mod)

# default ACF function:
acf(resid(mod_tw_fs), main="acf(resid(mod_tw_fs))")
acf_resid(mod_tw_fs)

b0 <- coef(mod_tw)[1]

test <- gratia::smooth_estimates(mod_tw)

smooths <- c("s(date_numeric):SpeciesH_arm","s(date_numeric):SpeciesH_punc")

date_species <- test %>% filter(smooth %in% smooths) %>%
  ggplot(aes(x=(anytime(date_numeric)))) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=smooth),alpha=.1) +
  geom_line(aes(y = est,color=smooth)) + ggpubr::theme_pubr() +
  geom_smooth() +
  scale_color_manual(values = c("#1b9e77","#d95f02")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02")) # + ggtitle("date by treatment smooth") + xlab("Date")


unique(levels(factor(test$smooth)))
test <- gratia::smooth_estimates(mod_tw)

smooths <- c("s(DOY):SpeciesH_arm","s(DOY):SpeciesH_punc")

doy_specie <- test %>% filter(smooth %in% smooths) %>%
  ggplot(aes(x=((DOY)))) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=smooth),alpha=.1) +
  geom_line(aes(y = est,color=smooth)) + ggpubr::theme_pubr() +
  scale_color_manual(values = c("#1b9e77","#d95f02")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02")) # + ggtitle("date by treatment smooth") + xlab("Date")






dat %>% filter(Species == "H_punc") %>% summarize(min=min(Date))



b0 <- coef(mod_tw_fs)[1]

test2 <- gratia::smooth_estimates(mod_tw_fs)

H_arm <- test2 %>% filter(smooth == "te(date_numeric,Species)") %>%
  filter(Species == "H_arm") %>%
  #filter(date_numeric >= 1000) %>%
  ggplot(aes(x=as.POSIXct(date_numeric, origin = "1970-01-01"))) +# geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=Species),alpha=.1) +
  geom_line(aes(y = est),size=.45) + ggpubr::theme_pubr() +
  geom_smooth(aes(y = est),size=1.25,se=FALSE,method="gam",formula = y~s(x,bs="tp",k=30)) +
  ggtitle("Helicoverpa armigera") +
  scale_x_continuous(limits=c(-4000,12000),breaks = c(-4000,0,4000,8000,12000),labels = c(1960,1970,1980,1990,2000)) +
  ylab("Partial Residuals") + xlab("")

H_punc <- test2 %>% filter(smooth == "te(date_numeric,Species)") %>%
  filter(Species == "H_punc") %>%
  #filter(date_numeric >= 1000) %>%
  ggplot(aes(x=((date_numeric)))) +# geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=Species),alpha=.1) +
  geom_vline(xintercept = 800,color="red",linetype=2) +
  geom_line(aes(y = est),size=.45) + ggpubr::theme_pubr() +
  geom_smooth(aes(y = est),size=1.25,se=FALSE,method="gam",formula = y~s(x,bs="tp",k=30)) +
  ggtitle("Helicoverpa punctigera") +
  scale_x_continuous(limits=c(-4000,12000),breaks = c(-4000,0,4000,8000,12000),labels = c(1960,1970,1980,1990,2000)) +
  ylab("Partial Residuals") + xlab("Date")

H_arm / H_punc



doy_specie <- test2 %>% filter(smooth == "te(DOY,Species)") %>%
  ggplot(aes(x=((DOY)))) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=Species),alpha=.1) +
  geom_line(aes(y = est,color=Species)) + ggpubr::theme_pubr() +
  scale_color_manual(values = c("#1b9e77","#d95f02")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02")) # + ggtitle("date by treatment smooth") + xlab("Date")



month_specie <- test2 %>% filter(smooth == "te(months,Species)") %>%
  ggplot(aes(x=((months )))) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=Species),alpha=.1) +
  geom_line(aes(y = est,color=Species)) + ggpubr::theme_pubr() +
  scale_color_manual(values = c("#1b9e77","#d95f02")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02")) # + ggtitle("date by treatment smooth") + xlab("Date")





m1 <- gamm(Count ~  te(date_numeric,Species,bs="fs",k=50) + 
             te(DOY,Species,bs="fs",k=50) + 
             te(months ,Species,bs="fs",k=12) +
             s(Trap,bs="re"),
             data = dat, select=TRUE, correlation = corARMA(form = ~ 1|Year, p = 1))
summary(m1$gam)

yup <- smooth_estimates(m1$gam)


date_species <- yup %>% filter(smooth == "te(date_numeric,Species)") %>%
  #filter(date_numeric >= 1000) %>%
  ggplot(aes(x=((date_numeric)))) + #geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=Species),alpha=.1) +
  geom_line(aes(y = est,color=Species),size=1.25) + ggpubr::theme_pubr() +
  scale_color_manual(values = c("#1b9e77","#d95f02")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02")) # + ggtitle("date by treatment smooth") + xlab("Date")

layout(matrix(1:2, ncol = 2)
res <- resid(m1$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(2) errors")
pacf(res, lag.max = 36, main = "pACF- AR(2) errors")
layout(1)
