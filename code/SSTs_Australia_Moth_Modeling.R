########
# Preliminary
#  Modeling with
#    SSTs
######

#Objective: to correlate moth populations with large scale sea surface temperature anamolies
# Three in mind:
#   ENSO: La Niña brings rain to australia
#   Indian Dipole: + phase reduces the chance of rain, - phase increases the chance of rain.
#   Southern Oscillation Index:
#    Positive phase summer: increased precipitation
#    Negative phase summer: decreased precipitation 
#    Positive phase winter: mixed increase and decrease rainfall
#    Negative phase winter: increased rainfall
#    

library(tidyverse)
library(lubridate)

url_noaa <- "https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/dmi.had.long.data" 
MARSHALL_SAM_URL = 'http://www.nerc-bas.ac.uk/public/icd/gjma/newsam.1957.2007.txt'


load_sam_marshall <- function(source_url = MARSHALL_SAM_URL) {
  sam_data <- utils::read.csv(source_url, sep = '', skip = 2, stringsAsFactors = FALSE)
  
  # The data is in the format 'Year, Jan, Feb, ..., Dec', so rename the columns with integers to be used for melt
  colnames(sam_data) <- c('year', 1 : 12)
  
  sam_data <- reshape2::melt(sam_data, id.vars = 'year', variable.name = 'month')
  sam_data$year <- strtoi(sam_data$year)
  sam_data$month <- strtoi(sam_data$month)
  sam_data <- sam_data[!is.na(sam_data$value), ]
  sam_data <- sam_data[order(sam_data$year, sam_data$month), ]
  rownames(sam_data) <- 1 : nrow(sam_data)
  
  return(sam_data)
}
load_iod_noaa <- function(source_url = url_noaa) {
  iod_data_lines <- readLines(source_url)
  iod_data_lines[2 : (which(iod_data_lines == '-9999') - 1)]
  iod_data_content <- paste0(
    iod_data_lines[2 : (which(iod_data_lines == '-9999') - 1)],
    collapse = '\n'
  )
  iod_data_raw <- read.table(textConnection(iod_data_content))
  
  years <- iod_data_raw[, 1]
  subset(data.frame(
    year = rep(years, each = 12),
    month = rep(1 : 12, length(years)),
    dmi = as.vector(as.matrix(iod_data_raw[, -1]))
  ), dmi != -9999)
}
load_soi_bom <- function(source_url = BOM_SOI_URL) {
  soi_html <- xml2::read_html(source_url)
  
  `%>%` <- rvest::`%>%`
  soi_pre <- soi_html %>% rvest::html_node('pre') %>% rvest::html_text()
  
  soi_data <- utils::read.csv(textConnection(soi_pre), sep = '', stringsAsFactors=FALSE)
  # The data is in the format 'Year, Jan, Feb, ..., Dec', so rename the columns with integers to be used for melt
  colnames(soi_data) <- c('year', 1 : 12)
  
  soi_data <- reshape2::melt(soi_data, id.vars = 'year', variable.name = 'month')
  soi_data$year <- strtoi(soi_data$year)
  soi_data$month <- strtoi(soi_data$month)
  
  soi_data <- soi_data[!is.na(soi_data$value), ]
  soi_data <- soi_data[order(soi_data$year, soi_data$month), ]
  
  return(soi_data)
}


library(rsoi)

MEI <- rsoi::download_mei() %>%
  select(Year,Date,MEI,Phase) %>%
  mutate(Month = month(Date)) %>%
  select(!Date)

SAM <- load_sam_marshall() %>% as_tibble() %>%
  rename(SAM = value)
IOD <- load_iod_noaa() %>% as_tibble() %>%
  rename(IOD = dmi)


SSTs <- MEI %>% select(Year,Month,MEI) %>%
  rename(year= Year, month = Month) %>%
  full_join(SAM,by=c("year","month"))  %>%
  full_join(IOD,by=c("year","month"))

SSTs2 <- SSTs %>%
  mutate(season = case_when(
    month >= 9 & month <= 11 ~ "Spring",
    month >= 3 & month <= 5 ~ "Autumn",
    month >= 6 & month <= 8 ~ "Winter",
    TRUE ~ "Summer"
  )) 

SSTs_yearly <- SSTs2 %>% group_by(year) %>%
  summarize(MEI = mean(MEI),SAM = mean(SAM),IOD=mean(IOD))

#Aussie seasons:
#Spring - the three transition months September (9), October (10) and November (11).
#Summer - the three hottest months December (12), January (1) and February (2).
#Autumn - the transition months March (3), April (4) and May (5).
#Winter - the three coldest months June (6), July (7) and August (8).

# Now lets bring in the moth data starting with Myrons stuff:

dat <- read.csv("data/processed/AUS_data/myron_data.csv") %>% as_tibble()


dat2 <- dat %>% mutate(Date = as.Date(Date), year=year(Date)) %>%
  left_join(SSTs2,by="year")

dat_yearly <- dat %>% mutate(Date = as.Date(Date), year=year(Date)) %>%
  left_join(SSTs_yearly,by="year")

ggplot(dat_yearly,aes(x=IOD,y=H_arm)) + geom_smooth(method="lm")




