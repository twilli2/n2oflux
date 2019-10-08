library(tidyverse)
library(readr)
library(lubridate)
weather <- read_csv("~/Downloads/1485341.csv")

weather <- as_tibble(weather)
weather$DATE <- as.Date(weather$DATE)
weather <- rename(weather, precip = DAILYPrecip)
weather <- select(weather, DATE, precip)
weather$year <- year(weather$DATE)
weather$year <- as.factor(weather$year)
weather$month <- month(weather$DATE, label = T) 
weather$day <- day(weather$DATE)
weather$precip <- as.double(weather$precip)
weather
weather %>%
  group_by(year,month) %>% 
  filter(month == "Aug" | month == "Sep"| month == 'Oct') %>% 
  summarize(tot_precip = sum(precip, na.rm = T)) %>% 
  View()

weather %>%
  filter(year == '2018', month == 'Sep') %>% 
  group_by(year,month) %>%
  summarize(tot_precip = sum(precip, na.rm = T)) %>% 
  View()

wd <- weather %>%
  group_by(DATE,month,year) %>% 
  summarize(tot_precip = sum(precip, na.rm = T))
ggplot(wd) +
  geom_line(aes(x = DATE, y = tot_precip, color = year))
weather


  