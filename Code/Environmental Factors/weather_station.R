library(readxl)
library(tidyverse)
df <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/weather_station_precip.xlsx",
col_types = c("date", "numeric"))
df$month <- month(df$DATE)
df$year <- year(df$DATE)
precip <- df %>% 
  group_by(month,year) %>% 
  summarize(mean_precip =mean(df$`COVM PP`),sum_precip = sum(df$`COVM PP`))
precip
