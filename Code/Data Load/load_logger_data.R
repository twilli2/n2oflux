library(readr)
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
data <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42760 13Jun18-0925-Field 1.xls",
sheet = "Em50 \"EM42760\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
"numeric", "skip", "numeric", "numeric",
"numeric", "skip", "numeric", "numeric",
"numeric"))
data$field <- 1               
data2 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42760 15Feb18-1027 Field 1.xls",
sheet = "Em50 \"EM42760\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
"numeric", "skip", "numeric", "numeric",
"numeric", "skip", "numeric", "numeric",
"numeric"))
data2$field <- 1
data3 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42760 29Mar18-1019 Field 1.xls",
sheet = "Em50 \"EM42760\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
"numeric", "skip", "numeric", "numeric",
"numeric", "skip", "numeric", "numeric",
"numeric"))
data3$field <- 1
data4 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42763 13Jun18-1155 Field 2.xls",
  sheet = "Em50 \"EM42763\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
  "numeric", "skip", "numeric", "numeric",
  "numeric", "skip", "numeric", "numeric",
  "numeric"))
data4$field <- 2
data5 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42763 14Feb18-1235 Field 2.xls",
  sheet = "Em50 \"EM42763\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
  "numeric", "skip", "numeric", "numeric",
  "numeric", "skip", "numeric", "numeric",
  "numeric"))
data5$field <- 2

data6 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42993 13Jun18-1341 Field 3.xls",
                    sheet = "Em50 \"EM42993\" Data", skip = 3, col_names = c('date_time','moist_10cm','temp_10cm','cond_10cm','moist_5cm','temp_5cm','cond_5cm'), 
                    col_types = c("date","skip", "skip", "numeric", "numeric", "numeric", "skip", "numeric", "numeric","numeric"))
data6$field <- 3

data7 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM43097 29Mar18-1254 Field 4.xls",
  sheet = "Em50 \"EM43097\" Data", skip = 3, 
  col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), 
  col_types = c("date",
  "numeric", "skip", "numeric", "numeric",
  "numeric", "numeric", "numeric",
  "numeric"))
data7$field <- 4

data8 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM43097 14Feb18-1409 Field 4.xls",
  sheet = "Em50 \"EM43097\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
  "numeric", "skip", "numeric", "numeric",
  "numeric", "numeric", "numeric",
  "numeric"))
data8$field <- 4

data9 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM43097 13Jun18-1455 Field 4.xls",
  sheet = "Em50 \"EM43097\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
  "numeric", "skip", "numeric", "numeric",
  "numeric", "numeric", "numeric",
  "numeric"))
data9$field <- 4

data10 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42760 7Oct18-1400-F1.xls",
sheet = "Em50 \"EM42760\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
"numeric", "skip", "numeric", "numeric",
"numeric", "skip", "numeric", "numeric",
"numeric"))
data10$field <- 1

data11 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42763 7Oct18-0953 - F2.xls",
sheet = "Em50 \"EM42763\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
"numeric", "skip", "numeric", "numeric",
"numeric", "skip", "numeric", "numeric",
"numeric"))
data11$field <- 2

data12 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42993 7Oct18-1101-F3.xls",
                    sheet = "Em50 \"EM42993\" Data", skip = 3, col_names = c('date_time','moist_10cm','temp_10cm','cond_10cm','moist_5cm','temp_5cm','cond_5cm'), 
                    col_types = c("date","skip", "skip", "numeric", "numeric", "numeric", "skip", "numeric", "numeric","numeric"))
data12$field <- 3

data13 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM43097 7Oct18-1226-F4.xls",
  sheet = "Em50 \"EM43097\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
  "numeric", "skip", "numeric", "numeric",
  "numeric", "numeric", "numeric",
  "numeric"))
data13$field <- 4

logger_data <- bind_rows(data,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12,data13)

#format logger data
logger_data$field <- as.factor(logger_data$field)
logger_data$month <- month(logger_data$date_time,label = T)
logger_data$date <- date(logger_data$date_time)
logger_data

precip <- logger_data %>% 
  group_by(field, date) %>% 
  summarise(total_precip = sum(precip, na.rm = T))

max_temp_5 <- logger_data %>%
  group_by(field, date) %>% 
  summarise(max_temp_5 = max(temp_5cm, na.rm = T))

min_temp_5 <- logger_data %>% 
  group_by(field, date) %>% 
  summarise(min_temp_5 = min(temp_5cm, na.rm = T))

max_temp_10 <- logger_data %>%
  group_by(field, date) %>% 
  summarise(max_temp_10 = max(temp_10cm, na.rm = T))

min_temp_10 <- logger_data %>% 
  group_by(field, date) %>% 
  summarise(min_temp_10 = min(temp_10cm, na.rm = T))

surf_moist_avg <- logger_data %>% 
  group_by(field, date) %>% 
  summarise(avg_moist = mean(moist_5cm, na.rm = TRUE))

moist_max_10cm <- logger_data %>% 
  group_by(field, date) %>% 
  summarise(max_moist_10cm = max(moist_10cm, na.rm = TRUE))

moist_avg_10cm <- logger_data %>% 
  group_by(field, date) %>% 
  summarise(avg_moist_10cm = mean(moist_10cm, na.rm = TRUE))

surf_moist_max <- logger_data %>% 
  group_by(field, date) %>% 
  summarise(max_moist = max(moist_5cm, na.rm = TRUE))

joined_env_data <- left_join(precip,max_temp_5,by = NULL)
joined_env_data <- left_join(joined_env_data,min_temp_5)
joined_env_data <- left_join(joined_env_data,max_temp_10)
joined_env_data <- left_join(joined_env_data,min_temp_10)
joined_env_data <- left_join(joined_env_data, surf_moist_avg)
joined_env_data <- left_join(joined_env_data, surf_moist_max)
joined_env_data <- left_join(joined_env_data, moist_avg_10cm)
joined_env_data <- left_join(joined_env_data, moist_max_10cm)
joined_env_data

