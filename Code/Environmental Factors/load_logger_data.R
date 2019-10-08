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

data14 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42760 9Jan19-1032 Field 1.xls",
sheet = "Em50 \"EM42760\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
"numeric", "skip", "numeric", "numeric",
"numeric", "skip", "numeric", "numeric",
"numeric"))
data14$field <- 1

data15 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42763 9Jan19-1537 Field 2.xls",
sheet = "Em50 \"EM42763\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
"numeric", "skip", "numeric", "numeric",
"numeric", "skip", "numeric", "numeric",
"numeric"))
data15$field <- 2

data16 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42993 9Jan19-1144 Field 3.xls",
                    sheet = "Em50 \"EM42993\" Data", skip = 3, col_names = c('date_time','moist_10cm','temp_10cm','cond_10cm','moist_5cm','temp_5cm','cond_5cm'), 
                    col_types = c("date","skip", "skip", "numeric", "numeric", "numeric", "skip", "numeric", "numeric","numeric"))
data16$field <- 3

data17 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM43097 9Jan19-1325 Field 4.xls",
  sheet = "Em50 \"EM43097\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
  "numeric", "skip", "numeric", "numeric",
  "numeric", "numeric", "numeric",
  "numeric"))
data17$field <- 4

data18 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42760 6Mar19-0948 Field 1.xls",
sheet = "Em50 \"EM42760\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
"numeric", "skip", "numeric", "numeric",
"numeric", "skip", "numeric", "numeric",
"numeric"))
data18$field <- 1

data19 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42763 6Mar19-1507 Field 2.xls",
                     sheet = "Em50 \"EM42763\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric"))
data19$field <- 2

data20 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42993 6Mar19-1207 Field 3.xls",
                     sheet = "Em50 \"EM42993\" Data", skip = 3, col_names = c('date_time','moist_10cm','temp_10cm','cond_10cm','moist_5cm','temp_5cm','cond_5cm'), 
                     col_types = c("date","skip", "skip", "numeric", "numeric", "numeric", "skip", "numeric", "numeric","numeric"))
data20$field <- 3


data21 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM43097 6Mar19-1302 Field 4.xls",
  sheet = "Em50 \"EM43097\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
  "numeric", "skip", "numeric", "numeric",
  "numeric", "numeric", "numeric",
  "numeric"))
data21$field <- 4

data22 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42760 17Apr19-1634 Field 1.xls",
                     sheet = "Em50 \"EM42760\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric"))
data22$field <- 1

data23 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42763 17Apr19-1359 Field 2.xls",
                     sheet = "Em50 \"EM42763\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric"))
data23$field <- 2

data24 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42993 17Apr19-1206 Field 3.xls",
                     sheet = "Em50 \"EM42993\" Data", skip = 3, col_names = c('date_time','moist_10cm','temp_10cm','cond_10cm','moist_5cm','temp_5cm','cond_5cm'), 
                     col_types = c("date","skip", "skip", "numeric", "numeric", "numeric", "skip", "numeric", "numeric","numeric"))
data24$field <- 3

data25 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM43097 17Apr19-1146 Field 4.xls",
                     sheet = "Em50 \"EM43097\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric", "numeric", "numeric",
                                                                                                                                                                                          "numeric"))
data25$field <- 4
data26 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42760 29May19-0958 Field 1.xls",
                     sheet = "Em50 \"EM42760\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
                                                                                                                                                                                          "skip", "numeric", "numeric", "numeric",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric"))
data26$field <- 1

data27 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42763 29May19-1303 Field 2.xls",
                     sheet = "Em50 \"EM42763\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric"))
data27$field <- 2

data28 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42993 29May19-1429 Field 3.xls",
                     sheet = "Em50 \"EM42993\" Data", skip = 3, col_names = c('date_time','moist_10cm','temp_10cm','cond_10cm','moist_5cm','temp_5cm','cond_5cm'), 
                     col_types = c("date","skip", "skip", "numeric", "numeric", "numeric", "skip", "numeric", "numeric","numeric"))
data28$field <- 3

data29 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM43097 29May19-1516 Field 4.xls",
                     sheet = "Em50 \"EM43097\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric", "numeric", "numeric",
                                                                                                                                                                                          "numeric"))
data29$field <- 4

data30 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42760 25Jul19-0950 Field 1.xls",
                     sheet = "Em50 \"EM42760\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
                                                                                                                                                                                          "skip", "numeric", "numeric", "numeric",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                         "numeric"))
data30$field <- 1

data31 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42763 25Jul19-1406 Field 2.xls",
                     sheet = "Em50 \"EM42763\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric"))
data31$field <- 2

data32 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM42993 25Jul19-1217 Field 3.xls",
                     sheet = "Em50 \"EM42993\" Data", skip = 3, col_names = c('date_time','moist_10cm','temp_10cm','cond_10cm','moist_5cm','temp_5cm','cond_5cm'), 
                     col_types = c("date","skip", "skip", "numeric", "numeric", "numeric", "skip", "numeric", "numeric","numeric"))
data32$field <- 3

data33 <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Decagon Data/EM43097 25Jul19-1259 Field 4.xls",
                     sheet = "Em50 \"EM43097\" Data", skip = 3, col_names = c('date_time','precip','moist_5cm','temp_5cm','cond_5cm','moist_10cm','temp_10cm','cond_10cm'), col_types = c("date",
                                                                                                                                                                                          "numeric", "skip", "numeric", "numeric",
                                                                                                                                                                                          "numeric", "numeric", "numeric",
                                                                                                                                                                                          "numeric"))
data33$field <- 4
logger_data <- bind_rows(data,data2,data3,data4,data5,data6,data7,
                         data8,data9,data10,data11,data12,data13,
                         data14,data15,data16,data17,data18, data19, 
                         data20, data21, data22, data23, data24, data25,
                         data26, data27, data28, data29, data30, data31, data32, data33)

#format logger data
logger_data$field <- as.numeric(logger_data$field)
logger_data$month <- month(logger_data$date_time,label = T)
logger_data$date <- date(logger_data$date_time)
logger_data$year <- year(logger_data$date_time)
logger_data

field1 <- logger_data %>% 
  subset(field == 1) %>% 
  rename(moist_5cm1 = moist_10cm, moist_10cm1 = moist_5cm, temp_5cm1 = temp_10cm, temp_10cm1 = temp_5cm, cond_5cm1 = cond_10cm, cond_10cm1 = cond_5cm)

field1 <- field1 %>% 
rename(moist_5cm = moist_5cm1, temp_5cm = temp_5cm1, cond_5cm = cond_5cm1, moist_10cm = moist_10cm1, temp_10cm = temp_10cm1, cond_10cm = cond_10cm1)

logger_data1<- subset(logger_data, field == 2 | field == 3 | field == 4)
logger_data<-bind_rows(logger_data1, field1)

precip <- logger_data %>% 
  group_by(field, date) %>% 
  summarise(total_precip = sum(precip, na.rm = T))

mean_precip <- logger_data %>% 
  group_by(field,date) %>% 
  summarise(mean_precip = mean(precip, na.rm = T))

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

mean_temp_5 <- logger_data %>% 
  group_by(field,date) %>% 
  summarise(mean_temp_5 = mean(temp_5cm, na.rm = T))

mean_temp_10 <- logger_data %>% 
  group_by(field,date) %>% 
  summarise(mean_temp_10 = mean(temp_10cm, na.rm = T))

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
joined_env_data <- left_join(joined_env_data, mean_temp_10)

joined_env_data$field <- as.factor(joined_env_data$field)
joined_env_data$date <- as.Date(joined_env_data$date)

joined_env_data$mean_avg_moist <- (joined_env_data$avg_moist+joined_env_data$avg_moist_10cm)/2
joined_env_data$mean_avg_temp <- (joined_env_data$max_temp_5+joined_env_data$min_temp_5)/2
rm(data,data2,data3,data4,data5,data6,data7,
                         data8,data9,data10,data11,data12,data13,
                         data14,data15,data16,data17,data18, data19, 
                         data20, data21, data22, data23, data24, data25,
                         data26, data27, data28, data29,data30, data31, data32, data33)
rm(field1, logger_data1, logger_data, precip, max_temp_5, min_temp_5, min_temp_10, max_temp_10, surf_moist_avg, surf_moist_max, moist_avg_10cm, moist_max_10cm, mean_temp_10, mean_temp_5, mean_precip)
