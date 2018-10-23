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
lys_env_data<-left_join(joined_env_data, lys_data)
utils::View(lys_env_data)
#write.csv(lys_env_data)
#write.csv(lys_env_data, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/R/lys_env_data.csv")
