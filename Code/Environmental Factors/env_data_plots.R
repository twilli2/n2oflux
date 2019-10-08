library(lubridate)
joined_env_data
joined_env_data$date<- as.POSIXct(joined_env_data$date)
joined_env_data$month <- month(joined_env_data$date, label = T)
season2<- joined_env_data %>% 
  filter(date >= "2018-10-01")
season2$season <- 2
season1 <- joined_env_data %>% 
  filter(date < "2018-10-01")
season1$season <- 1
joined_env_data <- bind_rows(season1,season2)
rm(season1,season2)
#total precipitation
ggplot(data = filter(joined_env_data, field != 3)) + 
  geom_point(mapping = aes(x = date, y = total_precip), color = "navy") +
  labs(
    x = "", y = "Precipitation (mm)" 
    ) +
  theme_bw() +
  theme(axis.text.x  = element_text(size=12, colour="black", face = "bold"),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0)) +
  facet_wrap(~field, nrow = 4)

ggplot(data = joined_env_data) + 
  geom_line(mapping = aes(x = date, max_temp_5, color = "Max 5cm"), size = 1) +
  geom_line(mapping = aes(x = date, max_temp_10, color = "Max 10cm"), size = 1) +
  geom_line(mapping = aes(x = date, min_temp_5, color = "Min 5cm"), size = 1) +
  geom_line(mapping = aes(x = date, min_temp_10, color = "Min 10cm"), size = 1) +
  scale_colour_manual(name = "", 
                      breaks = c("Max 5cm","Max 10cm","Min 5cm","Min 10cm"),
                      values = c("Max 5cm"="deeppink","Max 10cm"="cyan","Min 5cm"="deeppink3","Min 10cm"="cyan3")) +
  labs(x = "", y = "Soil Temp (C)") +
  theme_bw() +
  theme(axis.text.x  = element_text(size=12, colour="black", face = "bold"),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0)) +
  facet_wrap(~field, nrow = 4)

###average daily moisture from both sensors
joined_env_data$date <- as.Date(joined_env_data$date)
ggplot(data = joined_env_data) + 
  geom_line(mapping = aes(x = date, mean_avg_moist, color = field), size = 1) +
  ylab("Daily average soil mositure") + labs(color='Field', values = c("coral1", "palegreen3","steelblue2","plum4"))+
  xlab("")+
  scale_color_manual(labels = c("1", "2","3","4"), values = c("coral", "palegreen3","steelblue2","plum3"))+
  annotate("rect",xmin = as.Date("2019-06-12"),xmax = as.Date("2019-07-25"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
  annotate("rect",xmin = as.Date("2018-06-28"),xmax = as.Date("2018-10-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
  #scale_colour_manual(name = "", 
  #                    breaks = c("Max 5cm","Max 10cm","Min 5cm","Min 10cm"),
  #                    values = c("Max 5cm"="deeppink","Max 10cm"="cyan","Min 5cm"="deeppink3","Min     #                    10cm"="cyan3")) +
  #labs(x = "", y = "Average soil moisture") +
  theme_bw() +
  theme(axis.text.x  = element_text(size=20, colour="black", face = "plain"),  
        axis.title.x = element_text(size = 20, vjust=-0.1, face = "plain"),
        axis.text.y = element_text(size=20, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 20, face = "plain"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 20, colour = "black", face = "plain", angle = 0))

###average daily temperature from both sensors
ggplot(data = joined_env_data) + 
  geom_line(mapping = aes(x = date, mean_avg_temp, color = field), size = 1) +
  #scale_colour_manual(name = "", 
  #                    breaks = c("Max 5cm","Max 10cm","Min 5cm","Min 10cm"),
  #                    values = c("Max 5cm"="deeppink","Max 10cm"="cyan","Min 5cm"="deeppink3","Min     #                    10cm"="cyan3")) +
  annotate("rect",xmin = as.Date("2019-06-12"),xmax = as.Date("2019-07-25"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
  annotate("rect",xmin = as.Date("2018-06-28"),xmax = as.Date("2018-10-31"),ymin=-Inf,ymax=Inf, alpha=0.1, fill="black")+
  labs(x = "", y = "") +
  ylab("Daily average soil temperature (\u00B0C)") + labs(color='Field')  +
  scale_color_manual(labels = c("1", "2","3","4"), values = c("coral", "palegreen3","steelblue2","plum3"))+
  theme_bw() +
  theme(axis.text.x  = element_text(size=20, colour="black", face = "plain"),  
        axis.title.x = element_text(size = 20, vjust=-0.1, face = "plain"),
        axis.text.y = element_text(size=20, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 20, face = "plain"),
        legend.text = element_text(size = 20),
        legend.title = element_text("Field",size = 20),
        strip.text.x = element_text(size = 20, colour = "black", face = "plain", angle = 0))
 # facet_wrap(~field, nrow = 4)

ggplot(data = joined_env_data) + 
  geom_line(mapping = aes(x = date, avg_moist, color = "Max 5cm"), size = 1) +
  geom_line(mapping = aes(x = date, avg_moist_10cm, color = "Max 10cm"), size = 1) +
  scale_color_manual(name = "",
                     breaks = c("Max 5cm","Max 10cm"),
                     values = c("Max 5cm" = "orangered", "Max 10cm" = "orangered4")) +
  theme_bw() +
  theme(axis.text.x  = element_text(size=12, colour="black", face = "bold"),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0)) +
  labs(
    x = "", y = "Soil Moisture"
  ) +
  facet_wrap(~field, nrow = 4)

ggplot(data = joined_env_data) + 
  geom_line(mapping = aes(x = date, mean_avg_moist, color = "Mean Moist"), size = 1) +
  #geom_line(mapping = aes(x = date, avg_moist_10cm, color = ""), size = 1) +
  scale_color_manual(name = "",
                     breaks = c("Mean Moist"),
                     values = c("Mean Moist" = "blue")) +
  theme_bw() +
  theme(axis.text.x  = element_text(size=12, colour="black", face = "bold"),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0)) +
  labs(
    x = "", y = "Soil Moisture"
  ) +
  facet_wrap(~field, nrow = 4)

