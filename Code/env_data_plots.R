library(lubridate)
joined_env_data
joined_env_data$date<- as.POSIXct(joined_env_data$date)
joined_env_data$month <- month(joined_env_data$date, label = T)
ggplot(data = joined_env_data) + 
  geom_point(mapping = aes(x = date, y = total_precip), color = "navy") +
  labs(
    x = "Month", y = "Precipitation (mm)" 
    ) +
  theme_bw() +
  facet_wrap(~field)

ggplot(data = joined_env_data) + 
  geom_line(mapping = aes(x = date, max_temp_5, color = "Max 5cm"), size = 1) +
  geom_line(mapping = aes(x = date, max_temp_10, color = "Max 10cm"), size = 1) +
  geom_line(mapping = aes(x = date, min_temp_5, color = "Min 5cm"), size = 1) +
  geom_line(mapping = aes(x = date, min_temp_10, color = "Min 10cm"), size = 1) +
  scale_colour_manual(name = "", 
                      breaks = c("Max 5cm","Max 10cm","Min 5cm","Min 10cm"),
                      values = c("Max 5cm"="deeppink","Max 10cm"="cyan","Min 5cm"="deeppink3","Min 10cm"="cyan3")) +
  labs(x = "Month", y = "Soil Temp (°C)") +
  theme_dark() +
  theme(axis.text.x  = element_text(size=12, colour="black", face = "bold"),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "white", face = "bold", angle = 0)) +
  facet_wrap(~field)

yrng <- range(joined_env_data$max_moist)
temp <- data.frame(
  start = as.Date(c('2018-01-01', '2018-02-01', '2018-03-01', '2018-04-01')), 
  end   = as.Date(c('2018-01-31', '2018-02-28', '2018-03-31', '2018-04-30')))

ggplot(data = joined_env_data) + 
  geom_line(mapping = aes(x = date, max_moist, color = "Max 5cm"), size = 1) +
  geom_line(mapping = aes(x = date, max_moist_10cm, color = "Max 10cm"), size = 1) +
  scale_color_manual(name = "",
                     breaks = c("Max 5cm","Max 10cm"),
                     values = c("Max 5cm" = "orangered", "Max 10cm" = "orangered4")) +
  theme_dark() +
  theme(axis.text.x  = element_text(size=12, colour="black", face = "bold"),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "white", face = "bold", angle = 0)) +
  labs(
    x = "Month", y = "Soil Moisture"
  ) +
  facet_wrap(~field)

geom_rect(mapping = aes(x = date, y = max_moist, fill = month), data = joined_env_data)
utils::View(joined_env_data)
geom_rect(aes(NULL, NULL, xmin = month, xmax = month, fill = month),
          ymin = yrng[1], ymax = yrng[2], data = joined_env_data)
yrng
?geom_rect
# let's make it past & future proof for the next few years: 
library(lubridate)
library(tidyverse)
dateRanges <- as.data.frame(seq(from = as.Date("2017/1/1"), to = as.Date("2019/12/31"), "day"))
?seq
dateRanges$month<- month(dateRanges$date)
group_by(dateRanges,month)
date$Ranges
as.tibble(dateRanges)
ggplot(joined_env_data) + 
  geom_bar(data = dateRanges, aes(xmin = start , xmax = end, ymin = yrng[1], ymax = yrng[2]),
            inherit.aes= F, alpha = 0.4, fill = c("blue"))+
  geom_line(aes(x=  date, max_moist, color = "Max 5cm"), size = 1) +
  geom_line(mapping = aes(x = date, max_moist_10cm, color = "Max 10cm"), size = 1) +
  scale_color_manual(name = "",
                     breaks = c("Max 5cm","Max 10cm"),
                     values = c("Max 5cm" = "orangered", "Max 10cm" = "orangered4")) +
  theme_dark() +
  labs(
    x = "Month", y = "Soil Moisture"
  )+ 
  facet_wrap(~field)
