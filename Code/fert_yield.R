library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
#C:/Users/twilli2/
total_fert<- read_excel("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Fert-Yield data/Fertilizer Plans.xlsx",
sheet = "total fert")
 

yield_data <- as_tibble(c(1,2,3,4))
yield_data$yield_kg <- (c(1586,2707,1995,1771))
mutate(yield_data, yield_lb = yield_kg/1.12085)
yield_data <- rename(yield_data, field = value)
yield_data$total_n_lb<- c(155, 160, 232, 232)
yield_data$plot <- c("C")
yield_data1 <- yield_data
yield_data2 <- yield_data
yield_data2$plot <-  c("P")
yield_data2$yield_kg <- c(1468,2175,2982,2533)
fert_yield <- bind_rows(yield_data1,yield_data2)
fert_yield <- mutate(fert_yield, yield_lb = yield_kg/1.12085)
fert_yield$total_field_yield_lb <- 0
fert_yield
fert_yield[c(3,4,7,8),"total_field_yield_lb"] <- fert_yield[c(3,4,7,8),"total_field_yield_lb"] + 2049
fert_yield$average_yield_lb <- c(1363,2178,2220,1920)
View(fert_yield)

ggplot(fert_yield) +
  geom_col(aes(plot, yield_lb),color = "black", fill = "lightblue") +
  geom_col(aes(plot, average_yield_lb), alpha = 0.0, 
           color = "black", linetype = "dashed", size = .5) +
  labs(x = "Treatment", y = "Yield (lbs/acre)")+
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=14, colour="black", face = "bold"),  
        axis.title.x = element_text(size = 14, vjust=-1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 14, colour = "black", face = "bold", angle = 0)) +
  facet_grid(~field)

ggplot(data = total_fert) +
  geom_col(aes(season, lbs_n_acre, fill = type), color = "black")+
  labs(x = "Season", y = "Nitrogen addition (lbs/acre)")+
  scale_fill_discrete(name = "Type", labels = c("Ammonium", "Urea")) +
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=10, colour="black", face = "bold", angle = 90),  
        axis.title.x = element_text(vjust=-0.1, size = 14, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 14, colour = "black", face = "bold", angle = 0))+
  facet_grid(~field)

