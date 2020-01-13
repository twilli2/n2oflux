library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
#C:/Users/twilli2/
fert_data <- read_excel("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Fert-Yield data/Fertilizer Plans.xlsx",
sheet = "total fert", col_types = c("date","text", "text", "numeric", "numeric",
                      "numeric", "numeric", "numeric"))
#based on samples and estimates
cpyield <- yield_data %>% 
  filter(plot == "Conv" | plot == "EEF", season = 2) %>% 
  group_by(field,plot) %>% 
  summarise(mean_seed_kg_ha = mean(seed_kg_ha))

ggplot(cpyield) +
  geom_col(aes(plot, mean_seed_kg_ha),color = "black", fill = "lightblue")+
  #geom_col(aes(plot, average_yield_lb), alpha = 0.0, 
  #         color = "black", linetype = "dashed", size = .5) +
  labs(x = "Treatment", y = "Yield (kgs/ha)")+
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=14, color = "black", face = "bold"),  
        axis.title.x = element_text(size = 14, vjust=-1, face = "bold"),
        axis.text.y = element_text(size=14, color = "black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 14, color = "black", face = "bold", angle = 0)) +
  facet_grid(season~field)

weigh_wagon_yield_data <- read_excel("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Fert-Yield data/Yield Data.xlsx",
sheet = "Sheet4")
weigh_wagon_yield_data$season[weigh_wagon_yield_data$season == 1] <- "S1" 
weigh_wagon_yield_data$season[weigh_wagon_yield_data$season == 2] <- "S2"
seasonnames <- c("S1" = "Season 1", "S2" = "Season 2", "1" = "Field 1","2" = "Field 2","3" = "Field 3","4" = "Field 4")
ggplot(weigh_wagon_yield_data) +
  geom_col(aes(plot, seed_lbs_ac, fill = plot), color = "black")+
  scale_fill_manual(values = c("#e7298a","#1b9e77"),
                    labels = c("Conventional","Enhanced Efficiency"))+
  #geom_col(aes(plot, average_yield_lb), alpha = 0.0, 
  #         color = "black", linetype = "dashed", size = .5) +
  labs(x = "Treatment", y = "Dirt Seed Yield (lbs/ac)", fill = "")+
  theme_bw()+
  theme(axis.text.x  = element_text(size=14, colour="black", angle = 0),  
        axis.title.x = element_text(size = 16, vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text("Field",size = 16),
        legend.position = "bottom",
        strip.text.y = element_text(size = 14, color = "black", face = "plain", angle = -90),
        strip.text.x = element_text(size = 14, color = "black", face = "plain", angle = 0))+
  facet_grid(season~field, labeller = as_labeller(seasonnames))



fert_yield$season <- as.factor(fert_yield$season)

  
fert_total1 <-  filter(fert_data,date <= "2018-04-09")
fert_total1$season <- 1
fert_total2 <-  filter(fert_data,date > "2018-04-09") 
fert_total2$season <- 2
fert_total <- bind_rows(fert_total1,fert_total2)
fert_total2 %>%
  filter(plot == "C") %>% 
  group_by(field) %>% 
  summarise(total_urea = sum(urea_ac)*1.12085, total_ammonium = sum(nh4_ac)*1.12085, t_n = sum(tn_ac)*1.12085)

fert_applied <- fert_total %>%
  filter(plot == "C") %>% 
  group_by(season,field,plot) %>% 
  gather(compound, value = conc,nh4_ac:urea_ac)

seasonnames2 <- c("1" = "Season 1", "2" = "Season 2")
ggplot(data=fert_applied, color = black) +
  geom_col(aes(field, conc,fill = compound))+
  labs(y = expression("N application rate (lbs/acre)"))+
  labs(x = expression("Field"), fill = "")+
  scale_fill_brewer(palette = "Dark2",
                     breaks = c("nh4_ac","urea_ac"),
                    labels = c("Ammonium","Urea"))+
  theme_bw()+
  theme(axis.text.x  = element_text(size=14, colour="black", angle = 0),  
        axis.title.x = element_text(size = 16, vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text("Field",size = 16),
        legend.position = "bottom",
        strip.text.x = element_text(size = 14, color = "black", face = "plain", angle = 0))+
        facet_grid(~season, labeller = as_labeller(seasonnames2))
  


  
