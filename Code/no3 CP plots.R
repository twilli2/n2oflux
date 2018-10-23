library(ggplot2)
lys_data_CP <- filter(lys_data,compound == 'no3', treatment == 'C' | treatment == 'P', concentration > 0)
all_n <- filter(lys_data, treatment == 'C' | treatment == 'P', concentration > 0)
CP <- lys_data_CP %>% 
  group_by(field, date, treatment) %>% 
  summarise(mean_conc = mean(concentration,na.rm = T),
            total_conc = sum(concentration,na.rm = T),
            max_conc = max(concentration,na.rm = T),
            sd = sd(concentration, na.rm = T))
n <- all_n %>% 
  group_by(field, treatment, compound) %>% 
  summarise(sum = sum(concentration, na.rm = T), 
            sd = sd(concentration, na.rm = T),
            mean = mean(concentration, na.rm = T),
            se = std.error(concentration, na.rm = T))
library(plotrix)
n
pd <- position_dodge(.9)
pdd <- position_dodge2(.9)
ggplot(n) +
  geom_col(aes(treatment,mean, fill = compound), color = "black",
          width = 0.9, position  = pd) +
  geom_errorbar(aes(x = treatment, ymin = mean - se, ymax = mean + se), width = 0.9, position = pdd) +
  theme(legend.position = "right") +
  scale_fill_hue(h=c(500,50),name = "Compound", labels = c("Ammonium","Nitrate"))+
  labs(x = "Treatment", y = "Average N (ppm)") +
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=14, colour="black", face = "bold"),  
        axis.title.x = element_text(size = 14,vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0))+
  facet_grid(~field)
#
n1
n1 <- all_n %>% 
  group_by(field, treatment, compound) %>%
  summarise(sum = sum(concentration, na.rm = T), 
            sd = sd(concentration, na.rm = T),
            mean = mean(concentration, na.rm = T),
            se = std.error(concentration, na.rm = T) %>% 
            arrange_all(compound))

ggplot(data = n1, mapping = aes(treatment, mean, fill = compound)) +
  geom_col()+
  geom_errorbar(aes(x = treatment, ymin = mean - se, ymax = mean + se), width = 0.3) +
  theme(legend.position = "right") +
  scale_fill_hue(h=c(500,50),name = "Compound", labels = c("Ammonia","Nitrate"))+
  labs(x = "Treatment", y = "Average N (ppm)") +
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=14, colour="black", face = "bold"),  
        axis.title.x = element_text(size = 14,vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0))+
  facet_grid(~field)

#line nitrate by time

ggplot(data = CP, aes(x = date)) +
  geom_point(mapping = aes(y = max_conc, color = treatment), na.rm = TRUE, size = 2, shape = 3) +
  #scale_x_date(date_labels="%b",date_breaks ="1 month")+
  geom_line(mapping = aes(y = mean_conc, color = treatment), na.rm = TRUE, size = 1.2) +
  geom_point(mapping = aes(y = mean_conc, color = treatment), na.rm = TRUE, size = 2, shape = 1) +
  scale_colour_discrete(labels = c("Conventional","Slow release")) +
  labs(
    x = NULL, 
    y = "N as nitrate (ppm)", 
    caption = ("Average (lines) & maximum (cross points) N-nitrate concentration over time"), 
    color = ("Treatment")) +
 theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=12, colour="black", face = "bold"),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0))+
  facet_wrap(~field)
CP
#add fertilization dates 
