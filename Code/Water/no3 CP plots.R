library(tidyverse)
library(ggplot2)
library(plotrix)
install.packages("RColorBrewer")
library(RColorBrewer)

all_n <- filter(lys_data, treatment == 'C' | treatment == 'P', concentration > 0)
all_n$field <- as.factor(all_n$field)
nh4 <- rename(nh4,nh4conc=vol_conc)
no3 <- rename(no3,no3conc=vol_conc)
all_n <- left_join(nh4,no3, by = c("date","field","treatment"))

tn <- all_n %>% 
  group_by(date,field,treatment) %>% 
  mutate(total_n=nh4conc+no3conc)
tn$year <- year(tn$date)

mod1<- lm(total_n~treatment+field+year,data=tn)
anova(mod1)  
tidy(mod1)

ggplot(tn)+
  geom_boxplot(aes(treatment,total_n))+
  facet_grid(year~field)


ggplot(data = tn, aes(x = date)) +
  geom_point(mapping = aes(y = total_n, color = treatment), na.rm = TRUE, size = 2, shape = 3) +
  #scale_x_date(date_labels="%b",date_breaks ="1 month")+
  geom_line(mapping = aes(y = total_n, color = treatment), na.rm = TRUE, size = 1.2) +
  scale_colour_discrete(labels = c("Conventional","Slow release")) +
  labs(
    x = NULL, 
    y = "N as nitrate and ammonium (ppm)", 
    caption = ("Average N volume weighted concentration over time"), 
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


CP <- all_n %>%
  group_by(field, treatment, compound,year) %>% 
  summarise(mean_conc = mean(concentration,na.rm = T),
            total_conc = sum(concentration,na.rm = T),
            max_conc = max(concentration,na.rm = T),
            sd = sd(concentration, na.rm = T))



pd <- position_dodge(.5)
pdd <- position_dodge2(.5)

ggplot(CP) +
  geom_col(aes(treatment, mean_conc, fill = compound), color = "black",
          width = 0.9, position  = pd) +
  geom_errorbar(aes(x = treatment, ymin = mean_conc, ymax = mean_conc + sd), width = 0.5, position = pdd) +
  theme(legend.position = "right") +
  scale_fill_hue(h=c(500,950),name = "Compound", labels = c("Ammonium","Nitrate"))+
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
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0),
        strip.text.y = element_text(size = 12, colour = "black", face = "bold", angle = -90))+
  facet_grid(year~field)


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
