library(tidyverse)
library(psych)
library(stats)
#cp_n2o <- filter(flux_data,compound == "n2o", plot == "C" | plot == "P") %>%   select_all()
cp_n2o <- flux_data %>%  
  select(1,2,3,4,9,16,8,14) %>%
  filter(plot == "C" | plot == "P") %>% 
  filter(n2o_rsq > 0.1 & n2o_rsq < 1) %>% 
  rename(flux = n2o_flux) 
library(Rmisc)
?summarySE
detach(Rmisc, unload = T)
sum_behaviours <- summarySE(cp_n2o, measurevar =  "flux",
                            groupvar = c("field", "plot","season"), na.rm = TRUE)
season_names <- list('1' = 'Season 1', '2' = 'Season 2')
season_labeller <- function(variable,value){
  return(season_names[value])
}

library(ggplot2)
ggplot(sum_behaviours, aes(x = field, y = flux, fill = plot)) +
  scale_fill_discrete(name = "Treatment", labels = c("Conventional","EEF")) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin = flux - se, ymax = flux + se),
                width=.2, position = position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = , hjust = 1))+
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        #strip.text = element_text(labels = c("Season 1", "Season 2")),
        axis.text.x  = element_text(size=12, colour="black", face = "bold"),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=12, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0))+
  labs(x = "Field", y = "Average flux (ng N/cm/hr)")+
  facet_grid(~season, labeller = season_labeller)

df1 <- cp_n2o %>% 
  group_by(season, field, plot) %>%
  summarize(mean_flux = mean(flux), sd = sd(flux,na.rm = T)) %>% 
  filter(mean_flux != "NA")
  
ggplot(df1, aes(x = plot, y = mean_flux, fill = plot), color = "black") +
  geom_col() +
  scale_fill_discrete(name = "Treatment", labels = c("Conventional","Enhanced Efficiency")) +
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
  labs(x = NULL, y = "Average flux (ng N/cm/hr)")+
  #geom_errorbar(aes(x = plot, ymax = mean_flux + sd, ymin = mean_flux - sd), 
  #              size = .5, width = 0.3) +
  facet_grid(season~field)

hist(cp_n2o$flux)
shapiro.test(cp_n2o$flux) #null hyp = data from norm distribution. Rejected
summary(cp_n2o$flux)
filter(cp_n2o, plot == "C") %>% 
  summary(cp_n2o$flux) #median is higher 1.4873, mean is lower 2.3327 than population (1.3416,2.3868)
filter(cp_n2o, plot == "P") %>% 
  summary(cp_n2o$flux) #median is lower 1.172, mean is higher 2.440 than population. Greater range than C plots

cp_n2o<- cp_n2o %>% #add number to eliminate negs and log transform data
  mutate(flux_100 = flux + 100) %>% 
  mutate(lflux = log(flux_100)) 
summary(cp_n2o$lflux)
hist(cp_n2o$lflux)
shapiro.test(cp_n2o$lflux) #non normally distributed data after transformations.

kruskal.test(flux~plot, data = cp_n2o) #non-parametric anova
TukeyHSD(aov)
mod <- lm(data = cp_n2o,flux~field+plot+soil_temp)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(mod)
shapiro.test(df$flux)
summary(mod)
aov <- anova(mod)
plot(resid(mod))#residuals don't have a normal distribution, can't use linear model family
plot(mod)
library(dplyr)
cp_n2o_sum <- cp_n2o %>% 
  group_by(date, field, plot) %>% 
  mutate (mean_flux = mean(flux))
#%>% 
 # summarize(cs = cumsum(mean_flux))

df<- cp_n2o_sum %>% 
  group_by(field, plot) %>% 
  summarize(mean = mean(mean_flux), sd = sd(mean_flux))


cp_n2o_sum$date <- as.Date(cp_n2o_sum$date)

ggplot(cp_n2o_sum, aes(x = date, y = cs, color = plot)) +
  geom_point(size = 2, shape = 1) +
  geom_line(size = 1.2) +
  #geom_area(alpha = 0.2) + 
  scale_color_discrete(name = "Treatment", labels = c("Conventional","Slow-release")) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  labs(x = NULL, y = "Average cumulutive flux (ng N/cm/hr)")+
  geom_errorbar(aes(x = date, ymin = cs - sd, ymax = cs + sd), size = 0.5, width = 0.3) +
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=14, colour="black", face = "bold"),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0))+
  facet_wrap(field)

