library(tidyverse)
library(ggplot2)
library(readxl)
library(plotrix)
library(lubridate)
library(readxl)

flux_data <- read.csv("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Tidy Data/flux_data.csv")
n2oflux <- filter(flux_data, n2o_rsq >= 0.30 & n2o_rsq < 1, plot != "C" & plot != "P") %>%
  group_by(season,date,field,plot) %>% 
  summarise(mean_flux = mean(n2o_flux)) %>% 
  arrange(match(plot, c("0","25","50", "75", "100")),-desc(field), -desc(date))

n2oflux$plot <- factor(n2oflux$plot,levels = c("0","25","50", "75", "100"))
n2oflux$season <- factor(n2oflux$season, levels = c("1","2"))
n2oflux$date <- as.Date(n2oflux$date)
n2oflux1 <- n2oflux %>%
  filter(season==1) %>% 
  group_by(field,plot) %>% 
  mutate(cum_flux = cumsum(mean_flux)) 
n2oflux1$cum_flux <- n2oflux1$cum_flux*2.4  

n2oflux2 <- n2oflux %>%
  filter(season==2) %>% 
  group_by(field,plot) %>% 
  mutate(cum_flux = cumsum(mean_flux)) 
n2oflux2$cum_flux <- n2oflux2$cum_flux*2.4  

fieldnames <- c("1"= "Field 1","2" = "Field 2","3" = "Field 3", "4" = "Field 4")

t_n <- read_csv("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Tidy Data/total_nitrogen_addition.csv")
t_n$season <- as.factor(t_n$season)
t_n$field <- as.factor(t_n$field)
 
t_n <- t_n %>%
  group_by(date,month,year,field,plot) %>% 
  summarise(total_n = sum(total_n))

n_dates <- t_n %>% 
  group_by(date, field) %>% 
  select(1,2)
n_dates$cum_flux <- 0

#season 1
ggplot(n2oflux1) + 
  geom_point(aes(date,cum_flux,color = plot, shape = plot))+
  geom_line(aes(date,cum_flux,color = plot)) +
  #geom_point(filter(n_dates <= 2018-07-01), aes(date,cum_flux),size = 2,shape = 2)+
  facet_wrap(~field, labeller = as_labeller(fieldnames))+
  labs(x = "",y = expression(Cumulative~emissions~" "~(g~N[2]*O-N~~ha^{-1})), color='% max\n N rate',shape = NULL)+
  guides(shape = FALSE)+
  scale_y_continuous(limits = c(-25,330)) +
  theme_bw() +
  theme(axis.text.x  = element_text(size=14, colour="black"),  
        axis.title.x = element_text(size = 16, vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text("Field",size = 16),
        strip.text.x = element_text(size = 16, colour = "black", face = "plain", angle = 0))
#season 2
ggplot(n2oflux2) + 
  geom_point(aes(date,cum_flux,color = plot, shape = plot)) +
  geom_line(aes(date,cum_flux,color = plot))+
  #geom_point(data = n_dates,aes(date,cum_flux),size = 2,shape = 2)+
  facet_wrap(~field, labeller = as_labeller(fieldnames))+
  labs(x = "",y = expression(Cumulative~emissions~" "~(g~N[2]*O-N~~ha^{-1})), color='% max\n N rate')+
  scale_y_continuous(limits = c(-25,330)) +
  theme_bw() +
  guides(shape = FALSE)+
  theme(axis.text.x  = element_text(size=14, colour="black"),  
        axis.title.x = element_text(size = 16, vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text("Field",size = 16),
        strip.text.x = element_text(size = 16, colour = "black", face = "plain", angle = 0))  

n_dates <- total_n %>% 
  group_by(date, field) %>% 
  select(1,2)
n_dates$cum_flux <- 0
