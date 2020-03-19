library(tidyverse)
library(ggplot2)
library(readxl)
library(plotrix)
library(lubridate)
library(readxl)

flux_data <- read.csv("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Tidy Data/flux_data.csv")
n2oflux_means <- filter(flux_data, plot != "C" & plot != "P", !is.na(n2o_flux)) %>%
  mutate(n2o_flux = if_else(n2o_flux < 0,0, n2o_flux)) %>% 
  group_by(season,date,field,plot) %>% 
  summarise(mean_flux = mean(n2o_flux, na.rm = T), sd = sd(n2o_flux, na.rm = T)) %>% 
  arrange(match(plot, c("0","25","50", "75", "100")),-desc(field), -desc(date))

n2oflux_means$plot <- factor(n2oflux_means$plot,levels = c("0","25","50", "75", "100"))
n2oflux_means$season <- factor(n2oflux_means$season, levels = c("1","2"))
n2oflux_means$date <- as.Date(n2oflux_means$date)
n2oflux_means$field <- as.factor(n2oflux_means$field)
n2oflux_means$daily_g_ha <- n2oflux_means$mean_flux*2.4
n2oflux_means$sd <- n2oflux_means$sd*2.4/1000
n2oflux_means <- mutate(n2oflux_means, daily_kg_ha = daily_g_ha/1000)

###estimated daily means multiplied by sample days

season2 <- n2oflux_means %>% 
  filter(date > '2018-06-27')%>% 
  group_by(field,plot) %>% 
  mutate(jdate = (as.numeric(julian(date, origin=as.Date("2018-10-31"))))) %>% 
  mutate(x = lead(jdate))%>% 
  mutate(days = x - jdate) %>% 
  select(2,3,4,8,11,6) %>% 
  mutate(days = ifelse(is.na(days), 1, days)) %>% 
  mutate(flux = daily_kg_ha*days) %>% 
  mutate(cum_flux = cumsum(flux)) %>% 
  mutate(sd = sd*days)

n2 <- n2oflux_means %>% 
  filter(date > '2018-06-27')%>% 
  group_by(field,plot) %>% 
  tally()

season2 <- left_join(season2,n2, by = c("field","plot"))  
season2 <- season2 %>% 
  mutate(se = sd/sqrt(n))

season1 <- n2oflux_means %>% 
  filter(date <= '2018-06-27')%>% 
  group_by(field,plot) %>% 
  mutate(jdate = (as.numeric(julian(date, origin=as.Date("2017-10-03"))))) %>% 
  mutate(x = lead(jdate))%>% 
  mutate(days = x - jdate, is.na = 1) %>% 
  select(2,3,4,8,11,6) %>% 
  mutate(days = ifelse(is.na(days), 1, days)) %>% 
  mutate(flux = daily_kg_ha*days) %>% 
  mutate(cum_flux = cumsum(flux)) %>% 
  mutate(sd = sd*days)

n1 <- n2oflux_means %>% 
  filter(date <= '2018-06-27')%>% 
  group_by(field,plot) %>% 
  tally()

season1 <- left_join(season1,n1)  
season1 <- season1 %>% 
  mutate(se = sd/sqrt(n))

fieldnames <- c("1"= "Field 1","2" = "Field 2","3" = "Field 3", "4" = "Field 4")

#cumulative flux
###mean multiplied by sample days

ggplot(season2) + 
  geom_point(aes(date,cum_flux,color = plot, shape = plot))+
  geom_line(aes(date,cum_flux,color = plot)) +
  facet_wrap(~field, labeller = as_labeller(fieldnames))+
  labs(x = "",y = expression(Cumulative~emissions~" "~(kg~N[2]*O-N~~ha^{-1})), color='% max\n N rate',shape = NULL)+
  guides(shape = FALSE)+
  #scale_y_continuous(limits = c(-25,330)) +
  theme_bw() +
  theme(axis.text.x  = element_text(size=14, colour="black"),  
        axis.title.x = element_text(size = 16, vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text("Field",size = 16),
        strip.text.x = element_text(size = 16, colour = "black", face = "plain", angle = 0))

ggplot(season1) + 
  geom_point(aes(date,cum_flux,color = plot, shape = plot))+
  geom_line(aes(date,cum_flux ,color = plot)) +
  facet_wrap(~field, labeller = as_labeller(fieldnames))+
  labs(x = "",y = expression(Cumulative~emissions~" "~(kg~N[2]*O-N~~ha^{-1})), color='% max\n N rate',shape = NULL)+
  guides(shape = FALSE)+
  #scale_y_continuous(limits = c(-25,330)) +
  theme_bw() +
  theme(axis.text.x  = element_text(size=14, colour="black"),  
        axis.title.x = element_text(size = 16, vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text("Field",size = 16),
        strip.text.x = element_text(size = 16, colour = "black", face = "plain", angle = 0))

