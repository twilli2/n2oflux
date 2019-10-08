library(tidyverse)
library(ggplot2)
library(readxl)
library(plotrix)
library(lubridate)
library(readxl)
fert_data <- read_excel("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Fert-Yield data/Fertilizer Plans.xlsx",
sheet = "total fert", col_types = c("date","text", "text", "numeric", "numeric",
                      "numeric", "numeric", "numeric"))

df <- fert_data %>% 
  filter(plot == "100") %>% 
  select_all() %>% 
  group_by(date, field) %>% 
  summarize(N100 = sum(tn_ac)) %>% 
  arrange(field)

df <- df %>% 
  mutate('100' = N100, '75' = N100 * 0.75, '50' = N100 * 0.50, '25' = N100 * 0.25, '0' = N100 * 0)

df$field <- as.factor(df$field)
df$plot <- as.factor(df$field)
df$date <- as.Date(df$date)
df$month <- month(df$date) 
df$year <- year(df$date)
df$quarter <- quarter(df$date)

y1a <- df %>% 
  filter(quarter == "4" & year == "2017"| quarter == "2" & year == "2017")
y1b <- df %>% 
  filter(quarter == "1" & year == "2018" | quarter == "2" & year == "2018")
y1 <- bind_rows(y1a,y1b)
y1 <- gather(y1, plot, total_n, '100':'0')
y1$season <- "1" 

y2a <- df %>% 
  filter(quarter == "4" & year == "2018")
y2b <- df %>% 
  filter(quarter == "1" & year == "2019"|quarter == "2" & year == "2019")
y2 <-bind_rows(y2a,y2b)
y2 <- gather(y2, plot, total_n, '100':'0')
y2$season <- "2" 

t_n <- bind_rows(y1,y2)
t_n <- t_n [-3]
rm(df,fert_data,y1,y1a,y1b,y2,y2a,y2b)

##load gas sample and flux calc data
j <- select(p,1,2,3,8,9,16)

j$plot[j$plot == "N0"] <- "0"
j$plot[j$plot == "N25"] <- "25"
j$plot[j$plot == "N50"] <- "50"
j$plot[j$plot == "N75"] <- "75"
j$plot[j$plot == "N100"] <- "100"
j$plot[j$plot == "Conv"] <- "C"
j$plot[j$plot == "PA"] <- "P"

#mean gradient flux by date and plot with abherrant slopes removed
j <- filter(j, n2o_rsq >= 0.30 & n2o_rsq < 1) %>% 
  filter(plot != "C" & plot != "P") %>% 
  group_by(field, plot, date, season) %>% 
  summarise(mean_flux = mean(n2o_flux)) %>% 
  arrange(match(plot, c("0","25","50", "75", "100")),-desc(field), -desc(date))

#daily mean flux 268 days in season 1, 224 days in season 2

total_flux <- j %>%
  group_by(field,plot) %>% 
  summarise(total_flux = sum(mean_flux,na.rm=T)/492) %>% 
  mutate(total_flux = total_flux*2.4)
total_flux$field <- as.factor(total_flux$field)

total_n <- t_n %>% 
  group_by(field,plot) %>% 
  summarize(total_n = sum(total_n)) %>% 
  mutate(total_n = (total_n*1.12085)/2) 
total_n$field <- as.factor(total_n$field)
flux_tn <- left_join(total_flux,total_n)

###Figure 1. Average daily flux as function on nitrogen rate by field. Least squared quadratic regression

ggplot(data = flux_tn, aes(total_n,total_flux))+
  geom_point(aes(color = field), size = 6) +
  stat_smooth(method = "lm",
              formula = y ~ x + poly(x, 2) - 1) +
  labs(x = expression("Average nitrogen rate (kg N"~~ha^{-1}~year^{-1}*")"))+ 
  labs(y = expression(Average~daily~flux~" "~(g~N[2]*O-N~ha^{-1}~day^{-1})), color='Field') +
  annotate(geom = "text", x=200, y=0.1, label= ("r-squared = 0.83\np-value < 0.001"),size = 6)+
   theme_bw() +
   theme(axis.text.x  = element_text(size=20, colour="black"),  
         axis.title.x = element_text(size = 20, vjust=-0.1, face = "bold"),
         axis.text.y = element_text(size=20, colour="black"),
         axis.title.y = element_text(vjust=1.8, size = 20, face = "bold"),
         legend.text = element_text(size = 20),
         legend.title = element_text("Field",size = 20),
         legend.position = c(0.08, 0.85),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         strip.text.x = element_text(size = 20, colour = "black", face = "bold", angle = 0))

#+ I(x^2)
flux_tn <- mutate(flux_tn, total_n2=total_n^2)
mod <- lm(total_flux~total_n, data = flux_tn)
summary(mod)
quadmod <- lm(total_flux~total_n+total_n2, data = flux_tn)
summary(quadmod)
logmod <- lm(log(total_flux)~log(total_n), data = flux_tn)
summary(logmod)
#season 1
total_flux1 <- j %>%
  group_by(field,plot) %>% 
  filter(season == 1) %>% 
  summarise(total_flux = sum(mean_flux,na.rm=T)/268) %>% 
  mutate(total_flux = total_flux*2.4)
total_flux1$field <- as.factor(total_flux1$field)

total_n1 <- t_n %>% 
  group_by(field,plot) %>%
  filter(season==1) %>% 
  summarize(total_n = sum(total_n)) %>% 
  mutate(total_n = total_n*1.12085) 
total_n1$field <- as.factor(total_n1$field)

flux_tn1 <- left_join(total_flux1, total_n1)

total_flux2 <- j %>%
  group_by(plot, field) %>% 
  filter(season==2) %>% 
  summarise(total_flux = sum(mean_flux,na.rm = T)/224) %>% 
  mutate(total_flux = total_flux*2.4)
total_flux2$field <- as.factor(total_flux2$field)

total_n2 <- t_n %>% 
  group_by(field,plot) %>%
  filter(season==2) %>% 
  summarize(total_n = sum(total_n)) %>% 
  mutate(total_n = total_n*1.12085) 
total_n2$field <- as.factor(total_n2$field)

flux_tn2 <- left_join(total_flux2, total_n2)

ggplot(data = flux_tn1, aes(total_n,total_flux))+
  geom_point(aes(color = field), size = 2) +
  stat_smooth(method = "lm",
              formula = y ~ x + poly(x, 2) - 1) +
  labs(x = "Total Nitrogen (kg/ha)", y = "Average Daily Flux (ng N/hr/cm)") +
  labs(color='Field')
flux_tn1 <- mutate(flux_tn1,totaln2=(total_n^2))
mod1 <- lm(total_flux~total_n, data = flux_tn1)
quad1 <- lm(total_flux~total_n+totaln2, data = flux_tn1)
log1 <- lm(log(total_flux)~total_n,data = flux_tn1)
summary(mod1)
summary(quad1)
summary(log1)

ggplot(data = flux_tn2, aes(total_n,total_flux))+
  geom_point(aes(color = field), size = 2) +
  stat_smooth(method = "lm",
              formula = y ~ x + poly(x, 2) - 1) +
  labs(x = "Total Nitrogen (kg/ha)", y = "Average Daily Flux (ng N/hr/cm)") +
  labs(color='Field')
mod2 <- lm(total_flux~total_n, data = flux_tn2)
summary(mod2)
flux_tn2 <- mutate(flux_tn2,totaln2=(total_n^2))
quad2 <- lm(total_flux~total_n+totaln2,data=flux_tn2)
log2 <- lm(log(total_flux)~total_n,data=flux_tn2)
summary(quad2)
summary(log2)

### Figure 2. Avereage daily flux by N rate by year
ggplot(data = flux_tn1, aes(total_n,total_flux, color = field))+
  geom_point(size = 6, shape = 21, stroke = 3) +
  geom_point(data = flux_tn2, size = 6, shape = 22, stroke = 3)+
  stat_smooth(data = flux_tn1, aes(total_n, total_flux), color = "grey39", size = 1, method = "lm",
              formula = y ~ x + poly(x, 2) - 1, se = FALSE, inherit.aes = F) +
  stat_smooth(data = flux_tn2, aes(total_n, total_flux), color = "grey57", size = 1, method = "lm",
              formula = y ~ x + poly(x, 2) - 1, se = FALSE, inherit.aes = F) +

  labs(x = expression("Average nitrogen rate (kg N"~ha^{-1}~year^{-1}*")"))+ 
  labs(color='Field')+
  labs(y = expression(Average~daily~flux~" "~(g~N[2]*O-N~~ha^{-1}~day^{-1})), color='Field') +
  annotate(geom = "text", x=230, y=.2, label= ("Year 1\nr-squared = 0.67\np-value < 0.001"), size = 6)+
  annotate(geom = "text", x=230, y=.9, label= ("Year 2\nr-squared = 0.76\np-value < 0.001"), size = 6)+
  theme_bw() +
  theme(axis.text.x  = element_text(size=20, colour="black"),  
        axis.title.x = element_text(size = 20, vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=20, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        legend.title = element_text("Field",size = 20),
        legend.position = c(0.08, 0.85),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
        strip.text.x = element_text(size = 20, colour = "black", face = "bold", angle = 0))
  
