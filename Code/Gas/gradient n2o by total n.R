library(tidyverse)
library(ggplot2)
library(readxl)
library(plotrix)
library(lubridate)
library(readxl)
#loading fertilizer data
fert_data <- read_excel("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Fert-Yield data/Fertilizer Plans.xlsx",
sheet = "total fert", col_types = c("date","text", "text", "numeric", "numeric",
                      "numeric", "numeric", "numeric"))
#creates, simplifies and summarizes data frame 
#into totals by field
df <- fert_data %>% 
  filter(plot == "100") %>% 
  select_all() %>% 
  group_by(date, field) %>% 
  summarize(N100 = sum(tn_ac)) %>% 
  arrange(field)
#calculates rates based on percentage of 100% plot
df <- df %>% 
  mutate('100' = N100, '75' = N100 * 0.75, '50' = N100 * 0.50, '25' = N100 * 0.25, '0' = N100 * 0)
#insures data are in proper format
df$field <- as.factor(df$field)
df$plot <- as.factor(df$field)
df$date <- as.Date(df$date)
df$month <- month(df$date) 
df$year <- year(df$date)
df$quarter <- quarter(df$date)
#adds season columns
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
flux_data <- read.csv("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Tidy Data/flux_data.csv")
n2oflux <- filter(flux_data, n2o_rsq >= 0.30 & n2o_rsq < 1, plot != "C" & plot != "P", n2o_flux>=0) %>%
  group_by(season,date,field,plot) %>%
  mutate(n2o_flux*2.4) %>% 
  summarise(mean_flux = mean(n2o_flux), sd = sd(n2o_flux)) %>% 
  arrange(match(plot, c("0","25","50", "75", "100")),-desc(field), -desc(date))

n2oflux$plot <- factor(n2oflux$plot,levels = c("0","25","50", "75", "100"))
n2oflux$season <- factor(n2oflux$season, levels = c("1","2"))
n2oflux$date <- as.Date(n2oflux$date)

n2oflux1 <- n2oflux %>%
  filter(season==1) %>% 
  group_by(field,plot) %>% 
  mutate(cum_flux = cumsum(mean_flux)) 

n2oflux2 <- n2oflux %>%
  filter(season==2) %>% 
  group_by(field,plot) %>% 
  mutate(cum_flux = cumsum(mean_flux)) 
j <- bind_rows(n2oflux1,n2oflux2)

#makes sure names match
j$plot[j$plot == "N0"] <- "0"
j$plot[j$plot == "N25"] <- "25"
j$plot[j$plot == "N50"] <- "50"
j$plot[j$plot == "N75"] <- "75"
j$plot[j$plot == "N100"] <- "100"

#very little difference if
#mean gradient flux by date and plot with abherrant slopes are removed
#j <- filter(j, n2o_rsq >= 0.30 & n2o_rsq < 1 

#creates tally for standard error calculations
t <-   filter(flux_data, n2o_rsq >= 0.30 & n2o_rsq < 1, plot != "C" & plot != "P", n2o_flux>=0) %>%
  group_by(season,date,field,plot) %>% 
  tally() 
t$plot <- factor(t$plot,levels = c("0","25","50", "75", "100"))
t$season <- factor(t$season, levels = c("1","2"))
t$date <- as.Date(t$date)

#summarizes individual chamber flux data into average flux by plot

j <- left_join(j,t) 
j <- j %>% 
  mutate(se = sd/sqrt(n))
j$season <- as.character(j$season)

#daily mean flux 268 days in season 1, 224 days in season 2
#dividing total flux by number of days in sampling seasons
#multiplying by conversion factor to get kg/ha/day

total_flux <- j %>%
  group_by(field,plot) %>% 
  summarise(se = sum(se,na.rm=T)/492, total_flux = sum(mean_flux, na.rm=T)/492)
total_flux$field <- as.factor(total_flux$field)
##
total_flux_by_field <- j %>%
  group_by(season,field,plot) %>% 
  summarise(se = sum(se,na.rm=T)/246, total_flux = sum(mean_flux, na.rm=T)/246)
total_flux_by_field$field <- as.factor(total_flux_by_field$field)
#average n addition per season #
#multiplying to get kg/ha dividing by two seasons
total_n <- t_n %>% 
  group_by(field,plot) %>% 
  summarize(total_n = sum(total_n)) %>% 
  mutate(total_n = (total_n*1.12085)/2) 
total_n$field <- as.factor(total_n$field)
str(total_n)
str(total_flux)
flux_tn <- left_join(total_flux,total_n)

##
total_n_by_field <- t_n %>% 
  group_by(season,field,plot) %>% 
  summarize(total_n = sum(total_n)) %>% 
  mutate(total_n = (total_n*1.12085)) 
total_n_by_field$field <- as.factor(total_n_by_field$field)
flux_tn_by_field <- left_join(total_flux_by_field,total_n_by_field)
##all fields
log <- lm(log(total_flux+10)~total_n+season+field, data = flux_tn_by_field)
summary(log)
mod <- lm(total_flux~total_n+season+field, data = flux_tn_by_field)
quad <- lm(total_flux~total_n+(I(total_n^2))+season+field, data=flux_tn_by_field)
##
ggplot(data = flux_tn_by_field,aes(total_n,log(total_flux+10)))+
  geom_point()+
  stat_smooth()+
  facet_grid(field~season)
plot(log)
shapiro.test(log$residuals)
shapiro.test(mod$residuals)
library(lmtest)
bptest(mod)
ggplot(data = flux_tn_by_field,aes(total_n,total_flux))+
  geom_point()+
  stat_smooth()+
  facet_grid(field~season)
summary(mod)
library(sandwich)
coeftest(mod, vcov = vcovHC(mod, "HC1"))   # HC1 gives us the White standard errors
flux_tn_by_field$resids <- mod$residuals
flux.ols <- lm(log(resids^2) ~ log(total_n+.01)+field+season, data = flux_tn_by_field)
flux_tn_by_field$varfunc <- exp(flux.ols$fitted.values)
flux.gls <- lm(total_flux ~ total_n+field+season, weights = 1/sqrt(varfunc), data = flux_tn_by_field)
g <- ggplot(data = flux_tn_by_field, aes(y = total_flux, x = total_n)) + geom_point(col = 'blue')
g + geom_abline(slope = mod$coefficients[2], intercept = mod$coefficients[1], col = 'red') + geom_abline(slope = flux.gls$coefficients[2], intercept = flux.gls$coefficients[1], col = 'green')+
  geom_smooth(slope = quad$coefficients[2], intercept = quad$coefficients[1], col = 'blue')

summary(mod)
summary(flux.gls)
summary(quad)

## field 1
field1 <- filter(flux_tn_by_field,field == 1) %>% 
  mutate(total_n2=total_n^2)
mod <- lm(total_flux~total_n, data = field1)

summary(mod)
quadmod <- lm(total_flux~total_n+total_n2, data = field1)
summary(quadmod)
log <- lm(log(total_flux)~total_n+season, data = field1)
summary(log)
##
ggplot(data = field1,aes(total_n,log(total_flux)))+
  geom_point()+
  stat_smooth()+
  facet_grid(~season)
plot(log)
shapiro.test(log$residuals)
## field 2
field2 <- filter(flux_tn_by_field,field == 2) %>% 
  mutate(total_n2=total_n^2)
mod <- lm(total_flux~total_n, data = field2)
summary(mod)
quadmod <- lm(total_flux~total_n+total_n2, data = field2)
summary(quadmod)
##
ggplot(data = field2,aes(total_n,total_flux))+
  geom_point()+
  stat_smooth()
## field 3
field3 <- filter(flux_tn_by_field,field == 3) %>% 
  mutate(total_n2=total_n^2)
mod <- lm(total_flux~total_n, data = field3)
summary(mod)
quadmod <- lm(total_flux~total_n+total_n2, data = field3)
summary(quadmod)
##
ggplot(data = field3,aes(total_n,total_flux))+
  geom_point()+
  stat_smooth()
## field 4
field4 <- filter(flux_tn_by_field,field == 4) %>% 
  mutate(total_n2=total_n^2)
mod <- lm(total_flux~total_n, data = field4)
summary(mod)
quadmod <- lm(total_flux~total_n+total_n2, data = field4)
summary(quadmod)
##
ggplot(data = field4,aes(total_n,total_flux))+
  geom_point()+
  stat_smooth()

###Figure 1. Average daily flux as function of nitrogen rate by field. 
#Least squared quadratic regression
#Field 1 removed
ggplot(data = filter(flux_tn, field != 1), aes(total_n,total_flux))+
  geom_point(aes(color = field), size = 6, alpha = .50) +
  geom_errorbar(aes(ymin=total_flux-se, ymax=total_flux+se), width=5,
                 position=position_dodge())+
  #stat_smooth(method = "lm",
  #            formula = y~x+I(x^2), se = F)+
  stat_smooth(method = "lm",
              formula = y~x, se = F)+
  
              labs(x = expression("Average nitrogen rate (kg N"~~ha^{-1}~year^{-1}*")"))+ 
  labs(y = expression(Average~daily~flux~" "~(g~N[2]*O-N~ha^{-1}~day^{-1})), color='Field') +
  annotate("rect",xmin = 145,xmax = 200,ymin=-Inf,ymax=Inf, alpha=0.1, fill="blue")+
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

full_plot <- function (z){
  ggplot(data = filter(flux_tn, field == z), aes(total_n,total_flux))+
  geom_point(aes(color = field), size = 6, alpha = .50) +
  geom_errorbar(aes(ymin=total_flux-se, ymax=total_flux+se), width=5,
                 position=position_dodge())+
  #stat_smooth(method = "lm",
  #            formula = y~x+I(x^2), se = F)+
  stat_smooth(method = "lm",
              formula = y~x, se = F)+
  
              labs(x = expression("Average nitrogen rate (kg N"~~ha^{-1}~year^{-1}*")"))+ 
  labs(y = expression(Average~daily~flux~" "~(g~N[2]*O-N~ha^{-1}~day^{-1})), color='Field') +
  annotate("rect",xmin = 145,xmax = 200,ymin=-Inf,ymax=Inf, alpha=0.1, fill="blue")+
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

}

#removing field 1
flux_tn <- mutate(flux_tn, total_n2=total_n^2)
mod <- lm(total_flux~total_n, data = filter(flux_tn, field != 1))
summary(mod)
quadmod <- lm(total_flux~total_n+total_n2, data = filter(flux_tn, field != 1))
summary(quadmod)
# residuals are normally distributed
resids <- quadmod$residuals
shapiro.test(resids)

logmod <- lm(log(total_flux)~total_n, data = flux_tn)
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
  annotate("rect",xmin = 145,xmax = 200,ymin=-Inf,ymax=Inf, alpha=0.1, fill="blue")+
  annotate(geom = "text", x=235, y=.2, label= ("Year 1\nr-squared = 0.67\np-value < 0.001"), size = 6)+
  annotate(geom = "text", x=235, y=.9, label= ("Year 2\nr-squared = 0.76\np-value < 0.001"), size = 6)+
  
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
  

 #no averaging models
#individual fields season 2
mod3 <- lm(mean_flux~total_n, data = filter(flux_tn3, field ==1))
quad3 <- lm(total_flux~total_n+total_n^2,data=flux_tn2)
summary(quad3)
summary(mod3)

mod4 <- lm(total_flux~total_n, data = filter(flux_tn2, field ==2))
quad4 <- lm(total_flux~total_n+total_n^2,data=flux_tn2)
summary(quad4)
summary(mod4)

#all fields no averaging
flux_tn3 <- flux_tn3 %>% 
  mutate(total_n2 = total_n^2) 
mod4 <- lm(mean_flux~total_n, data = flux_tn3)
quad4 <- lm(mean_flux~total_n+total_n2,data=flux_tn3)
summary(quad4)
summary(mod4)

resids4 <- mod4$resid
shapiro.test(resids4)
resids4 <- quad4$resid
shapiro.test(resids4)

total_n2 <- t_n %>% 
  group_by(field,plot,season) %>% 
  summarize(total_n = sum(total_n)) %>% 
  mutate(total_n = (total_n*1.12085)) 
total_n2$season<- as.character(total_n2$season)

flux_tn3 <- left_join(j,total_n2, by = c("field","plot","season"))
flux_tn3 <- flux_tn3 %>%
  filter(season == 2) %>% 
  group_by(field,plot) %>% 
  summarize(mean_flux = mean(mean_flux,na.rm = T),total_n = mean(total_n))
### Figure 3. Avereage daily flux by N rate by year
ggplot(data = flux_tn3, aes(total_n,mean_flux, color = field))+
  geom_point(size = 6, shape = 21, stroke = 3)+
    #geom_point(data = flux_tn3, size = 6, shape = 22, stroke = 3)+
  stat_smooth(data = flux_tn3, aes(total_n, mean_flux), color = "grey39", size = 2, method = "lm",
              formula = y ~ x, se = FALSE, inherit.aes = F)+
  stat_smooth(data = flux_tn3, aes(total_n, mean_flux), color = "grey57", size = 1, method = "lm",
              formula = y ~ x + poly(x, 3) - 1, se = FALSE, inherit.aes = F) 

  labs(x = expression("Average nitrogen rate (kg N"~ha^{-1}~year^{-1}*")"))+ 
  labs(color='Field')+
  labs(y = expression(Average~daily~flux~" "~(g~N[2]*O-N~~ha^{-1}~day^{-1})), color='Field') +
  annotate("rect",xmin = 145,xmax = 200,ymin=-Inf,ymax=Inf, alpha=0.1, fill="blue")+
  annotate(geom = "text", x=235, y=.2, label= ("Year 1\nr-squared = 0.67\np-value < 0.001"), size = 6)+
  annotate(geom = "text", x=235, y=.9, label= ("Year 2\nr-squared = 0.76\np-value < 0.001"), size = 6)+
  
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
  
p$plot <- as.character(p$plot) 
p1 <- p %>% 
  filter(plot!="C"&plot!="P") %>% 
  arrange(date,field, match(plot, c("0","25","50","75","100"),chamber)) %>% 
  select(1,2,3,4,16)
t_n1 <- select(t_n,1,2,7,8)
t_n1$date <- as.Date(t_n1$date)
p1$date <- as.Date(p1$date)

write.csv(p1, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/p1.csv")

write.csv(t_n1, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/t_n1.csv")
