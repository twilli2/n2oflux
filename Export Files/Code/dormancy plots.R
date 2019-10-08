library(tidyverse)
df <- filter(flux_data,compound == "co2") %>%  
  select_all()
n2o <- filter(flux_data,compound == "n2o") %>% 
  select_all()
df

df1 <- df %>% 
  group_by(date) %>% 
  summarize(max_flux = max(flux,na.rm = T), maxst = max(soiltemp,na.rm = T),
            mean_flux = mean(flux,na.rm = T), meanst = mean(soiltemp,na.rm = T),
            log_flux = log(mean_flux)+10, logst = log(meanst)+10)
View(df1)

ggplot(data = df1) +
  geom_point(mapping = (aes(x = date, y = log_flux+4))) +
  geom_line(aes(x=date,y = log_flux+4),color = "blue") +
  geom_point(mapping = (aes(x = date, y = logst-1.5))) +
  geom_line(mapping = (aes(x = date, y = logst-1.5)), color = "red")

ggplot(data = df1) +
  geom_point(mapping = (aes(x = date, y = log_flux))) +
  geom_line(aes(x=date,y = log_flux),color = "blue") +
  geom_point(mapping = (aes(x = date, y = logst))) +
  geom_line(mapping = (aes(x = date, y = logst)), color = "red")

cor.test(df1$meanst,df1$mean_flux)
#mean soil temperature and mean CO2 flux are 80% correlated?
lm <- lm(mean_flux ~ meanst, data = df1)
anova(lm)
ggplot(data = df1)+
  geom_smooth(aes(x = meanst, y = mean_flux))

cor.test(df$flux,df$soiltemp)

n2o <- n2o %>% 
  group_by(date) %>% 
  summarize(max_flux = max(flux,na.rm = T), maxst = max(soiltemp,na.rm = T),
            mean_flux = mean(flux,na.rm = T), meanst = mean(soiltemp,na.rm = T),
            log_flux = log(mean_flux)+10, logst = log(meanst)+10)
cor.test(n2o$meanst,n2o$mean_flux)
ggplot(data = n2o) +
  geom_line(aes(x = date, y = mean_flux), color = "blue") +
  geom_line(aes(x = date, y = meanst), color = "cyan3")
ggplot(data = n2o)+
  geom_smooth(aes(x = meanst, y = mean_flux))
