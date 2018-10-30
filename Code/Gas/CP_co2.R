library(tidyverse)
cp_co2 <- filter(flux_data,compound == "co2", plot == "C" | plot == "P") %>%  
   select_all()
cp_co2 <- cp_co2 %>% 
  group_by(field, plot, date) %>% 
  summarize(mean_flux = mean(flux)) %>% 
  filter(mean_flux != "NA") %>%
  mutate(cs = cumsum(mean_flux))
View(cp_co2)

ggplot(cp_co2, aes(x = date, y = cs, color = plot)) +
  geom_point(position = "jitter") +
  geom_line() +
  labs(x = "Date", y = "Cumulutive flux (mg C/meter/hour)")+
  facet_wrap(~field)

library(RColorBrewer)
ggplot(cp_co2,aes(x = plot, y = mean_flux, fill = plot)) +
  geom_boxplot(color = "black", size = .05, alpha = 0.7) +
  stat_boxplot(geom = "errorbar") +
  facet_grid(.~field)

ggplot(data = cp_co2) +
  geom_point(mapping = (aes(x = date, y = mean_flux, color = plot))) +
  geom_line(aes(x=date,y=mean_flux,color=plot)) +
  facet_wrap(~field)
  
C_co2 <- filter(flux_data, compound == "co2", plot == "C")
P_co2 <- filter(flux_data, plot == "P")    

ggplot(data = C_co2)+
  geom_smooth(mapping = (aes(x = soiltemp, y = flux)))
                         +
  geom_smooth(data = P_co2, mapping = (aes(x = soiltemp, y = flux)))

              