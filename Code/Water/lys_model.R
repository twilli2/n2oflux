joined_env_data$date <- as.Date(joined_env_data$date)
joined_env_data
lys_data
lys_data$field <- as.factor(lys_data$field)
CP

step(mean_conc~treatment+date,data = CP, direction = "both")

p <- joined_env_data %>% 
  group_by(field, date) %>% 
  summarize(mean_avg_5cm = mean(avg_moist)) %>% 
  filter(mean_avg_5cm != "NA") %>%
  mutate(moist_cs = cumsum(mean_avg_5cm))

lys_env_data<-right_join(p, CP)
lys_env_data
#is mean no3 conc related to cumulative precipitation? hard to say, attach precip data from weather service
m <- lm(mean_conc ~ precip_cs, lys_env_data)
summary(m)
ggplot(data = lys_env_data, color = treatment) +
  geom_point(aes(x = precip_cs, y = mean_conc))+
  facet_grid(~field)
#is mean no3 conc related to cum soil moisture?
m <- lm(mean_conc ~ moist_cs + treatment, lys_env_data)
summary(m)
ggplot(data = lys_env_data) +
  geom_point(aes(x = moist_cs, y = mean_conc, color = treatment), size = 3)+
  facet_grid(~field)
#is mean flux related to cum soil moisture?
m <- lm(mean_flux ~ moist_cs + plot, flux_env)
summary(m)
ggplot(data = flux_env) +
  geom_point(aes(x = moist_cs, y = mean_flux, color = plot), size = 3) +
  facet_grid(~field)
#volume weighted averages by treatment
m <- lm(vol_conc ~ treatment, no3)
summary(m)
ggplot(data = no3, color = treatment) +
  geom_point(aes(x = treatment, y = vol_conc))+
  facet_grid(~field)
hist(no3$vol_conc)
shapiro.test(log(no3$vol_conc))
