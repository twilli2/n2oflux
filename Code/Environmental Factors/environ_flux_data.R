library(tidyr)
flux_data$date <- as.Date(flux_data$date)
flux_env <- left_join(flux_data, joined_env_data, by = c("date","field"))

summary(flux_env)

cp_n2o <- filter(flux_env, compound == 'n2o', plot == 'C'|plot == 'P') %>% 
  select_all() %>% 
  group_by(date, field, plot) %>% 
  summarize(mean_flux = mean(flux, na.rm = T), median_flux = median(flux,na.rm = T), mean_temp = mean(max_temp_5, na.rm = T),
            mean_precip = mean(total_precip, na.rm = T), mean_moist = mean(avg_moist,na.rm = T)) %>% 
  group_by(field,plot) %>% 
  summarize(mean_flux = mean(mean_flux, na.rm = T), median_flux = median(median_flux, na.rm = T),temp = mean(mean_temp, na.rm = T), precip = mean(mean_precip,na.rm = T), moist = mean(mean_moist, na.rm = T))

cp_n2o


a <- flux_data %>%
  filter(compound == 'co2') %>% 
  group_by(field, plot) %>% 
  summarize(median = median(flux, na.rm = T), mean = mean(flux, na.rm = T))

b <- flux_data %>%
  filter(compound == 'n2o', plot == "C"|plot == "P")

c <- flux_data %>%
  filter(compound == 'n2o', plot != "C" | plot != "P")

%>% 
  group_by(field, plot) %>% 
  mutate(sd = sd(flux, na.rm = T)) %>% 
  summarize(median = median(flux, na.rm = T), mean = mean(flux, na.rm = T), sd = sd(flux))

ggplot(b) +
  geom_boxplot(aes(x = plot, y = flux), outlier.shape = NA, notch = T)+
  coord_cartesian(ylim = c(-3, 6))+
  facet_grid(~field)

ggplot(c) +
  geom_boxplot(aes(x = plot, y = flux), outlier.shape = NA, notch = T)+
  coord_cartesian(ylim = c(-3, 6))+
  facet_grid(~field)

ggplot(joined_env_data) +
  geom_line(aes(x = date, y = total_precip)) +
  geom_smooth(aes(x= date, y = max_temp_5)) +
  facet_wrap(~field)

flux_data$date <- as.Date(flux_data$date)
cp_n2o_sum$date <- as.Date(cp_n2o_sum$date)
flux_env<- left_join(cp_n2o_sum, joined_env_data, by = c("date", "field"))

ggplot(flux_env) +
  geom_line(aes(x = date, y = total_precip)) +
  geom_line(aes(x = date, y = mean_flux, color = plot)) + 
  geom_smooth(aes(x = date, y = max_temp_5)) +
  facet_wrap(~field)

flux_env <- left_join(p, cp_n2o, by = c("date", "field"))
flux_env
ggplot(flux_env) +
  geom_line(aes(x = date, y = total_precip)) +
  geom_line(aes(x = date, y = mean_flux, color = plot), na.rm = T, size = 1) +
  geom_smooth(aes(x = date, y = max_temp_5)) +
  geom_point(aes(x = date, y = soiltemp), color = "red") +
  facet_wrap(~field)
