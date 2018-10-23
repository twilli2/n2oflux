ggplot(joined_env_data) +
  geom_line(aes(x = date, y = total_precip)) +
  geom_smooth(aes(x= date, y = max_temp_5)) +
  facet_wrap(~field)

joined_env_data
joined_env_data$date <- as.Date(joined_env_data$date)

cp_n2o_st$date <- as.Date(cp_n2o$date)
flux_env<- left_join(cp_n2o, joined_env_data, by = c("date", "field"))
ggplot(flux_env) +
  geom_line(aes(x = date, y = total_precip)) +
  geom_line(aes(x = date, y = mean_flux, color = plot)) + 
  geom_smooth(aes(x = date, y = max_temp_5)) +
  facet_wrap(~field)

flux_env1 <- left_join(joined_env_data,cp_n2o, by = c("date", "field"))
ggplot(flux_env1) +
  geom_line(aes(x = date, y = total_precip)) +
  geom_line(aes(x = date, y = mean_flux, color = plot), na.rm = T, size = 1) +
  geom_smooth(aes(x = date, y = max_temp_5)) +
  geom_point(aes(x = date, y = soiltemp), color = "red") +
  facet_wrap(~field)
