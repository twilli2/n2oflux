df <- joined_env_data
df$year <- year(df$date)
df$month <- month(df$date)
precip_data <- df %>% 
  filter(year == 2018 & month < 8)
n <- df %>% 
  filter(year == 2019)
precip_data <- bind_rows(precip_data,n)

pd <- precip_data %>% 
  group_by(year, field) %>% 
  summarise(tp = sum(total_precip),am = mean(mean_avg_moist),maxm5=max(max_moist), at = mean(mean_avg_temp),maxt=max(max_temp_5),maxt10=max(max_temp_10),mint5=min(min_temp_5),mint10=min(min_temp_10))
pd
mod1<- lm(total_precip~field,precip_data)
summary(mod1)
anova(mod1)
TukeyHSD(aov(mod1))

mod2<- glm(mean_avg_moist~year+field-1,family = gaussian, data=sd1)
summary(mod1)
anova(mod1)

a<- left_join(df,flux_tn3)
ab <- a %>% 
  group_by(date,field) %>% 
  summarize(av_temp = mean(mean_avg_temp),av_moist=mean(mean_avg_moist),mean_flux = mean(mean_flux,na.rm = T),total_n = mean(total_n))
ab %>% 
  group_by(date,field) %>% 
  summarize(mean_flux = mean_flux, cum_mean = cummean(av_moist))
abmod<- lm(mean_flux~total_n++av_temp+av_moist+field,data = ab)
summary(abmod)
