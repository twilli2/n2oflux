#lysimter check nh
lys_check_nh <- filter(lys_data,compound == 'nh3') %>% 
  group_by(field, date, treatment, lys_num) %>% 
  summarise(conc = mean(concentration,na.rm = T))
lys_check_nh
ggplot(lys_check_nh, aes(lys_num, conc)) +
  geom_boxplot() +
  labs(
    x = "Lysimter Number", 
    y = "Ammonia (mg N/L)", 
    caption = ("Ammonia concentration over time by treatment and lysimeter"), 
    color = ("Lysimeter")) +
  theme_bw() +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 2)))

ggplot(lys_check_nh, aes(lys_num, conc)) +
  geom_col () +
  labs(
    x = "Lysimter Number", 
    y = "Ammonia (mg N/L)", 
    caption = ("Ammonia concentration over time by treatment and lysimeter"), 
    color = ("Lysimeter")) +
  theme_bw() +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 2)))
