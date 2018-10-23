library(ggplot2)
lys_data_CP_nh3 <- filter(lys_data,compound == 'nh3', treatment == 'C' | treatment == 'P')
lys_data_CP_nh3
CP_nh3_by_treat <- lys_data_CP_nh3 %>% 
  group_by(field, date, treatment) %>% 
  summarise(conc = mean(concentration,na.rm = T))
CP_nh3_by_treat
ggplot(CP_nh3_by_treat, aes(date, conc, color = treatment)) +
  geom_line(na.rm = TRUE, size = 1) +
  geom_point() +
  scale_colour_discrete(labels = c("Conventional","Slow release")) +
  labs(
    x = "Month", 
    y = "Ammonia (mg N/L)", 
    caption = ("Ammonia concentration over time by treatment and field"), 
    color = ("Fertilizer Type")) +
  theme_bw() +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 2))) +
  facet_wrap(~field)

  library(modelr)
options(na.action = na.warn)

F1<- filter(lys_data_CP_nh3, treatment == "C"|treatment == "P", field == 4)
mod<-lm(concentration ~ treatment, F1)
summary(mod)
anova(mod)
#no signif dif between nor within fields by treatment
