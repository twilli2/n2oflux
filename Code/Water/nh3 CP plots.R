library(ggplot2)
lys_data_CP_nh3 <- filter(lys_data,compound == 'nh3', treatment == 'C' | treatment == 'P', concentration > 0)
lys_data_CP_nh3

lys_data_CP_nh3 <- lys_data_CP_nh3 %>% 
  group_by(field, date, treatment) %>%  
  summarize(mean_conc = mean(concentration, na.rm = T),
  total_conc = sum(concentration,na.rm = T),
  max_conc = max(concentration,na.rm = T),
  sd = sd(concentration, na.rm = T))

F1<- filter(lys_data_CP_nh3, treatment == "C"|treatment == "P", field == 1)
summary(F1)
mod<-lm(mean_conc ~ treatment, F1)
summary(mod)
anova(mod)
#Not significant
F2<- filter(lys_data_CP_nh3, treatment == "C"|treatment == "P", field == 2)
summary(F2)
mod<-lm(mean_conc ~ treatment, F2)
summary(mod)
anova(mod)
#Not significant

ggplot(lys_data_CP_nh3, aes(date, mean_conc, color = treatment)) +
  geom_line(na.rm = TRUE, size = 1) +
  geom_point() +
  scale_colour_discrete(labels = c("Conventional","Slow release")) +
  labs(
    x = "Month", 
    y = "Ammonium (mg N/L)", 
    caption = ("Ammonium concentration over time by treatment and field"), 
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
