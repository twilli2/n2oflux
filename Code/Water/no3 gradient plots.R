library(ggplot2)
lys_data_grad<- filter(lys_data, compound == 'no3', treatment != 'C'& treatment != 'P') %>% 
  group_by(field)
grad_by_treat <- lys_data_grad %>% 
  group_by(treatment)
ggplot(grad_by_treat, aes(date, concentration, color = treatment)) +  
  geom_line(na.rm = TRUE, size = 1) +
  geom_point() +
  scale_colour_discrete() +
  labs(
    x = "Month", 
    y = "Nitrate/Nitrite (mg N/L)", 
    caption = ("Nitrate/Nitrite concentration over time by treatment and field"), 
    color = ("Percent Fertilizer")) +
  theme_bw() +
  facet_wrap(~field)

F1<- filter(lys_data_grad, treatment != "C"|treatment != "P", field == 1)
summary(F1)
mod<-lm(concentration ~ treatment, F1)
summary(mod)
anova(mod)
# Field 4 signif diff in 100 and 75 plot
# model F-statistic: 68.09 on 4 and 33 DF,  p-value: 1.789e-15
# treatment75    8.0108     1.9470   4.114 0.000243 ***
# treatment100  27.0833     1.9470  13.910 2.34e-15 ***
# Field 2 signif diff in 100 plot
# treatment100  3.18400    0.42986   7.407 9.29e-08 ***
# model F-statistic: 22.44 on 4 and 25 DF,  p-value: 5.753e-08
# Field 1 25 plot signif diff 
# treatment25   0.22000    0.08140   2.703   0.0137 *
View(count(lys_data_grad %>%
  group_by(field, treatment, concentration))) 
