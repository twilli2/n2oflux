library(ggplot2)
lys_data_grad_nh3<- filter(lys_data, compound == 'nh3', treatment != 'C'& treatment != 'P') %>% 
  group_by(field)
grad_by_treat_nh3 <- lys_data_grad_nh3 %>% 
  group_by(treatment)
ggplot(grad_by_treat_nh3, aes(date, concentration, color = treatment)) +  
  geom_line(na.rm = TRUE, size = 1)+
  geom_point(na.rm = T) +
  labs(
    x = "Month", 
    y = "Ammonia (mg N/L)", 
    caption = ("Ammonia concentration over time by treatment and field"), 
    color = ("Percent Fertilizer")) +
  theme_bw() +
  facet_wrap(~field)

F1<- filter(lys_data_grad_nh3, treatment != "C"|treatment != "P", field == 1)
summary(F1)
mod<-lm(concentration ~ treatment, F1)
summary(mod)
anova(mod)

#model building
ggplot(grad_by_treat_nh3)+
  geom_point(aes(treatment,concentration))
mod1 <- lm(concentration ~ treatment + field, data = grad_by_treat_nh3)
mod2 <- lm(concentration ~ treatment * field, data = grad_by_treat_nh3)
grid <- grad_by_treat_nh3 %>% 
  data_grid(treatment !="C"|treatment !="P", field) %>% 
  gather_predictions(mod1,mod2)
grid
ggplot(grad_by_treat_nh3,aes(treatment, concentration, color = field))+
  geom_point() +
  geom_line(data = grid, aes(y=pred)) +
  facet_wrap(~ model)
