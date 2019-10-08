library(ggplot2)
lys_data_grad<- filter(lys_data, treatment != 'C'& treatment != 'P', compound =='no3') %>% 
  group_by(field)

grad_by_treat <- lys_data_grad %>% 
  group_by(treatment)

ggplot(grad_by_treat, aes(date, concentration, color = treatment)) +  
  geom_point(na.rm = TRUE, size = 1) +
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

mod<-lm(concentration ~ treatment, lys_data_G)
summary(mod)
coeffs = coefficients(mod); coeffs
summary(mod)$r.squared
anova(mod)
kruskal.test(concentration ~ treatment, data = lys_data_grad)
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
n2
n2 <- grad_by_treat %>% 
  group_by(field, treatment, compound) %>% 
  summarise(mean = mean(concentration, na.rm = T), 
            sd = sd(concentration, na.rm = T))

ggplot(data = n2, mapping = aes(treatment, mean, fill = compound)) +
  geom_point() +
  theme(legend.position = "right") +
  scale_fill_hue(h=c(500,50),name = "Compound", labels = c("Ammonia","Nitrate"))+
  labs(x = "Treatment", y = "Average N (ppm)") +
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=14, colour="black", face = "bold"),  
        axis.title.x = element_text(size = 14,vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0))

ggplot(data = n2, mapping = aes(treatment, mean, fill = compound)) +
  geom_col() +
  theme(legend.position = "right") +
  scale_fill_hue(h=c(500,50),name = "Compound", labels = c("Ammonia","Nitrate"))+
  labs(x = "Treatment", y = "Average N (ppm)") +
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=14, colour="black", face = "bold"),  
        axis.title.x = element_text(size = 14,vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0))+
  facet_wrap(~field)

