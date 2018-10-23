F1<- filter(lys_data_CP, treatment == "C"|treatment == "P", field == 4)
summary(F1)
mod<-lm(concentration ~ treatment, F1)
summary(mod)
anova(mod)
#signif dif in field 3
#F-statistic:  6.62 on 1 and 62 DF,  p-value: 0.01249
install.packages("hexbin")

no3_2 <- lys_data %>% 
  filter(compound == "no3", treatment == "C"| treatment == "P") %>% 
  mutate(lconc = log2(concentration))
ggplot(no3_2, aes(date,lconc)) +
  geom_hex(bins = 25)
lys_data_no <- subset(lys_data, compound == "no3")
lys_data_no$field <- as_factor(lys_data_no$field)
ggplot(lys_data_no)+
  geom_point(aes(treatment,concentration))
mod1 <- lm(concentration ~ treatment + field, data = lys_data_no )
mod2 <- lm(concentration ~ treatment * field, data = lys_data_no)
grid <- lys_data_no %>%
  data_grid(treatment, field) %>% 
  gather_predictions(mod1,mod2)
grid
ggplot(lys_data_no,aes(treatment, concentration, color = field))+
  geom_point() +
  geom_line(data = grid, aes(y=pred),group = "treatment") +
  facet_wrap(~ model)
library(MASS)
mod3 <- rlm(concentration~treatment+field,data=lys_data_no)
summary(mod3)
anova(mod3)
lys_data

select(lys_data, date, field, treatment, compound, concentration)

?select
mod <- rlm(concentration ~ compound + treatment + field, data = lys_data)
summary(mod)
anova(mod)
