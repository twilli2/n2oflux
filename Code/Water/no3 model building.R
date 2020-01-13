F1<- filter(lys_data_CP, treatment == "C"|treatment == "P", field == 1)
summary(F1)
mod<-lm(concentration ~ treatment+year, F1)
summary(mod)
anova(mod)
#Not significant
F2<- filter(lys_data_CP, treatment == "C"|treatment == "P", field == 2)
summary(F2)
mod<-lm(concentration ~ treatment+year, F2)
summary(mod)
anova(mod)
#Not significant
F3<- filter(lys_data_CP, treatment == "C"|treatment == "P", field == 3)
summary(F3)
mod<-lm(concentration ~ treatment, F3)
summary(mod)
anova(mod)
#signif dif in field 3
#F-statistic:  6.62 on 1 and 62 DF,  p-value: 0.01249
F4<- filter(lys_data_CP, treatment == "C"|treatment == "P", field == 4)
summary(F4)
mod<-lm(concentration ~ treatment, F4)
summary(mod)
anova(mod)
#Not significant
T<- filter(lys_data_CP, treatment == "C"|treatment == "P")
summary(T)
mod<-lm(concentration ~ treatment * field, T)
summary(mod)
anova(mod)
#fields 3 and 4 significantly highter than 2 and 1
#field 3 significantly lower in P treatment

install.packages("hexbin")

no3_2 <- lys_data %>% 
  group_by(field, date, compound, treatment) %>% 
  summarise(mean_conc = mean(concentration,na.rm=T)) %>% 
  mutate(lconc = log2(mean_conc)) %>% 
  filter(compound == "no3", treatment == "C"| treatment == "P", mean_conc >= 0)
         
no3_2

lys_data_no <- subset(lys_data, compound == "no3")
lys_data_no$field <- as_factor(lys_data_no$field)
ggplot(lys_data_no)+
  geom_point(aes(treatment,concentration))

mod1 <- lm(lconc ~ treatment + field, data = no3_2)
summary(mod1)
mod2 <- lm(concentration ~ treatment * field, data = lys_data_no)

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
###
###
all_mod <- lm(total_n~treatment,data=tn)
tidy(all_mod)

F1<- filter(tn, treatment == "C"|treatment == "P", field == 1)
summary(F1)
mod<-lm(total_n ~ treatment+year, F1)
summary(mod)
anova(mod)
#year, not treatment
F2<- filter(tn, treatment == "C"|treatment == "P", field == 2)
summary(F2)
mod<-lm(total_n ~ treatment+year, F2)
summary(mod)
anova(mod)
#Not significant
F3<- filter(tn, treatment == "C"|treatment == "P", field == 3)
summary(F3)
mod<-lm(total_n ~ treatment, F3)
summary(mod)
anova(mod)
#signif dif in field 3
#F-statistic:  6.62 on 1 and 62 DF,  p-value: 0.01249
F4<- filter(tn, treatment == "C"|treatment == "P", field == 4)
summary(F4)
mod<-lm(total_n ~ treatment, F4)
summary(mod)
anova(mod)
#Not significant
T<- filter(lys_data_CP, treatment == "C"|treatment == "P")
summary(T)
mod<-lm(concentration ~ treatment * field, T)
summary(mod)
anova(mod)
#fields 3 and 4 significantly highter than 2 and 1
#field 3 significantly lower in P treatment

install.packages("hexbin")

no3_2 <- lys_data %>% 
  group_by(field, date, compound, treatment) %>% 
  summarise(mean_conc = mean(concentration,na.rm=T)) %>% 
  mutate(lconc = log2(mean_conc)) %>% 
  filter(compound == "no3", treatment == "C"| treatment == "P", mean_conc >= 0)
         
no3_2

lys_data_no <- subset(lys_data, compound == "no3")
lys_data_no$field <- as_factor(lys_data_no$field)
ggplot(lys_data_no)+
  geom_point(aes(treatment,concentration))

mod1 <- lm(lconc ~ treatment + field, data = no3_2)
summary(mod1)
mod2 <- lm(concentration ~ treatment * field, data = lys_data_no)

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

