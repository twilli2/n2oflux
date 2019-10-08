library(modelr)
library(tidyverse)
library(splines)
library(e1071)
library(ggpubr)
#normality test
library(tidyverse)
library(ggplot2)
install.packages("pastecs")
library(pastecs)
flux_datan <- filter(flux_data, compound =="n2o")
stat.desc(flux_datan)
summary(flux_datan)

N0 <- flux_datan %>%
  filter(plot == "0") 
stat.desc(N0)

N25 <- flux_datan %>%
  filter(plot == "25") 
stat.desc(N25)
%>% 
  sapply(mean, na.rm = T) %>% 
  summary()

flux_datan %>% 
  filter(plot == "0") %>% 
  summary()
plot(flux_datan$flux)
ggplot(flux_datan)+
  geom_boxplot(aes(plot, flux))
ggplot(flux_datan)+
  geom_col(aes(plot, flux))
ggplot(flux_datan)+
  geom_bar(aes(flux))


grad_n2o <- filter(flux_data, compound == "n2o", plot != "C" & plot != "P") %>% 
  group_by(date,field,plot)

hist(grad_n2o$flux)
shapiro.test((grad_n2o$flux))
grad_n2o <- grad_n2o %>% 
  mutate(lflux = log(flux))
grad_n2o$plot <- factor(grad_n2o$plot, order = T, levels = c("0","25","50","75","100")) 

summary(grad_n2o)
hist(grad_n2o$lflux)
shapiro.test((grad_n2o$lflux))
ggplot(grad_n2o,aes(x = plot, y = log(flux), fill = plot)) +
  geom_boxplot(color = "black", size = .05, alpha = 0.7) +
  facet_wrap(~field)
kruskal.test(flux~plot, grad_n2o)


m = lm(flux~plot,grad_n2o)
anova(m)
TukeyHSD(m)
#simple linear model flux and soil temp
m = lm(flux~soiltemp, flux_datan)
coeffs = coefficients(m); coeffs 
stchange = 1           # degree of temp change 
flux = coeffs[1] + coeffs[2]*stchange 
flux
newdata = data.frame(soiltemp= 1) # wrap the parameter
predict(m, newdata)    # apply predict
summary(m)$r.squared 
#for each increase of soil temp by 1 degree, flux goes up by 0.74 ng N/m/H
ggplot(flux_datan) + 
  geom_point(aes(x = soiltemp, y = flux))

df <- filter(grad_n2o) %>% 
  group_by(plot) %>% 
  summarize(flux_mean = mean (flux,na.rm = T),sd = sd(flux,na.rm = T))

df



#stat_boxplot(geom = "errorbar")

hist(grad_n2o$flux, breaks = 50)
summary.data.frame(grad_n2o)
qqnorm(grad_n2o$flux)
qqline(grad_n2o$flux)
?hist
abline
ggdensity(cp_n2o$soiltemp)
ggqqplot(cp_n2o$soiltemp)
shapiro.test(grad_n2o$flux)

s1 <- sample(grad_n2o$flux, 1000)
shapiro.test(s1)


ggdensity(cp_n2o$soiltemp)
ggqqplot(cp_n2o$soiltemp)
shapiro.test(cp_n2o$soiltemp)

cp_n2o$st1.5 <- cp_n2o$soiltemp + 1.5
cp_n2o$stlog <- log(cp_n2o$st1.5)

ggdensity(cp_n2o$stlog)
ggqqplot(cp_n2o$stlog)
shapiro.test(cp_n2o$stlog)

ggdensity(cp_n2o$mean_flux)
ggqqplot(cp_n2o$mean_flux)
shapiro.test(cp_n2o$mean_flux)


cp_n2o$f2 <- cp_n2o$mean_flux + 2
cp_n2o$flog <- log(cp_n2o$f2)

ggdensity(cp_n2o$flog)
ggqqplot(cp_n2o$flog)
shapiro.test(cp_n2o$flog)

skewness(cp_n2o$stlog)
?skewness
hist(cp_n2o$soiltemp)
hist(cp_n2o$stlog)
summary(cp_n2o$soiltemp)

ggplot(cp_n2o) +
  geom_point(aes(x = soiltemp, y = flog, color = plot))

n <- cp_n2o 

n %>% 
  ggplot(aes(soiltemp, flog)) +
  geom_line() +
  ggtitle("Full data = ")

n_mod <- lm(flog ~ soiltemp + plot, data = n)

n %>% 
  add_predictions(n_mod) %>% 
  ggplot(aes(soiltemp, pred)) +
  geom_smooth() +
  ggtitle("Linear trend +")

n %>% 
  add_residuals(n_mod) %>% 
  ggplot(aes(soiltemp, resid, color = plot)) +
  geom_hline(yintercept = 0, color = "white", size = 3) +
  geom_line() +
  ggtitle("Remaining pattern")+
  geom_smooth(color = "black")

ggplot(n, aes(plot)) +
  geom_point(aes(y = flog)) +
  geom_point(data = n_mod, aes(y = pred), color = "red",size = 4)

ggplot(n, aes(soiltemp, flog))+
  geom_point(aes(color = plot))+
  facet_wrap(~plot)

mod2 <- lm(flux ~ soiltemp + plot, data = filter(n, plot !="C" & plot !="P"))
mod3 <- lm(flux ~ soiltemp * plot, data = filter(n, plot !="C" & plot !="P"))
grid <- filter(n, plot !="C" & plot !="P") %>%
  data_grid(soiltemp, plot) %>% 
  gather_predictions(mod2, mod3)
grid
ggplot(n, aes(soiltemp, flux, color = plot))+
  geom_point() +
  geom_line(data = grid, aes(y = pred), size = 2)+
  facet_wrap(~ model)

by_field <- n %>% 
  group_by(field) %>% 
  nest()
field_model <- function(df) {
  lm(flux ~ soiltemp, data = df)
}
models <- map(by_field$data, field_model)
by_field <- by_field %>% 
  mutate(model = map(data, field_model))
by_field <- by_field %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_field
resids <- unnest(by_field, resids)
resids %>% 
  ggplot(aes(soiltemp, resid)) +
  geom_line(aes(group = field), alpha = 1/3) +
  geom_smooth(se = FALSE)
resids %>% 
  ggplot(aes(soiltemp, resid, group = field)) +
  geom_line(alpha = 1/3) +
  facet_wrap(~field)

broom::glance(n_mod)
glance <- by_field %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
library(broom)
glance %>% 
  arrange(r.squared)
glance %>% 
  ggplot(aes(field, r.squared)) +
  geom_jitter(width = 0.5)

library(splines)
model_matrix(n, flux ~ ns(soiltemp, 2))
n
ggplot(n,aes(soiltemp, flux)) +
  geom_point()
mod5 <- lm(flux ~ ns(soiltemp, 5), data = n)
mod5
grid <- n %>% 
  data_grid(x = seq_range("soiltemp", n = 50, expand = 0.1)) %>% 
  gather_predictions(mod5, .pred = "flux")

ggplot(n, aes(soiltemp, flux)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ field)
View(n)
n2<- n %>% 
  mutate(lflux = log2(flux), lsoiltemp = log2(soiltemp))
mod_1 <- lm(lflux ~ lsoiltemp, data = n2)
grid <- n2 %>%
  data_grid(soiltemp = seq_range(soiltemp, 20)) %>% 
  mutate(lsoiltemp = log2(soiltemp)) %>% 
  add_predictions(mod_1, "lflux") %>% 
  mutate(flux = 2 ^ lflux)
ggplot(n2,aes(soiltemp,flux))+
  geom_hex(bins = 15) +
  geom_line(data = grid, color = "red", size = 1)
n2 <- n2 %>% 
  add_residuals(mod_1, "lresid")
ggplot(n2, aes(lsoiltemp, lresid))+
  geom_hex(bins = 15)
ggplot(n2, aes(field,lresid)) + geom_boxplot()
ggplot(n2, aes(plot,lresid)) + geom_boxplot()

mod1 <- lm(lflux ~ plot * soiltemp * date, data = n2)
n2 %>% 
  add_residuals(mod1, "resid") %>% 
  ggplot(aes(plot, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_boxplot()
summary(mod1)
anova(mod1)

mod <- lm(flux ~ plot * ns(soiltemp, 5), data = n)

n %>% 
  data_grid(plot, soiltemp = seq_range(soiltemp, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(soiltemp, pred, colour = plot)) + 
  geom_line() +
  geom_point()

n2 %>% 
  ggplot(aes(x = lflux)) +
  geom_histogram()
