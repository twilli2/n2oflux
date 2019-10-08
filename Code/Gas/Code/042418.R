utils::View(mtcars)
# mod1<-lm(dependent~independent,data)
mod1<-lm(flux~soiltemp,n2o)
print(mod1)
summary(mod1)
plot(mod1)
#simple scatter plot
plot(logn2o~plot,n2o)
n2o %>%    
  group_by (plot) %>%   
  summarise(avg = mean(logn2o)) %>%   
  arrange(avg)
#filter and assign
mod<-(dplyr:: filter(n2o, plot=="PA")) 
print(mod)
plot(mod)
summary(mod)
utils::View(n2o)

plot(flux~soiltemp,data=n2o, pch = 16, cex = 1.3, col = "blue",
     main = "N2O FLUX PLOTTED AGAINST SOIL TEMPERATURE", 
     xlab = "TEMP (C)", ylab = "N2O FLUX (ng N cm-2 h-1)")
#In the above code, the syntax pch = 16 creates solid dots, 
#while cex = 1.3 creates dots that are 1.3 times bigger than the default 
#where cex = 1
abline(mod1)
#add that r2 line
aov_model = lm(flux ~ plot, n2o)
summary(aov_model)
boxplot(flux~plot,n2o)
?ylim
#mix categorical and continuous
mixed_model = lm(flux ~ plot + soiltemp, n2o)
summary(mixed_model)
plot(mixed_model)

