utils::View(mtcars)
# mod1<-lm(dependent~independent,data)

#simple scatter plot
plot(flux~date,CP)
plot(flux~plot,CP)

plot(flux~soiltemp,data=gradientn2o, pch = 16, cex = 1.3, col = "blue",
     main = "N2O FLUX PLOTTED AGAINST SOIL TEMPERATURE", 
     xlab = "TEMP (C)", ylab = "N2O FLUX (ng N cm-2 h-1)")
#In the above code, the syntax pch = 16 creates solid dots, 
#while cex = 1.3 creates dots that are 1.3 times bigger than the default 
#where cex = 1
mod1<-lm(flux~soiltemp, data=gradientn2o)
abline(mod1)
#add that r2 line
summary(mod1)
plot(flux~plot,data=gradientn2o, pch = 16, cex = 1.3, col = "red",
     main = "N2O FLUX PLOTTED AGAINST GRADIENT PLOT", 
     xlab = "% ", ylab = "N2O FLUX (ng N cm-2 h-1)")
mod2 <-lm(flux~plot, gradientn2o)
abline(mod2)  
summary(mod2)

boxplot(flux~plot,gradientn2o)

?ylim
#mix categorical and continuous
mixed_model = lm(flux ~ plot + field, gradientn2o)
summary(mixed_model)
plot(mixed_model)


