install.packages("dplyr")
#call plyr and tidyr to be able to use ddply to summarise
library(plyr)
library(tidyr)
mod<-lm(logn2o~plot,gradientn2o)
plot(mod)
summary(mod)
#compare flux differences between chambers within each plot
#subset removing rows with NA
n2o<-dplyr::filter(n2o,!is.na(n2o$flux))

utils::View(gradientn2o)

#subset based on field, keep all variables
colnames(n2o)
F1 <- subset(n2o, field=="1",
              select=field:logn2o)

#remove entire columns
F1<- F1[,-4]

#subset based on factor
F1C<- subset(F1,plot=="Conv",
            select=flux&plot)

F1P<- subset(F1,plot=="PA",
            select=flux&plot)


F1Cmod<-lm(flux~chamber, data=F1C)
F1Pmod<-lm(flux~chamber, data=F1P)

#filter based on two plot criteria
dplyr::tbl_df(n2o)
n2o %>%    
  group_by(plot) %>%   
  summarise(avg = mean(logn2o)) %>%   
  arrange(avg)
dplyr:: filter(n2o, plot %in% "PA"|"Conv") 
utils::View(F1)
#Extract rows that meet logical criteria.
  PC<-F1%>%
  filter(plot=="PA"|plot=="Conv")
dplyr::glimpse(n2o)
utils::View(n2o)
F1mod<-lm(flux~plot,data=F1)
summary(F1Cmod)
summary(F1Pmod)
summary(F1mod)
par(mfrow=c(2,2))
plot(F1Cmod)
plot(F1Pmod)
plot(F1mod)

install.packages("car")
library(car)
vif(mod.1)
install.packages("leaps")
library(leaps)
b<-regsubsets(flux~.,data=n2o)
rst<-summary(b)
names(rst)
rst$adjr2
x<-1:6
plot(x=1:6,y=rst$adjr2,xlab="# of predictors", ylab='Adjusted R2')
plot(x,rst$cp,xlab="# of predictors", ylab='cp')
plot(x,rst$bic,xlab="# of predictors", ylab='BIC')
rst
mod.2<-lm(C~Temp+logNO3, data=dta)
plot(mod.2)
vif(mod.2)
anova(mod.full, mod.2)
summary(mod.2)
cor.matrix(dta[,c(5,8,9)])
step(mod.full)
