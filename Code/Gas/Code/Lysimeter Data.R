#install dplyr
install.packages("dplyr")
#call plyr and tidyr to be able to use ddply to summarise
library(plyr)
library(tidyr)
#save latest flux data as CSV
#import CSV dataset
library(readr)
#import and assign modes and classes
lysdata <- read_csv("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Lysimeter Data/Lysimeter Data.csv")
col_types = cols(mol = col_factor(compound = c("no3","nh3")), 
    date = col_date(format = "%m/%d/%Y"), 
    field = col_factor(levels = c("F1","F2", "F3", "F4")), 
    treatment = col_factor(levels = c("0","25", "50", "75", "100","P", "C")),
    concentration = as.numeric)
utils::View(lysdata)
sapply(lysdata,class)
sapply(lysdata,mode)
lysdata$field<-as.factor(lysdata$field)
lysdata$treatment<-as.factor(lysdata$treatment)
lysdata$date<-as.Date.character(lysdata$date,format = "%m/%d/%Y")
lysdata$concentration<-as.numeric(lysdata$concentration)

lys<-dplyr::filter(lysdata,!is.na(lysdata$concentration))
utils::View(lys)
no3 <- subset(lys, compound=="no3",
              select=date:concentration)
nh3<- subset(lys, compound=="nh3",
             select=date:concentration)
no3$no310<-(no3$concentration)+10
no3$logno3<-log(no3$no310)
nh3$nh310<-(nh3$concentration)+10
nh3$lognh3<-log(nh3$nh310)
CPno3<- subset(no3, treatment=="C" | treatment=="P",
               select=date:logno3)
CPnh3<- subset(nh3, treatment=="C" | treatment=="P",
               select=date:lognh3)
utils::View(CPno3)

#all fields
modlno3<-lm(logno3~treatment,CPno3)
summary(modlno3)
#field 1
CPno31<-subset(CPno3,y=logno3, field=="F1",
               select=date:logno3)
modl1<-lm(logno3~treatment,CPno31)
summary(modl1)
#field 2
CPno32<-subset(CPno3,y=logno3, field=="F2",
               select=date:logno3)
modl2<-lm(logno3~treatment,CPno32)
summary(modl2)
#field 3
CPno33<-subset(CPno3,y=logno3, field=="F3",
               select=date:logno3)
modl3<-lm(logno3~treatment,CPno33)
summary(modl3)
#field 4
CPno34<-subset(CPno3,y=logno3, field=="F4",
               select=date:logno3)
modl4<-lm(logno3~treatment,CPno34)
summary(modl4)
anova(modl4)

hist(CPno3$logno3)
utils::View(CPno3)
install.packages("ggplot2")
library(datasets)
library(ggplot2)
histcp<-ggplot(data=CPno3,aes(x=,y=concentration))+
  geom_histogram
histcp
?hist
h2oCP<-ggplot(CPno3,aes(x=treatment,y=concentration,fill=field))+
  geom_boxplot(fill="blue", color="goldenrod2",size=.05,alpha = 0.7,
               outlier.color = "#1F3552", outlier.shape = 20,notch=FALSE)+
  scale_x_discrete(name="Treatment")+
  scale_y_continuous(name="LOG NO3 mL/L",#line break in axis title
                     breaks = seq(1.5, 3.5, .25),
                     limits=c(1.5, 3.5))+ 
  labs(caption="Boxplot of nitrate by plot by field")+
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(size=0.25, colour = "lightgray"),
        panel.grid.minor = element_line(size=0.1,color="lightgray"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12))+
  facet_grid(.~field)
h2oCP
plot(CPno34$concentration~CPno34$treatment)
dotchart(CPno31$concentration)
