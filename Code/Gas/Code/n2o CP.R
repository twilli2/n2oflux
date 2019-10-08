#install dplyr
install.packages("dplyr")
#call plyr and tidyr to be able to use ddply to summarise
library(plyr)
library(tidyr)
#save latest flux data as CSV
#import CSV dataset
library(readr)
#import and assign modes and classes
Slopes_and_Flux_CSV <- read_csv("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux CSV.csv")
col_types = cols(mol = col_factor(levels = c("co2", 
                                             "n2o")), date = col_date(format = "%m/%d/%Y"), 
                 chamber = col_factor(levels = c("1", 
                                                 "2", "3", "4")), field = col_factor(levels = c("1", 
                                                                                                "2", "3", "4")), plot = col_factor(levels = c("0", 
                                                                                                                                              "25", "50", "75", "100","P", "C")),flux = as.numeric)
fluxes<-Slopes_and_Flux_CSV
utils::View(fluxes)
sapply(fluxes,class)
sapply(fluxes,mode)
fluxes$field<-as.factor(fluxes$field)
fluxes$plot<-as.factor(fluxes$plot)
fluxes$date<-as.Date.character(fluxes$date,format = "%m/%d/%Y")
fluxes$flux<-as.numeric(fluxes$flux)
#IGNORE another way but messes with plot because its mixed
#char_data <- read.csv("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux CSV.csv", stringsAsFactors = F)
#num_data <- data.frame(data.matrix(char_data))
#numeric_columns <- sapply(num_data,function(x){mean(as.numeric(is.na(x)))<0.5})
#final_data <- data.frame(num_data[,numeric_columns], char_data[,!numeric_columns])
#final_data$plot <- as.character(final_data$plot)
#final_data$field <- as.factor(final_data$field)
#final_data$chamber <- as.factor(final_data$chamber)
#final_data$date <- as.Date.character(final_data$date,format = "%m/%d/%Y")
#final_data$compound <- as.factor(final_data$compound)

#sapply(final_data,mode)
#sapply(final_data,class)

#asign dataset to "fluxes"
#fluxes<-final_data
print(fluxes)
#make it a table
dplyr::tbl_df(fluxes)
#subset removing rows with NA
fluxes<-dplyr::filter(fluxes,!is.na(fluxes$flux))
fluxes<-dplyr::filter(fluxes,!is.na(fluxes$soiltemp))

#get a glimpse of your data
dplyr::glimpse(fluxes)
#get a table pop out
utils::View(fluxes)
#or view as a table summary
print(fluxes)

#subset based on compound, keep all variables
n2o <- subset(fluxes, compound=="n2o",
              select=field:soiltemp)
co2<- subset(fluxes, compound=="co2",
             select=field:soiltemp)
#add a column, in this case, add 10 and get log of flux
n2o$n2o10<-(n2o$flux)+10
n2o$logn2o<-log(n2o$n2o10)
co2$co210<-(co2$flux+10)
co2$logco2<-log(co2$co210)
co2<-dplyr::filter(co2,!is.na(co2$logco2))

utils::View(n2o)
utils::View(co2)
#create table just based on certain values of certain column
gradientn2o<- subset(n2o, plot=="0" | plot=="25" |plot=="50"| plot=="75"|plot=="100",
                     select=field:logn2o)
gradientn2o$plot <- as.factor(gradientn2o$plot)

CPn2o<- subset(n2o, plot=="C" | plot=="P",
               select=field:logn2o)


gradco2<-subset(co2, plot=="0" | plot=="25" |plot=="50"| plot=="75"|plot=="100",
                select=field:logco2)
CPco2<- subset(co2, plot=="C" | plot=="P",
               select=field:logco2)

install.packages("ggplot2")
library(datasets)
library(ggplot2)
#order categorical rows how you want
gradientn2o$plot = ordered(gradientn2o$plot, levels=c("0","25","50","75","100"))
gradco2$plot=ordered(gradco2$plot,levels=c("0","25","50","75","100"))
utils::View(gradientn2o)
#how to make a fancy boxplot
library(RColorBrewer)
m<-ggplot(gradientn2o,aes(x=plot,y=logn2o,fill=field))+
  geom_boxplot(fill="blue", color="goldenrod2",size=.05,alpha = 0.7,
               outlier.color = "#1F3552", outlier.shape = 20,notch=FALSE)+
  scale_x_discrete(name="% of Fertilizer")+
  scale_y_continuous(name="LOG N2O FLUX\n(ng N cm-2 h-1)",#line break in axis title
                     breaks = seq(1.5, 3.5, .25),
                     limits=c(1.5, 3.5))+ 
  labs(caption="Boxplot of flux by % fertilizer by field")+
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
m

n<-ggplot(CPn2o,aes(x=plot,y=logn2o,fill=field))+
  geom_boxplot(fill="blue", color="goldenrod2",size=.05,alpha = 0.7,
               outlier.color = "#1F3552", outlier.shape = 20,notch=FALSE)+
  scale_x_discrete(name="% of Fertilizer")+
  scale_y_continuous(name="LOG N2O FLUX\n(ng N cm-2 h-1)",#line break in axis title
                     breaks = seq(1.5, 3.5, .25),
                     limits=c(1.5, 3.5))+ 
  labs(caption="Boxplot of flux Conventional VS Precision Ag by field")+
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
n
#all fields
modn<-lm(logn2o~plot,CPn2o)
summary(modn)
#field 1
CPn2o1<-subset(CPn2o,y=logn2o, field=="1",
               select=field:logn2o)
modn1<-lm(logn2o~plot,CPn2o1)
summary(modn1)
#field 2
CPn2o2<-subset(CPn2o,y=logn2o,field=="2")
modn2<-lm(logn2o~plot,CPn2o2)
summary(modn2)
#field 3
CPn2o3<-subset(CPn2o,y=logn2o,field=="3")
modn3<-lm(logn2o~plot,CPn2o3)
summary(modn3)
#field 4
CPn2o4<-subset(CPn2o,y=logn2o,field=="4")
modn4<-lm(logn2o~plot,CPn2o4)
summary(modn4)

#co2 gradient plots
utils::View(gradco2)
c<-ggplot(gradco2,aes(x=plot,y=logco2,fill=field))+
  geom_boxplot(fill="blue", color="goldenrod2",size=.05,alpha = 0.7,
               outlier.color = "#1F3552", outlier.shape = 20,notch=FALSE)+
  scale_x_discrete(name="% of Fertilizer")+
  scale_y_continuous(name="CO2 FLUX\n(mg C cm-2 h-1)",#line break in axis title
                     breaks = seq(2.3, 2.35, .05),
                     limits=c(2.3, 2.35))+ 
  labs(caption="Boxplot of flux by % fertilizer by field")+
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
c

p<-ggplot(CPco2,aes(x=plot,y=logco2,fill=field))+
  geom_boxplot(fill="blue", color="goldenrod2",size=.05,alpha = 0.7,
               outlier.color = "#1F3552", outlier.shape = 20,notch=FALSE)+
  scale_x_discrete(name="% of Fertilizer")+
  scale_y_continuous(name="CO2 FLUX\n(mg C cm-2 h-1)",#line break in axis title
                     breaks = seq(2.3, 2.35, .05),
                     limits=c(2.3, 2.35))+ 
  labs(caption="Boxplot of flux by % fertilizer by field")+
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
p


#summarise by field, treatment and date (quote marks MATTER)
treatment<- ddply(CPn2o$plot=="C", c("field":"logn2o"), data=CPn2o,summarise,
                  N  = length(`"logn2o"`),
                  mean = mean(`"logn2o"`),
                  sd   = sd(`"logn2o"`),
                  se = sd / sqrt(N),
                  min = min(`"logn2o"`),
                  max = max(`"logn2o"`)
)
utils::View(treatment)
#from here you can see sds at a glance and note areas where outliers may need to be checked
plot(n2o$flux~n2o$date)
ggplot(data=CPn2o)+
  geom_point(mapping = aes(x = date, y = flux, color=field))
