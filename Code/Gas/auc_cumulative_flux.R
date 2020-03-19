library(tidyverse)
library(ggplot2)
library(readxl)
library(plotrix)
library(lubridate)
library(zoo)
library(flextable)

df <- n2oflux_cum %>% 
  filter(field == 4, plot ==0)
x <- df$date
y <- df$cum_flux
id <- order(x)

AUC <- sum(diff(x[id])*rollmean(y[id],2))


new_data$field <- 4
new_data$plot <-0
new_data$AUC <- AUC 

auc_data_table <- bind_rows(auc_data_table,new_data)
write.csv(auc_data_table,"~/Dropbox/Lab data/S Willamette GWMA Dropbox/Tidy Data/auc_data_table.csv")
###
df <- season2 %>% 
  filter(field == 2, plot == 100)
x <- df$jdate
y <- df$daily_kg_ha
id <- order(x)
AUC <- sum(diff(x[id])*rollsum(y[id],2))

#new_data <- as.data.frame(AUC)
new_data$field <- 2
new_data$plot <- 100
new_data$AUC <- AUC 
#auc_data_table <- new_data
auc_data_table <- bind_rows(auc_data_table,new_data)
auc_data_table
write.csv(auc_data_table,"~/Dropbox/Lab data/S Willamette GWMA Dropbox/Tidy Data/auc_data_table.csv")

sum(diff(x[id]))
rollsum(y[id],2)
x.Date <- as.Date(paste(2004, rep(1:4, 4:1), sample(1:28, 10), sep = "-"))
x.Date
x <- zoo(rnorm(12), x.Date)
x
