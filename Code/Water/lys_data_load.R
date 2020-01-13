library(tidyverse)
library(lubridate)
library(forcats)
library(ggplot2)
#install.packages("stats")

#C:/Users/twilli2/

lys_data <- read_csv("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Lysimeter Data/Lysimeter Data.csv",
  col_types = cols(compound = col_factor(levels = c("nh3",
  "no3")), date = col_date(format = "%m/%d/%Y"),
  field = col_factor(levels = c("F1",
  "F2", "F3", "F4")), treatment = col_factor(levels = c("0",
  "25", "50", "75", "100", "C","P")),lys_num = col_character()))
lys_data$month <- month(lys_data$date, label=TRUE)
lys_data$field <- gsub("F","",lys_data$field)
lys_data$year <- year(lys_data$date)

vol_dat <- read_csv("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Lysimeter Data/vol_dat.csv",
col_types = cols(Date = col_date(format = "%m/%d/%Y"),
Field = col_factor(levels = c("1",
"2", "3", "4")), Treatment = col_factor(levels = c("CV",
"PA", "N0", "N2", "N5", "N7",
"N1")), Volume = col_number(), 'X6' = col_skip(), 'X7' = col_skip()))

vol_dat$Date<-as.Date(vol_dat$Date)

vol_dat$Treatment <- gsub('CV','C', vol_dat$Treatment)
vol_dat$Treatment <- gsub('PA', 'P',vol_dat$Treatment)
vol_dat <- rename(vol_dat, treatment = Treatment)
vol_dat <- rename(vol_dat, date = Date)
vol_dat <- rename(vol_dat, field = Field)
vol_dat <- rename(vol_dat, plot = Lysimeter)

CP_V <- subset(vol_dat, treatment == "C" | treatment ==  "P")
CP_V$treatment <- as.factor(CP_V$treatment)
CPN <- subset(lys_data, treatment == "C" | treatment == "P")

lys_data_CP <- left_join(CP_V,CPN)
lys_data_CP <- filter(lys_data_CP,concentration > 0)
