library(tidyverse)
library(lubridate)
library(forcats)
library(ggplot2)
#C:/Users/twilli2/
lys_data <- read_csv("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Lysimeter Data/Lysimeter Data.csv",
  col_types = cols(compound = col_factor(levels = c("nh3",
  "no3")), date = col_date(format = "%m/%d/%Y"),
  field = col_factor(levels = c("F1",
  "F2", "F3", "F4")), treatment = col_factor(levels = c("0",
  "25", "50", "75", "100", "C","P")),lys_num = col_character()))
lys_data$month <- month(lys_data$date, label=TRUE)
lys_data$field <- gsub("F","",lys_data$field)
lys_data_CP <- filter(lys_data,compound == 'no3', treatment == 'C' | treatment == 'P', concentration > 0)
