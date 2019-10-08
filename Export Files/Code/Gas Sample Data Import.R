library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(forcats)
library(ggplot2)

df <- read_excel("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 29/Data Field Visit 29.xlsx",
sheet = "Slopes 3.20.19", col_types = c("date",
"text", "text", "text", "text", "blank",
"blank", "numeric", "numeric", "blank",
"blank", "blank", "blank", "numeric",
"blank", "blank", "blank", "blank",
"blank", "blank", "blank", "blank",
"blank"))

col_types = cols(compound = col_factor(levels = c("nh3", "no3")), 
                 date = col_date(format = "%m/%d/%Y"),
                 field = col_factor(levels = c("F1","F2", "F3", "F4")), 
                 treatment = col_factor(levels = c("0","25", "50", "75", "100", "C","P")),
                 lys_num = col_character()))

renamedf %>% 
  group_by(`FIELD #`,) %>%
  summarise()
