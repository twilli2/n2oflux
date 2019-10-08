library(readxl)
library(tidyverse)
library(lubridate)


vol_dat <- read_csv("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Lysimeter Data/vol_dat.csv",
col_types = cols(Date = col_date(format = "%m/%d/%Y"),
Field = col_factor(levels = c("1",
"2", "3", "4")), Treatment = col_factor(levels = c("CV",
"PA", "N0", "N2", "N5", "N7",
"N1")), Volume = col_number()))

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
df <- left_join(CP_V,CPN)
utils::View(df)
write.csv(df, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/R/all_lys_data.csv")

no3 <- df %>% 
  filter(compound == "no3") %>% 
  group_by(field, date, treatment, plot) %>% 
  mutate(mass = concentration*Volume)
no3 <- no3 %>%
  group_by(date,field, treatment) %>% 
  summarize(TV = sum(Volume,na.rm = T),TC = sum(mass,na.rm = T), mean = mean(concentration,na.rm = T))
no3 <- no3 %>%
  group_by(date,field,treatment) %>% 
  mutate(vol_conc = TC/TV)
utils::View(no3)
write.csv(no3, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/R/Volume weighted NO3 conc.csv")

meanCV <- df %>%
  filter(compound == "no3") %>%
  group_by(field,date,treatment) %>% 
  summarize(mean = mean(concentration, na.rm = T))
utils::View(meanCV)

nh4 <- df %>% 
  filter(compound == "nh3") %>% 
  group_by(field, date, treatment, plot) %>% 
  mutate(mass = concentration*Volume)

nh4 <- nh4 %>%
  group_by(date,field, treatment) %>% 
  summarize(TV = sum(Volume,na.rm = T),TC = sum(mass,na.rm = T))
  
nh4 <- nh4 %>%
  group_by(date,field,treatment) %>% 
  mutate(vol_conc = TC/TV)
utils::View(nh4)
write.csv(nh4, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/R/Volume weighted NH4 conc.csv") 
