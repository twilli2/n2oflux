flux_data$month <- month(flux_data$date, label=T)
flux_data
report <- flux_data %>% 
  group_by(compound, field, plot, date, month) %>%
  filter(month == "Mar"| month == "Apr"| month == "May"| month == "Jun") %>% 
  summarize(mean_flux = mean(flux, na.rm = T)) %>% 
  group_by(date)
report
utils::View(report)
install.packages("xlsx") 
library(xlsx)
?write.xlsx
write.csv(report, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/R/report2.csv")

utils::View(lys_data)
report2 <- lys_data %>% 
  group_by(compound, field, treatment, date, month) %>% 
  filter(month == "Mar"| month == "Apr"| month == "May"| month == "Jun") %>%
  summarize(mean_conc = mean(concentration, na.rm = T)) %>% 
  group_by(date)

utils::View(report2)
write.csv(report2, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/R/report3.csv")
