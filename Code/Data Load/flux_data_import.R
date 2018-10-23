library(readr)
flux_data <- read_csv("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux CSV.csv",
    col_types = cols(chamber = col_character(),
compound = col_factor(levels = c("co2",
"n2o")), date = col_datetime(format = "%m/%d/%Y"),
field = col_factor(levels = c("1","2","3","4")), plot = col_factor(levels = c("0",
"25", "50", "75", "100", "C",
"P"))))
View(flux_data)

