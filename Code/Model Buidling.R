library(tidyverse)
df<- add_column(c("field","date","avg_soiltemp","avg_airtemp","soil_moisture","fert_type","fert_conc","lysimeter_id","chamber_id"))

df <- as.tibble(c(1,2,3,4))

df <- add_column(c("field", "date"))
df <- df$field(c(1,2,3,4))
summary(df)
print(df)
df$field <- c(1,2,3,4)
