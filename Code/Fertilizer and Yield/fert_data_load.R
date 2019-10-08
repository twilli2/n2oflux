library(readxl)
fert_data <- read_excel("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Fert-Yield data/Fertilizer Plans.xlsx",
                        sheet = "total fert", col_types = c("date",
                                                            "text", "text", "numeric", "numeric",
                                                            "numeric", "numeric", "numeric"))

fert_data
library(tidyverse)
#a successful function
k <- vector("list",4)

j <- function(x) {
  a <- x * 0.75
  b <- x * 0.5
  c <- x * 0.25
  d <- x * 0
  k <- c(x,a,b,c,d)
  print(k)
}  

j(100)

as.vector(plot,"list")

a <- fert_data %>% 
  filter(plot == "100") 
  
a$plot <- as.numeric(a$plot)

b <- cbind(a$plot)
c <- cbind(a$tn_ac)
 b
c
full_join(b,c)
c <- mutate_all(j(c))
a
library(lubridate)
df <- left_join(fert_data, flux_data)
df <- df$date = col_datetime(format = "%m/%d/%Y")

View(df)
df$month <- mutate(month = month(date))
library(tidyverse)
df <- filter(df,compound == "co2", plot == "C") %>%
  select_all()

df

df <- df %>% 
  group_by(date, field, flux, soiltemp, tn_ac) %>% 
  summarize()
            
View(df)
