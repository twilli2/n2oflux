lys_data %>%
  group_by(date, compound) %>%
  mutate(count = count(concentration))
  summarize(mean = mean(concentration, na.rm = T))
            
lys_data %>% 
  summarize(sum(!Is.na('concentration')))

df <- na.omit(lys_data)

count <- df %>% 
  group_by(date, compound, concentration) %>% 
  summarize(count = n())

lys_sample <- count %>% 
  group_by(date, compound) %>% 
  summarize(sum = sum(count))
write.csv(lys_sample, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/R/Water Sample Count.csv")

flux_data <- na.omit(flux_data)

count1 <- flux_data %>% 
  group_by(date, compound,flux) %>% 
  summarize(count = n())

flux_sample <- count1 %>% 
  group_by(date, compound) %>% 
  summarize(sum = sum(count)) %>% 
  mutate(samp  = sum * 4)
  
write.csv(flux_sample, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/R/Gas Sample Count.csv")
