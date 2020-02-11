library(tidyverse)
library(ggplot2)
library(readxl)
library(plotrix)
library(lubridate)
library(readxl)
fert_data <- read_excel("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Fert-Yield data/Fertilizer Plans.xlsx",
sheet = "total fert", col_types = c("date",
"text", "text", "numeric", "numeric",
"numeric", "numeric", "numeric"))

df <- fert_data %>% 
  filter(plot == "100") %>% 
  select_all() %>% 
  group_by(date, field) %>% 
  summarize(N100 = sum(tn_ac)) %>% 
  arrange(field)

df <- df %>% 
  mutate('100' = N100, '75' = N100 * 0.75, '50' = N100 * 0.50, '25' = N100 * 0.25, '0' = N100 * 0)

df$field <- as.factor(df$field)
df$plot <- as.factor(df$field)
df$date <- as.Date(df$date)
df$month <- month(df$date) 
df$year <- year(df$date)
df$quarter <- quarter(df$date)

df

y1a <- df %>% 
  filter(quarter == "4" & year == "2017"| quarter == "2" & year == "2017")
y1b <- df %>% 
  filter(quarter == "1" & year == "2018" | quarter == "2" & year == "2018")
y1 <- bind_rows(y1a,y1b)
y1 <- gather(y1, plot, total_n, '100':'0')
y1$season <- "1" 

#y1 <- y1 %>%  group_by(date, field, plot) %>%   summarise(total = cumsum(total_n))

y2a <- df %>% 
  filter(quarter == "4" & year == "2018")
y2b <- df %>% 
  filter(quarter == "1" & year == "2019"|quarter == "2" & year == "2019")
y2 <-bind_rows(y2a,y2b)
y2 <- gather(y2, plot, total_n, '100':'0')
#y2 <- select(y2,1,2,7,8)
y2$season <- "2" 

total_n <- bind_rows(y1,y2)
total_n <- total_n [-3]

sum_n <- total_n %>%
  filter(plot == 100) %>% 
  group_by(season, field) %>%
  summarize(sum_n = sum(total_n)) %>% 
  mutate(sum_n*1.12085)
 
t_n <- total_n %>%
  group_by(date,month,year,field,plot) %>% 
  summarise(total_n = sum(total_n))

tn <- select(total_n,1,2,7,8,9)

#y2$date <- as.POSIXct(y2$date)

ggplot(cum_n, aes(x = date, y = cum_n, color = plot)) +
  geom_point(size = 2, shape = 1) +
  geom_line(size = 1)+
  facet_grid(~field)
#n2o gradient only

j <- select(p,1,2,3,8,16)

j$plot[j$plot == "N0"] <- "0"
j$plot[j$plot == "N25"] <- "25"
j$plot[j$plot == "N50"] <- "50"
j$plot[j$plot == "N75"] <- "75"
j$plot[j$plot == "N100"] <- "100"
j$plot[j$plot == "Conv"] <- "C"
j$plot[j$plot == "PA"] <- "P"
#mean flux by date and plot
j <- filter(j, n2o_rsq >= 0.30 & n2o_rsq <= 1) %>% 
  filter(plot != "C" & plot != "P") %>% 
  group_by(field, plot, date) %>% 
  summarise(mean_flux = mean(n2o_flux)) %>% 
  arrange(match(plot, c("0","25","50", "75", "100")),-desc(field), -desc(date))

write.csv(j,"~/Dropbox/Lab data/S Willamette GWMA Dropbox/Tidy Data/mean_flux.csv")

co2flux <- filter(flux_data, co2_rsq >= 0.30 & co2_rsq < 1, plot != "C" & plot != "P") %>% 
  select(1,2,3,9,6,15) %>%
  group_by(date,field,season) %>% 
  summarise(max_flux = max(co2_flux),min_flux = min(co2_flux))

total_n <- t_n %>% 
  group_by(date, field) %>% 
  summarize(total_n = sum(total_n)) %>% 
  mutate(total_n = total_n*1.12085) 

ggplot()+
  geom_smooth(data = filter(co2flux, season == 1), aes(x = date, y = max_flux, color = field),se = F)+
  geom_smooth(data = filter(co2flux, season == 1), aes(x = date, y = min_flux, color = field),se = F)+
  geom_smooth(data = filter(co2flux, season == 2), aes(x = date, y = max_flux, color = field),se = F)+
  geom_smooth(data = filter(co2flux, season == 2), aes(x = date, y = min_flux, color = field),se = F)+
  geom_point(aes(x = date, y = total_n*0.00095, color = field), data = filter(total_n))+
  theme_bw() +
  theme(axis.text.x  = element_text(size=16, colour="black"),  
        axis.title.x = element_text(size = 16, vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text("Field",size = 16))+
        #strip.text.x = element_text(size = 16, colour = "black", face = "bold", angle = 0))+
   labs(y = expression(Minimum~and~Maximum~Flux~" "~CO[2]-C~~g~~m^{-2}~h^{-1}), x = "", color='Field')


n2oflux <- filter(flux_data, n2o_rsq >= 0.30 & n2o_rsq < 1, plot != "C" & plot != "P") %>% 
  select(1,2,3,9,8,16) %>%
  group_by(date,field,season) %>% 
  summarise(max_flux = max(n2o_flux),min_flux = min(n2o_flux))

ggplot()+
  geom_smooth(data = filter(n2oflux, season == 1), aes(x = date, y = max_flux, color = field),se = F)+
  geom_smooth(data = filter(n2oflux, season == 1), aes(x = date, y = min_flux, color = field),se = F)+
  geom_smooth(data = filter(n2oflux, season == 2), aes(x = date, y = max_flux, color = field),se = F)+
  geom_smooth(data = filter(n2oflux, season == 2), aes(x = date, y = min_flux, color = field),se = F)+
  geom_point(aes(x = date, y = total_n*0.1, color = field), data = filter(total_n))+
  theme_bw() +
  theme(axis.text.x  = element_text(size=16, colour="black"),  
        axis.title.x = element_text(size = 16, vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text("Field",size = 16))+
        #strip.text.x = element_text(size = 16, colour = "black", face = "bold", angle = 0))+
  #ylab=expression(Production~rate~" "~mu~moles~NO[3]^{-1}-N~Kg^{-1}) 
  labs(y = expression(Minimimum~and~Maximum~Flux~" "~N[2]~O-N~~ng~cm^{-2}~h^{-1}), x = "", color='Field')
  

         
j2$month <- month(j2$date)
j2$year <- year(j2$date)
j2 <- select(j2,1,2,4,5,6)
total_n <- select(total_n,2,3,4,6,7)
f_n <- full_join(j2,total_n, by = c("field","month","year","plot"))
j3 <- select(p,1,2,3,4,15,6,16,8)
j3 <- filter(j3, plot != "C" & plot != "P")
#head(j3)
#write.csv(j3, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/gradiant_flux_data_final.csv")


###mean cumulative flux per plot/sample date and fertilization dates

ggplot(j, aes(x = date, y = flux, color = plot)) +
  geom_point(size = 2, shape = 1) +
  geom_line()+
  geom_point(data = filter(total_n, plot == "0"), aes(x = date, y = cum_n), color = "red", shape = 2)+
  #geom_point(data = y2, aes(x = date, y = cum_n, color = plot)) +
  #geom_line(data = y2, aes(x = date, y = cum_n)) +
  facet_grid(~field)



ggplot(total_flux,aes(total_flux,plot))+
  geom_point()

lm(flux~date,j)
j2 <- j
j2$month <- month(j2$date)
j2$year <- year(j2$date)
j2 <- select(j2,1,2,4,5,6)
total_n <- select(total_n,2,3,4,6,7)
f_n <- full_join(j2,total_n, by = c("field","month","year","plot"))

write.csv(j2,file="total_flux~total_n.csv")

ggplot(f_n,aes(x = total_n, y = mean_flux))+
  geom_point()+
  geom_smooth()+
  facet_grid(~field)

f1 <- j2 %>% 
  filter(field == 1)
mdl1 <- lm(total_flux~total_n, data = f1)
summary(mdl1)
mdl2 <- lm(total_flux~total_n+ I(total_n^2), data = f1)
summary(mdl2)
mdl3 <- lm(total_flux~total_n + I(total_n^2) + I(total_n^3), data = f1)
summary(mdl3)
mdl4 <- lm(total_flux~I(total_n^2), data = f1)
summary(mdl4)
mdl4 <- lm(total_flux~I(total_n^2), data = f1)    

mdl1 <- glm(total_flux~total_n, data = j2) #linear
summary(mdl1)
mdl2 <- glm(total_flux~total_n+ I(total_n^2), data = j2)#quadratic
summary(mdl2)
mdl3 <- glm(total_flux~total_n + I(total_n^2) + I(total_n^3), data = j2)#polynomial
summary(mdl3)
mdl4 <- glm(total_flux~I(total_n^2), data = j2)
summary(mdl4)
    


#### 
total_n <- total_n %>% 
  group_by(season,field, plot) 

ggplot(total_n, aes(x = plot, y = total_n, color = field)) +
  geom_point(size = 2, shape = 1) +
  geom_line()+
  facet_wrap(~season)

grad_n2o <- filter(flux_env, compound == 'n2o', plot == '0'|plot == '25'|plot == '50'|plot == '75'|plot == '100') %>% 
  select_all() %>% 
  group_by(date, field, plot) %>% 
  summarize(mean_flux = mean(flux, na.rm = T), median_flux = median(flux,na.rm = T))
grad_n2o$quarter <- quarter(grad_n2o$date)
grad_n2o$year <- year(grad_n2o$date)

s1a <- grad_n2o %>% 
  filter(quarter == "4" & year == "2017")
s1b <- grad_n2o %>% 
  filter(quarter == "1:3" & year == "2018")
s1 <- bind_rows(s1a,s1b)
s1$season <- "1" 
s2a <- grad_n2o %>% 
  filter(quarter == "4" & year == "2018")
s2b <- grad_n2o %>% 
  filter(quarter == "1:3" & year == "2019")
s2 <- bind_rows(s2a,s2b)
s2$season <- "2" 
flux <- bind_rows(s1,s2) 

flux <- flux %>% 
  group_by(season,field, plot)

df <- left_join(total_n,flux)

ggplot(df, aes(x = total_n, y = median_flux, color = field)) +
  geom_point(size = 2, shape = 1) +
  geom_line()+
  facet_wrap(~season)

ggplot(df, aes(x = total_n, y = mean_flux, color = field)) +
  geom_point(size = 2, shape = 1) +
  geom_line()+
  facet_wrap(~season)

ggplot(df, aes(x = total_n, y = "median_flux")) +
  geom_col()+
  facet_grid(~season)


fert <- subset(fert_data, plot == "C"|plot == 'P')

ggplot(fert, aes(x = date, y = tn_ac, color = plot)) +
  geom_point(size = 2, shape = 1) +
  geom_line()
  facet_wrap(~field)

cp_n2o <- filter(flux_data,compound == "n2o", plot == "C" | plot == "P", flux > -24) %>%   
select_all()

cp_n2o$date <- as.Date(cp_n2o$date)

cp_n2o_sum <- cp_n2o %>% 
  group_by(field, plot, date) %>% 
  summarize(mean_flux = mean(flux), sd = sd(flux,na.rm = T)) %>% 
  filter(mean_flux != "NA") %>%
  mutate(cs = cumsum(mean_flux))

cp_n2o_sum

n2o_fert <- full_join(fert, cp_n2o_sum)

n2o_fert <- mutate(n2o_fert, log_f = log(mean_flux))

ggplot(n2o_fert)  +
  geom_point(aes(x = date, y = tn_ac, color = plot)) +
  geom_smooth(aes(x = date, y = mean_flux*10, color = plot))+
  facet_wrap(~field)

n2o_fert
utils::View(n2o_fert)

ggplot(n2o_fert, aes(x = date, y = mean_flux, color = plot)) +
  geom_point(size = 2, shape = 1) +
  geom_line(size = 1.2) +
  scale_color_discrete(name = "Treatment", labels = c("Conventional","Slow-release")) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=12, colour="black", face = "bold"),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0))+
  labs(x = NULL, y = "Average flux (ng N/cm/hr)")+
  geom_errorbar(aes(x = date, ymin = mean_flux - sd, ymax = mean_flux + sd), 
                size = .5, width = 0.3) +
  geom_point(data = n2o_fert, mapping = aes(x = date, y = fert, shape = plot), size = 3.0, 
             show.legend = F) +
  facet_wrap(~field)

cp_n2o_1 <-  filter(cp_n2o, flux > -24) %>%  
  select_all() %>% 
  group_by(field, plot) %>% 
  summarize(mean_flux = mean(flux), sd = sd(flux,na.rm = T), se = std.error(flux,na.rm = T))

