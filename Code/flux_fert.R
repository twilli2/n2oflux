library(tidyverse)
library(ggplot2)
library(readxl)
library(plotrix)
library(lubridate)
install.package("trap.rule")
#sum of flux by gradient plot sum of N addition

df <- fert_data %>% 
  filter(plot == "100") %>% 
  select_all() %>% 
  group_by(date, field) %>% 
  summarize(N100 = sum(tn_ac)) %>% 
  arrange(field)

df <- df %>% 
  mutate('100' = N100, '75' = N100 * 0.75, '50' = N100 * 0.50, '25' = N100 * 0.25, '0' = N100 * 0)

df$field <- as.factor(df$field)
df$date <- as.Date(df$date)
df$month <- month(df$date) 
df$year <- year(df$date)
df$quarter <- quarter(df$date)

df

y1a <- df %>% 
  filter(quarter == "4" | quarter == "2" & year == "2017")
y1b <- df %>% 
  filter(quarter == "1" | quarter == "2" & year == "2018")
y1 <- bind_rows(y1a,y1b)
y1 <- gather(y1, plot, total_n, '100':'0')
y1$season <- "1" 
y1 %>% 
  group_by(field, plot) %>% 
  summarise(total = sum(total_n))
y2a <- df %>% 
  filter(quarter == "4" & year == "2018")
y2b <- df %>% 
  filter(quarter == "1"|quarter == "2" & year == "2019")
y2 <- bind_rows(y2a,y2b)
y2 <- gather(y2, plot, total_n, '100':'0')
y2$season <- "2" 
y2 %>% 
  group_by(field, plot) %>% 
  summarise(total = sum(total_n))

total_n <- bind_rows(y1,y2) 
 
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

