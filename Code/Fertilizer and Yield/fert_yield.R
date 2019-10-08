library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
#C:/Users/twilli2/
fert_data <- read_excel("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Fert-Yield data/Fertilizer Plans.xlsx",
sheet = "total fert", col_types = c("date","text", "text", "numeric", "numeric",
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

y1a <- df %>% 
  filter(quarter == "4" & year == "2017"| quarter == "2" & year == "2017")
y1b <- df %>% 
  filter(quarter == "1" & year == "2018" | quarter == "2" & year == "2018")
y1 <- bind_rows(y1a,y1b)
y1 <- gather(y1, plot, total_n, '100':'0')
y1$season <- "1" 

y2a <- df %>% 
  filter(quarter == "4" & year == "2018")
y2b <- df %>% 
  filter(quarter == "1" & year == "2019"|quarter == "2" & year == "2019")
y2 <-bind_rows(y2a,y2b)
y2 <- gather(y2, plot, total_n, '100':'0')
y2$season <- "2" 

t_n <- bind_rows(y1,y2)
t_n <- t_n [-3]
rm(df,fert_data,y1,y1a,y1b,y2,y2a,y2b)

yield_n <- t_n %>%
  filter(season == 1) %>% 
  group_by(field,plot) %>% 
  summarize(total_n = sum(total_n)) %>% 
  mutate(total_n = (total_n*1.12085)) 
yield_n$field <- as.factor(yield_n$field)

yield_data <- read_excel("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Fert-Yield data/Yield Data.xlsx",
sheet = "Sheet3")
grad_yield <- filter(yield_data,plot == "0"|plot =="25"|plot =="50"|plot =="75"|plot =="100")
grad_yield$field <- as.factor(grad_yield$field)
#yield_data <- as_tibble(c(1,2,3,4))
#yield_data$yield_kg <- (c(1586,2707,1995,1771))
#mutate(yield_data, yield_lb = yield_kg/1.12085)
#yield_data <- rename(yield_data, field = value)
#yield_data$total_n_lb<- c(155, 160, 232, 232)
#yield_data$plot <- c("C")
#yield_data1 <- yield_data
#yield_data2 <- yield_data
#yield_data2$plot <-  c("P")
#yield_data2$yield_kg <- c(1468,2175,2982,2533)
#fert_yield <- bind_rows(yield_data1,yield_data2)
#fert_yield <- mutate(fert_yield, yield_lb = yield_kg/1.12085)
#fert_yield$total_field_yield_lb <- 0
#fert_yield
#fert_yield[c(3,4,7,8),"total_field_yield_lb"] <- fert_yield[c(3,4,7,8),"total_field_yield_lb"] + 2049
#fert_yield$average_yield_lb <- c(1363,2178,2220,1920)
#fert_yield$season <- 1
#View(fert_yield)
fert_yield <- left_join(grad_yield,yield_n)

ggplot(fert_yield) +
  geom_col(aes(plot, kg_ha),color = "black", fill = "lightblue") +
  #geom_col(aes(plot, average_yield_lb), alpha = 0.0, 
  #         color = "black", linetype = "dashed", size = .5) +
  labs(x = "Treatment", y = "Yield (kgs/ha)")+
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=14, colour="black", face = "bold"),  
        axis.title.x = element_text(size = 14, vjust=-1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 14, colour = "black", face = "bold", angle = 0)) +
  facet_grid(season~field)
fert_yield$season <- as.factor(fert_yield$season)

fert_total<-total_fert %>% 
  filter(plot == "C" | plot == "P") %>% 
  group_by(date, field, plot) %>% 
  summarize(totaln_kg = sum(tn_ac)*1.12085, t_urea=sum(urea_ac)*1.12085,t_nh4=sum(nh4_ac)*1.12085,
  t_p=sum(tp_ac)*1.12085,t_k=sum(tk_ac)*1.12085)

fert_total1 <-  filter(fert_total,date <= "2018-04-09")
fert_total1$season <- 1
fert_total2 <-  filter(fert_total,date > "2018-04-09") 
fert_total2$season <- 2
fert_total <- bind_rows(fert_total1,fert_total2)
fert_total <- fert_total %>%
  group_by(season,field,plot) %>%
  summarize(totaln_kg = sum(totaln_kg),t_urea=sum(t_urea),t_nh4=sum(t_nh4),t_p = sum(t_p),t_k = sum(t_k))

library(reshape2)

df1 <- filter(fert_total, season == 1) 
df1 <- df1[-1]
df1 <- df1[-2]
df1 <- melt(df1, id.vars = "field")
df1$season <- 1

df2 <- filter(fert_total, season == 2) 
df2 <- df2[-1]
df2 <- df2[-2]
df2 <- melt(df2, id.vars = "field")
df2$season <- 2

df <- bind_rows(df1,df2)
ggplot(df, aes(x=field,y=value,fill=variable)) +
  geom_bar(stat = 'identity', position = 'dodge')+
  #geom_col(aes(field, t_urea),color = "black", fill = "green", position = "dodge") +
  #geom_col(aes(field, t_nh4),color = "black", fill = "red", position = "dodge") +
  #geom_col(aes(plot, average_yield_lb), alpha = 0.0, 
  #         color = "black", linetype = "dashed", size = .5) +
  labs(x = "Field") +
  labs(y = expression("Application rate (kg ai"~~ha^{-1}*")"))+ 
  labs(color='variable')+
  scale_fill_discrete(name = "",
    labels = c("Total N", "Urea-N","Ammonium-N","Phosphorus","Pottasium"))+
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=20, colour="black", face = "plain"),  
        axis.title.x = element_text(size = 20, vjust=-1, face = "plain"),
        axis.text.y = element_text(size=20, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 20, colour = "black", face = "plain", angle = 0))+
  facet_grid(~season, labeller = as_labeller(seasonnames))
seasonnames <- c("1" = "Season 1", "2" = "Season 2")
###fert rate by seed yield
fert_yield$plot <- factor(fert_yield$plot,levels = c("0","25","50", "75", "100"))
ggplot(data = fert_yield) +
  geom_point(aes(total_n, kg_ha, color = field), size = 6)+
  labs(y = expression("Estimated seed yield (kg"~~ha^{-1}*")"),color = "Field")+
  labs(x = expression("Nitrogen rate (kg N"~~ha^{-1}*")"))+ 
  geom_smooth(aes(total_n, kg_ha),method = "glm",
              formula = y ~ x + poly(x, 2)-1, se = FALSE, inherit.aes = F,fullrange = T)+
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=20, colour="black", face = "plain", angle = 0),  
        axis.title.x = element_text(vjust=-0.1, size = 20, face = "plain"),
        axis.text.y = element_text(size=20, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 20, face = "plain"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 20, colour = "black", face = "plain", angle = 0))

