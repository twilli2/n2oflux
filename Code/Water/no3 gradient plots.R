library(ggplot2)
library(broom)
library(tidyverse)
library(readxl)
library(lubridate)
lys_data_grad<- filter(lys_data, compound =='no3') %>% 
  group_by(field)

lys_data_grad <- select(lys_data_grad,1,2,4,6)
lys_data_grad <- rename(lys_data_grad, plot = "treatment")
lys_data_grad1 <- lys_data_grad %>% 
  group_by(field,plot) %>% 
  summarize(mean_conc= mean(concentration,na.rm = T))

fert_data <- read_excel("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Fert-Yield data/Fertilizer Plans.xlsx",
sheet = "total fert", col_types = c("date","text", "text", "numeric", "numeric",
                      "numeric", "numeric", "numeric"))

df <- fert_data %>% 
  filter(plot == "100")%>% 
  select_all() %>% 
  group_by(date, field) %>% 
  mutate(N100 = sum(tn_ac)) %>% 
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
y1 <- mutate(y1, "C" = N100)
y1 <- mutate(y1, "P" = N100)
y1 <- gather(y1, plot, total_n, '100':'0','C')
y1$season <- "1" 

y2a <- df %>% 
  filter(quarter == "4" & year == "2018")
y2b <- df %>% 
  filter(quarter == "1" & year == "2019"|quarter == "2" & year == "2019")
y2 <-bind_rows(y2a,y2b)
y2 <- mutate(y2, "C" = N100)
y2 <- mutate(y2, "P" = N100)
y2 <- gather(y2, plot, total_n, '100':'0','C')
y2$season <- "2" 

total_n <- bind_rows(y1,y2)
total_n <- select(total_n,1,2,13,14,15)
total_n1 <- total_n %>% 
  group_by(field,plot) %>% 
  summarise(total_n = sum(total_n)/2)

lys_n <- left_join(lys_data_grad1,total_n1)
lys_n <- filter(lys_n,plot!="P"&plot!="C")

linear_no3_n <- lm(mean_conc~total_n,data=lys_n)
quad_no3_n <- lm(mean_conc~total_n+I(total_n^2),data = lys_n)

log_no2_n <- lm((log(abs(mean_conc))~log(abs(total_n))), data = lys_n)
poly_no3_n <- lm(mean_conc~total_n + I(total_n^2) + I(total_n^3),lys_n)#cubic
           
new.data <- data.frame(total_n=seq(from=min(lys_n$total_n),
                                           to=max(lys_n$total_n),
                                                  length.out = 200))

pred_lm <- predict(linear_no3_n,newdata=new.data)
pred_quad <- predict(quad_no3_n,newdata=new.data)
pred_poly <- predict(poly_no3_n,newdata=new.data)
preds <- data.frame(new.data,
                    lm = pred_lm,
                    quad= pred_quad,
                    poly=pred_poly)
preds <- reshape2::melt(preds,
                        id.vars=1)

ggplot(data =preds) + 
  geom_line(aes(x=total_n,y=value, color = variable))+
  geom_point(data=lys_n,aes(x = total_n, y = mean_conc, color = field))
summary(quad_no3_n)
summary(linear_no3_n)
summary(poly_no3_n)
augment(quad_no3_n) %>% 
  arrange(-.cooksd)
lys_n
ggplot(data = lys_n, aes(total_n,mean_conc))+
  geom_point(aes(color = field), size = 6) +
  stat_smooth(method = "lm",
              formula = y~I(x^2))+
              #formula = y ~ exp(x),se = F) +
  labs(x = expression("Average nitrogen rate (kg N"~~ha^{-1}~year^{-1}*")"))+ 
  labs(y = expression(Average~concentration~" "~(mg~NO[3]*-N~~L^{-1})), color='Field') +
  annotate(geom = "text", x=50, y=20, label= ("r-squared = 0.30\np-value < 0.01"),size = 6)+
   theme_bw() +
   theme(axis.text.x  = element_text(size=20, colour="black"),  
         axis.title.x = element_text(size = 20, vjust=-0.1, face = "bold"),
         axis.text.y = element_text(size=20, colour="black",hjust = 1),
         axis.title.y = element_text(vjust=1.8, size = 20, face = "bold"),
         legend.text = element_text(size = 18),
         legend.title = element_text("Field",size = 18),
         legend.position = c(0.08, 0.80),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         strip.text.x = element_text(size = 20, colour = "black", face = "bold", angle = 0))
###
grad_by_treat <- lys_data_grad %>% 
  group_by(treatment)
ggplot(grad_by_treat, aes(date, concentration, color = treatment)) +  
  geom_point(na.rm = TRUE, size = 1) +
  geom_point() +
  scale_colour_discrete() +
  labs(
    x = "Month", 
    y = "Nitrate/Nitrite (mg N/L)", 
    caption = ("Nitrate/Nitrite concentration over time by treatment and field"), 
    color = ("Percent Fertilizer")) +
  theme_bw() +
  facet_wrap(~field)

F1<- filter(lys_data_grad, treatment != "C"|treatment != "P", field == 1)
summary(F1)

mod<-lm(concentration ~ treatment, lys_data_G)
summary(mod)
coeffs = coefficients(mod); coeffs
summary(mod)$r.squared
anova(mod)
kruskal.test(concentration ~ treatment, data = lys_data_grad)
# Field 4 signif diff in 100 and 75 plot
# model F-statistic: 68.09 on 4 and 33 DF,  p-value: 1.789e-15
# treatment75    8.0108     1.9470   4.114 0.000243 ***
# treatment100  27.0833     1.9470  13.910 2.34e-15 ***
# Field 2 signif diff in 100 plot
# treatment100  3.18400    0.42986   7.407 9.29e-08 ***
# model F-statistic: 22.44 on 4 and 25 DF,  p-value: 5.753e-08
# Field 1 25 plot signif diff 
# treatment25   0.22000    0.08140   2.703   0.0137 *
View(count(lys_data_grad %>%
  group_by(field, treatment, concentration))) 
n2
n2 <- grad_by_treat %>% 
  group_by(field, treatment, compound) %>% 
  summarise(mean = mean(concentration, na.rm = T), 
            sd = sd(concentration, na.rm = T))

ggplot(data = n2, mapping = aes(treatment, mean, fill = compound)) +
  geom_point() +
  theme(legend.position = "right") +
  scale_fill_hue(h=c(500,50),name = "Compound", labels = c("Ammonia","Nitrate"))+
  labs(x = "Treatment", y = "Average N (ppm)") +
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=14, colour="black", face = "bold"),  
        axis.title.x = element_text(size = 14,vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0))

ggplot(data = n2, mapping = aes(treatment, mean, fill = compound)) +
  geom_col() +
  theme(legend.position = "right") +
  scale_fill_hue(h=c(500,50),name = "Compound", labels = c("Ammonia","Nitrate"))+
  labs(x = "Treatment", y = "Average N (ppm)") +
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=14, colour="black", face = "bold"),  
        axis.title.x = element_text(size = 14,vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0))+
  facet_wrap(~field)

