library(tidyverse)
library(lubridate)
library(broom)
library(emmeans)
##load lys_data_load
##2019 Data Only
CP <- lys_data_CP %>%
  filter(year == 2019) %>% 
  group_by(field, date, treatment, plot, compound) %>% 
  mutate(mass = concentration*Volume)

vol_weighted <- CP %>%
  group_by(date,field, treatment, compound) %>% 
  summarize(TV = sum(Volume,na.rm = T),TC = sum(mass,na.rm = T)) %>% 
  mutate(vol_conc = TC/TV)            
vol_weighted$field <- as.factor(vol_weighted$field)
vol_weighted$treatment <- as.factor(vol_weighted$treatment)
#din anova
v_w <- vol_weighted %>% 
  group_by(date,field,treatment) %>% 
  summarise(sum_conc= sum(vol_conc))

lm2 <- lm(log(sum_conc)~field*treatment,data = v_w)
summary(lm2)
emmeans(lm2,~field|treatment)
resids <- lm2$resid
plot(resids)
hist(resids,breaks = 50)
shapiro.test(resids)
confint(lm1, level = 0.95)
TukeyHSD(aov(lm2))
anova(lm2)
tin <- vol_weighted %>%
  filter(compound == "no3") %>% 
  group_by(date,field,treatment) %>% 
  summarise(tin = sum(vol_conc))
#just no3 not normal distribution
lm1 <- lm(tin~treatment*field,data = tin)
summary(lm1)
means<- emmeans(lm1,~field|treatment)
means1 <- tin %>% 
  group_by(field,treatment) %>% 
  summarise(means = mean(tin))
means
means1
t.test(vol_conc~treatment,dat = filter(vol_weighted,compound == "no3",field==2), equal.var=F)
df <- as.tibble(means)
resids <- lm1$resid
plot(resids)
hist(resids,breaks = 50)
shapiro.test(resids)
confint(lm1, level = 0.95)
TukeyHSD(aov(lm1))

pd <- position_dodge(.5)
pdd <- position_dodge2(.5)

ggplot(df) +
  geom_col(aes(treatment, emmean, fill = treatment), color = "black",
  width = 0.9, position = pd) +
  geom_errorbar(aes(x = treatment, ymin = emmean-SE, ymax = emmean+SE), width = 0.5, position = pdd) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#66a61e","#7570b3"),name = "", labels = "")+
  labs(x = "", y = "Average nitrate - N (mg/L)") +
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(color = "black"),
        axis.text.x  = element_blank(),  
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=24, colour="black"),
        axis.title.y = element_text(vjust=2, size = 24, face = "bold"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text.x = element_text(size = 24, colour = "black", face = "plain", angle = 0),
        strip.text.y = element_text(size = 24, colour = "black", face = "plain", angle = -90))+
  facet_grid(~field, labeller = as_labeller(fieldnames))

###
#line nitrate by time
vol_w_no3 <- vol_weighted %>% 
  filter(compound=="no3")
total_n$fert_dates <- 0
total_n <- filter(total_n, season == 2)
total_n <- select(total_n,1,2,8)
total_n <- as.data.frame(total_n)
write.csv(total_n, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/total_n.csv")
total_n <- read_csv("~/Dropbox/Lab data/S Willamette GWMA Dropbox/total_n.csv",
col_types = cols(date = col_date(format = "%m/%d/%Y"),
field = col_factor(levels = c("1",
"2", "3", "4"))))
View(total_n)

ggplot(vol_w_no3,aes(x = date)) +
  geom_line(aes(y = vol_conc, color = treatment), na.rm = TRUE, size = 1.2) +
  geom_point(aes(y = vol_conc, color = treatment), na.rm = TRUE, size = 2, shape = 1,stroke = 2) +
  geom_point(data = total_n,aes(y = fert_dates), size = 3, shape = 3, stroke = 2) +
  #geom_errorbar(data = df1, aes(ymin = tin-tse, ymax=tin-tse, na.rm = TRUE))+
scale_color_manual(values = c("#66a61e","#7570b3"),name = "", labels = NULL)+
  labs(x = NULL,y = "Average nitrate/nitrite - N (mg/L)", color = "") + 
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
                strip.background = element_rect(color = "black"),
        axis.text.x  = element_text(size=12, colour="black", face = "plain",angle=90),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=22, colour="black"),
        axis.title.y = element_text(vjust=3, hjust = 1,size = 24, face = "bold"),
        legend.position = "none",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 24, colour = "black", face = "plain", angle = 0))+
  facet_grid(~field,labeller = as_labeller(fieldnames))

##box plot by date
box <- filter(lys_data_CP, compound == "no3",year == 2019)
box$date <- as.factor(box$date)

ggplot(box,aes(x = date, y = concentration, fill = treatment)) +
  geom_boxplot()+
  scale_fill_manual(values = c("#66a61e","#7570b3"),name = "", labels = c("Conventional","EEF"))+
  labs(x = NULL,y = "N as nitrate (ppm)", color = "")+
  
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(color = "black"),
        axis.text.x  = element_text(size=8, colour="black", face = "plain", angle = 90),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=24, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 24, face = "bold"),
        legend.position = "none",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 24, colour = "black", face = "bold", angle = 0))+
  facet_wrap(~field,labeller = as_labeller(fieldnames))
fieldnames <- c("1" = "Field 1", "2" = "Field 2", "3"="Field 3","4"="Field 4")
