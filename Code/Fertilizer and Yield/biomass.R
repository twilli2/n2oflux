library(tidyverse)
library(emmeans)
library(readxl)
biomass <- read_excel("~/Dropbox/GWMA CIG-ODA project/2019 Fescue harvest biomass data TW-1.xlsx",
sheet = "Sheet4")
biomass <- rename(biomass = `biomass kg ha`, biomass)

biomass <- filter(biomass,treatment == "Conv" | treatment == "EEF", season == 2) 
biomass$field <- as.factor(biomass$field)
biomass$treatment <- as.factor(biomass$treatment)
cp_bio <- biomass %>%
  group_by(field,treatment) %>% 
  summarise(mean_seed = mean(seed_kg),mean_hay=mean(straw_kg),mean_total=mean(total_kg))
cp_bio$field <- as.factor(cp_bio$field)
cp_bio$treatment <- as.factor(cp_bio$treatment)
#cp_bio$season[cp_bio$season == 1] <- "Season 1"
#cp_bio$season[cp_bio$season == 2] <- "Season 2"


m1 <- lm(seed_kg~field*treatment,data = biomass)
m2 <- lm(total_kg~field*treatment,data = biomass)
anova(m1)
resids_m1 <- m1$resid
shapiro.test(resids_m1)
summary(m1)
TukeyHSD(aov(m1))

anova(m2)
resids_m2 <- m2$resid
shapiro.test(resids_m2)
summary(m2)
TukeyHSD(aov(m2))

cp_bio<- emmeans(m1,~field|treatment)
cp_bio2 <- emmeans(m2,~field|treatment)
cp_bio <- as.tibble(cp_bio)
cp_bio2 <- as.tibble(cp_bio2)


cp_bio <-  cp_bio %>% 
mutate(emmean = emmean/1000, lower.CL = lower.CL/1000, upper.CL = upper.CL/1000)
cp_bio2 <-  cp_bio2 %>% 
mutate(emmean = emmean/1000, lower.CL = lower.CL/1000, upper.CL = upper.CL/1000)

TukeyHSD(aov(m1))

ggplot(cp_bio) +
  geom_col(mapping = aes(treatment,emmean, fill = treatment))+
  geom_col(data = cp_bio2, mapping = aes(treatment,emmean, fill = treatment),alpha = .5)+
  geom_errorbar(aes(x = treatment, ymin = lower.CL, ymax = upper.CL),width = 0.5)+
  scale_fill_manual(values = c("#66a61e","#7570b3"),
                    labels = c("Conventional","Enhanced Efficiency"))+
  labs(x = "", y = "Average seed (dark) of total biomass (Mg/ha)", fill = "")+
  theme_bw()+
  theme(axis.text.x  = element_blank(),
        axis.title.x = element_text(size = 24, vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=24, colour="black"),
        axis.title.y = element_text(vjust=2, size = 20, face = "bold"),
        legend.text = element_text(size = 24),
        legend.title = element_text("Field",size = 24),
        legend.position = "none",
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) +
  facet_grid(~field)

fieldnames <- c("1" = "Field 1", "2" = "Field 2", "3"="Field 3","4"="Field 4","Season 1" = "Season 1", "Season 2" = "Season 2")

ggplot(filter(biomass,treatment != "Conv"|treatment != "EEF"))+
  geom_point(aes(treatment,biomass))+
  geom_smooth(aes(treatment,biomass), method = "lm")

ggplot(fert_total) +
  geom_col(mapping = aes(plot,totaln_kg, fill = plot))+
  facet_grid(season~field)
