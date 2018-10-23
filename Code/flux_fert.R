library(tidyverse)
library(ggplot2)
fert <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Fert-Yield data/Fertilizer Plans.xlsx",
sheet = "Sheet1", col_types = c("numeric",
"text", "date", "text", "numeric",
"numeric","numeric"))
fert$field <- as.factor(fert$field)
fert$plot <- as.factor(fert$plot)
fert$date <- as.Date(fert$date)
fert <- filter(fert, plot == "C"|plot == 'P')
cp_n2o$date <- as.Date(cp_n2o$date)
n2o_fert <- full_join(cp_n2o, fert, by = c("plot","field","date"))
n2o_fert <- mutate(n2o_fert, fert = log(total_lbs_acre))

n2o_fert
utils::View(n2o_fert)

ggplot(cp_n2o, aes(x = date, y = mean_flux, color = plot)) +
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

cp_n2o_1 <-  filter(flux_data,compound == "n2o", plot == "C" | plot == "P", flux > -24) %>%  
  select_all() %>% 
  group_by(field, plot) %>% 
  summarize(mean_flux = mean(flux), sd = sd(flux,na.rm = T), se = std.error(flux,na.rm = T))


ggsave("n2o_flux_mean.png")
