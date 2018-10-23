library(tidyverse)
cp_n2o <- filter(flux_data,compound == "n2o", plot == "C" | plot == "P", flux > -24) %>%  
  select_all()

cp_n2o <- cp_n2o %>% 
  group_by(field, plot, date) %>% 
  summarize(mean_flux = mean(flux), sd = sd(flux,na.rm = T)) %>% 
  filter(mean_flux != "NA") %>%
  mutate(cs = cumsum(mean_flux))

cp_n2o_st <- cp_n2o %>% 
  group_by(field, plot, date, soiltemp) %>% 
  summarize(mean_flux = mean(flux)) %>% 
  filter(mean_flux != "NA")

ggplot(cp_n2o, aes(x = date, y = cs, color = plot)) +
  geom_point(size = 2, shape = 1) +
  geom_line(size = 1.2) +
  #geom_area(alpha = 0.2) + 
  scale_color_discrete(name = "Treatment", labels = c("Conventional","Slow-release")) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  labs(x = NULL, y = "Average cumulutive flux (ng N/cm/hr)")+
  geom_errorbar(aes(x = date, ymin = cs - sd, ymax = cs + sd), size = 0.5, width = 0.3) +
  theme(panel.background = element_rect(fill='white', colour='white'), 
        panel.grid = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_blank(),
        axis.text.x  = element_text(size=14, colour="black", face = "bold"),  
        axis.title.x = element_text(vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12, colour = "black", face = "bold", angle = 0))+
  facet_wrap(~field)

  
ggplot(cp_n2o_1, aes(x = plot, y = mean_flux,fill = plot), color = "black") +
  geom_col() +
  scale_fill_discrete(name = "Treatment", labels = c("Conventional","Slow-release")) +
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
  geom_errorbar(aes(x = plot, ymin = mean_flux - se, ymax = mean_flux + se), 
                size = .5, width = 0.3) +
  facet_grid(~field)
