library(tidyverse)
grad_co2 <- filter(flux_data, compound == "co2", plot != "C" & plot != "P") %>% 
  group_by(date,field,plot) %>% 
  summarize(flux_mean = mean (flux,na.rm = T))    
ggplot(grad_co2, aes(x = date, y = flux_mean, color = plot)) +
  geom_point(position = "jitter") +
  geom_line() +
  facet_wrap(~field)


library(RColorBrewer)
ggplot(grad_co2,aes(x = plot, y = flux_mean, fill = plot)) +
  geom_boxplot(color = "black", size = .05, alpha = 0.7) +
  stat_boxplot(geom = "errorbar") +
  facet_grid(.~field)

ggplot(data = filter(flux_data, compound == "co2"))+
  geom_smooth(mapping = (aes(x = soiltemp, y = flux)))

?geom_boxplot
    outlier.color = "#1F3552", outlier.shape = 20, notch=FALSE) +
    scale_x_discrete(name = "% of Fertilizer") +
    scale_y_continuous(name = "LOG CO2 FLUX\n(mg C cm-2 h-1)",
                       breaks = seq(-10, 1, .1),
                       limits=c(-10, 1))) + 
    labs(caption = "Boxplot of flux by % fertilizer by field") +
    theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(size=0.25, colour = "lightgray"),
        panel.grid.minor = element_line(size=0.1,color="lightgray"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12))+
  facet_grid(.~field)
a
