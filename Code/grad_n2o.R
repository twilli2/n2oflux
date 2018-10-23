library(tidyverse)
grad_n2o <- filter(flux_data, compound == "n2o", plot != "C" & plot != "P") %>% 
  group_by(date,field,plot) %>% 
  summarize(flux_mean = mean (flux,na.rm = T))    
ggplot(grad_n2o, aes(x = date, y = flux_mean, color = plot)) +
  geom_point() +
  geom_line() +
  facet_wrap(~field)


library(RColorBrewer)
ggplot(grad_n2o,aes(x = plot, y = flux_mean, fill = plot)) +
  geom_boxplot(color = "black", size = .05, alpha = 0.7) +
  stat_boxplot(geom = "errorbar") +
  facet_grid(.~field)

ggplot(data = grad_n2o) +
  geom_histogram(mapping = aes(x = flux_mean))



stat_summary(
    mapping = aes(x = plot, y = flux),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

ggplot(data = filter(flux_data, compound == "n2o")) +
  stat_ydensity(
  mapping = aes(x = plot, y = flux))

data <- filter(flux_data, compound == "n2o") %>% 
        mutate(lflux = log(flux)) %>% 
        mutate(lsoil = log(soiltemp))

mod <- lm(lflux ~ lsoil, data)
summary(mod)
  
geom_smooth(mapping = aes(x = soiltemp, y = lflux))

