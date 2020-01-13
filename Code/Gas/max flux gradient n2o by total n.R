f <-   filter(j,plot != "C" & plot != "P") %>%
  group_by(field, plot, date, season) %>% 
  arrange(match(plot, c("0","25","50", "75", "100")),-desc(field), -desc(date))
f$season <- as.factor(f$season)

f<- f %>%
  group_by(field,plot,season) %>% 
  summarise(total_flux = sum(mean_flux,na.rm=T)/246) %>% 
  mutate(total_flux = total_flux*2.4)

f$field <- as.factor(f$field)

total_n <- t_n %>% 
  group_by(field,plot,season) %>% 
  summarize(total_n = sum(total_n)) %>% 
  mutate(total_n = (total_n*1.12085)) 
total_n$field <- as.factor(total_n$field)
flux_tn <- left_join(f,total_n)

###Figure 1. Average daily flux as function on nitrogen rate by field. Least squared quadratic regression
glm(total_flux ~ total_n+as.factor(season)+as.factor(field),data = flux_tn)
# draw the 3D scatterplot
install.packages("plotly")

library(plotly)
r <- plot_ly(data = flux_tn, z = ~total_flux, x = ~total_n, y = ~season, opacity = 0.6) %>%
  add_markers(color = ~field) 
r  
# draw two planes
r %>%
  add_surface(x = ~x, y = ~y, z = ~plane0, showscale = FALSE) %>%
  add_surface(x = ~x, y = ~y, z = ~plane1, showscale = FALSE)

ggplot(data = flux_tn, aes(total_n,total_flux))+
  geom_point(aes(color = field, shape = season), size = 6) +
  stat_smooth(method = "lm",
              formula = y~x+I(x^2),se=F)
              #formula = y ~ exp(x),se = F) +
  labs(x = expression("Average nitrogen rate (kg N"~~ha^{-1}~year^{-1}*")"))+ 
  labs(y = expression(Average~daily~flux~" "~(g~N[2]*O-N~ha^{-1}~day^{-1})), color='Field') +
  annotate(geom = "text", x=200, y=0.1, label= ("r-squared = 0.83\np-value < 0.001"),size = 6)+
   theme_bw() +
   theme(axis.text.x  = element_text(size=20, colour="black"),  
         axis.title.x = element_text(size = 20, vjust=-0.1, face = "bold"),
         axis.text.y = element_text(size=20, colour="black"),
         axis.title.y = element_text(vjust=1.8, size = 20, face = "bold"),
         legend.text = element_text(size = 20),
         legend.title = element_text("Field",size = 20),
         legend.position = c(0.08, 0.85),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         strip.text.x = element_text(size = 20, colour = "black", face = "bold", angle = 0))
