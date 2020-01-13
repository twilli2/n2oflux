
co2flux <- filter(flux_data, co2_rsq >= 0.30 & co2_rsq < 1, plot != "C" & plot != "P") %>% 
  select(1,2,3,9,6,15) %>% 
  group_by(season,date,field,plot) %>% 
  summarise(mean_flux = mean(co2_flux)) %>% 
  arrange(match(plot, c("0","25","50", "75", "100")),-desc(field), -desc(date))

co2flux <- co2flux %>% 
  group_by(field,plot) %>% 
  mutate(cum_flux = cumsum(mean_flux)) %>% 
  mutate(cum_flux = cum_flux*24)
co2flux$plot <- factor(co2flux$plot,levels = c("0","25","50", "75", "100"))
fieldnames <- c("1"= "Field 1","2" = "Field 2","3" = "Field 3", "4" = "Field 4")

n_dates <- total_n %>% 
  group_by(date, field) %>% 
  select(1,2)
n_dates$cum_flux <- 0

ggplot(data = co2flux) + 
  geom_point(aes(date,cum_flux,color = plot)) +
  geom_line(aes(date,cum_flux,color = plot))+
  geom_point(data = n_dates,aes(date,cum_flux),size = 2,shape = 2)+
 facet_wrap(~field, labeller = as_labeller(fieldnames))+
  labs(x = "",y = expression(Cumulative~daily~flux~" "~(g~CO[2]-C~~m^{-2}~day^{-1})), color='% max\n N rate')+
  theme_bw() +
  theme(axis.text.x  = element_text(size=14, colour="black"),  
        axis.title.x = element_text(size = 16, vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text("Field",size = 16),
        strip.text.x = element_text(size = 16, colour = "black", face = "plain", angle = 0))
  


n2oflux <- filter(flux_data, n2o_rsq >= 0.30 & n2o_rsq < 1, plot != "C" & plot != "P") %>% 
  select(1,2,3,9,8,16) %>%
  group_by(season,date,field,plot) %>% 
  summarise(mean_flux = mean(n2o_flux)) %>% 
  arrange(match(plot, c("0","25","50", "75", "100")),-desc(field), -desc(date))

n2oflux <- n2oflux %>% 
  group_by(field,plot) %>% 
  mutate(cum_flux = cumsum(mean_flux)) %>% 
  mutate(cum_flux=cum_flux*2.4)
n2oflux$plot <- factor(n2oflux$plot,levels = c("0","25","50", "75", "100"))
fieldnames <- c("1"= "Field 1","2" = "Field 2","3" = "Field 3", "4" = "Field 4")

n2oflux100 <- n2oflux %>% 
  filter(plot == 100) %>% 
  group_by(season,field) %>% 
  mutate(total = sum(mean_flux))
write.csv(n2oflux100, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/Presentations/gradiant_n2o_flux_data.csv")

#write.csv(j3, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/gradiant_flux_data_final.csv")

ggplot(data = n2oflux) + 
  geom_point(aes(date,cum_flux,color = plot)) +
  geom_line(aes(date,cum_flux,color = plot))+
  geom_point(data = n_dates,aes(date,cum_flux),size = 2,shape = 2)+
  facet_wrap(~field, labeller = as_labeller(fieldnames))+
  labs(x = "",y = expression(Cumulative~daily~flux~" "~(g~N[2]*O-N~~ha^{-2}~day^{-1})), color='% max\n N rate')+
  theme_bw() +
  theme(axis.text.x  = element_text(size=14, colour="black"),  
        axis.title.x = element_text(size = 16, vjust=-0.1, face = "bold"),
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.y = element_text(vjust=1.8, size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text("Field",size = 16),
        strip.text.x = element_text(size = 16, colour = "black", face = "plain", angle = 0))
  

n_dates <- total_n %>% 
  group_by(date, field) %>% 
  select(1,2)
n_dates$cum_flux <- 0
