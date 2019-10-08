#n2o flux by field and gradient plot
df = gradientn2o
# create multiple linear model
lm_fit <- lm(flux~field-1,data=df)
summary(lm_fit)
plot(flux~field,data=gradientn2o, pch = 16, cex = 1.3, col = "blue",
     main = "N2O FLUX PLOTTED AGAINST FIELD", 
     xlab = "FIELD", ylab = "N2O FLUX (ng N cm-2 h-1)")
abline(lm_fit)
a<-ggplot(data = df, aes(x = field, y = flux)) + geom_boxplot()+ geom_point(color='blue')
a
mmod<-lm(flux~field-1+plot,data=df)
summary(mmod)
plot(flux~field-1+plot,data=df, pch = 16, cex = 1.3, col = "blue",
     main = "N2O FLUX PLOTTED AGAINST FIELD", 
     xlab = "FIELD", ylab = "N2O FLUX (ng N cm-2 h-1)")
b<-ggplot(data = df, aes(x = field&plot, y = flux)) + geom_boxplot()+ geom_point(color='blue')
b
# save predictions of the model in the new data frame 
# together with variable you want to plot against
predicted_df <- data.frame(flux_pred = predict(lm_fit, df), flux=df$flux)
 
# this is the predicted line of multiple linear regression
ggplot(data = df, aes(x = field, y = flux)) + 
  geom_point(color='blue')
  
  geom_line(color='red',data = predicted_df, aes(x=field, y=flux))

# this is predicted line comparing only chosen variables
ggplot(data = df, aes(x = date, y = flux)) + 
  geom_point(color='blue') +
  geom_smooth(method = "loess", se = FALSE)

plot(flux~date,data=gradientn2o, pch = 16, cex = 1.3, col = "blue",
     main = "N2O FLUX PLOTTED AGAINST DATE", 
     xlab = "DATE", ylab = "N2O FLUX (ng N cm-2 h-1)")
abline(mod3)

plot(flux~plot+field,data=gradientn2o, pch = 16, cex = 1.3, col = "blue",
     main = "N2O FLUX PLOTTED AGAINST PLOT", 
     xlab = "PLOT", ylab = "N2O FLUX (ng N cm-2 h-1)")
#add a column, in this case, add 10 and get log of flux
gradientn2o$n2o10<-(gradientn2o$flux)+10
gradientn2o$logn2o<-log(gradientn2o$n2o10)
utils::View(n2o)
#boxplot with ggplot
f <- ggplot(gradientn2o, aes(x=field, y=flux)) + geom_boxplot()
f
field <- gradientn2o[which(gradientn2o$field == "1" |
                             gradientn2o$field == "2" |
                             gradientn2o$field == "3" |
                     gradientn2o$field== "4"), ]
field$plot <- as.factor(field$flux) 
labels = c("0", "25","50","75","100")

p10 <- ggplot(field, aes(x = field, y = flux)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Mean ozone in\nparts per billion",
                     breaks = seq(0, 175, 50),
                     limits=c(0, 175)) +
  scale_x_discrete(name = "Month") +
  ggtitle("Boxplot of mean ozone by month") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  facet_grid(. ~ field)

p10

mod3<-glm(flux~field-1,family=gaussian,gradientn2o)
par(mfrow=c(2,2)) 
plot(mod3)
abline(mod3)
summary(mod3)

ggplot(data = gradientn2o, aes(x = field, y = flux)) + 
  geom_point(color='blue') +
  geom_smooth(method = "loess", se = FALSE)
print(gradientn2o)
utils::View(gradientn2o)

p10 <- ggplot(df,aes(x = field, y= flux)) +
  geom_boxplot(colour = lines, fill = fill,
               size = 1, notch = TRUE) +
  scale_y_continuous(name = "Flux",
                     breaks = seq(0, 175, 25),
                     limits=c(0, 175)) +
  scale_x_discrete(name = "Field") +
  ggtitle("Boxplot") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(colour="black", size = 11),
        axis.text.y=element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black"))
p10
utils::View(df)
df <- subset(df, compound=="n2o",
              select=field:soiltemp)
df<-dplyr::filter(df,!is.na(df$flux))

