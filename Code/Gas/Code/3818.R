install.packages("dplyr")

dplyr::tbl_df(flux)
#exclude and group columns based on name
n2o<-(flux1[c("field","plot","chamber")], grepl( "n2o" , names( Compound ) ) ])
n2o<-(flux1$Compound==n2o)
mod<-lm(flux~plot,data=n2o)
anova(mod)
boxplot(mod)
