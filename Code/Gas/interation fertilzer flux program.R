df <- flux_tn_by_field
df$total_flux <- df$total_flux^2
theta.0 <- min(df$total_flux)*1.1
model.0 <- lm(log(-total_flux-theta.0)~total_n,data = df)
alpha.0 <- -exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]
start <- list(alpha=alpha.0,beta=beta.0,theta=theta.0)
start
model <- nls(total_flux~alpha*exp(beta*total_n)+theta,data=df,start = start)

ggplot(data=df,aes(total_n,total_flux))+
  geom_point()+
  stat_smooth(method = "gam")

?stat_smooth
plot(df$total_n,df$total_flux)
lines(df$total_n,predict(model,list(x=df$total_n)),col="skyblue",lwd=1)
summary(model)
