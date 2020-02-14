p1 <- p %>% 
  filter(plot!="C"&plot!="P") %>% 
  arrange(date,field, match(plot, c("0","25","50","75","100"),chamber)) %>% 
  select(1,2,3,4,16)
t_n1 <- select(t_n,2,3,7,8)
t_n1$date <- as.Date(t_n1$date)
t_n1$plot <- as.factor(t_n1$plot)
p1$date <- as.Date(p1$date)

write.csv(p1, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/p1.csv")

write.csv(t_n1, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/t_n1.csv")

#seq_len(x) creates sequential vector counting by 1 to the lenght of (x)
#nrow(x) returns the number of rows present in (x)
#abs(x) computes the absolute value aka non-negative value |x| of (x)
#order(x) arranges by ascending order of (x)
#seq_along(x) creates a sequential vector the length of an existing vector (x)
str(p1)
str(t_n1)
library(data.table)
setDT(t_n1)[, date := date]
results <- t_n1[p1, on = .(field,plot, date = date), roll = 'nearest']
results
?merge
test_plot<- function (z,df=results,x,y){
  df <- filter(df,field == z) 
  ggplot(data = df)+
    geom_point(aes(df$x,df$y))+
    stat_smooth(aes(df$x,df$y),method = "lm",
             formula = df$y ~ df$x + poly(df$x, 2) - 1)
}

test_plot(1,results,total_n, n2o_flux)
test_plot(2)
test_plot(3)
test_plot(4)

model_test <- function(z){
  mod <- lm(log(n2o_flux)~total_n, data = filter(results, field ==z))
  summary1 <- (summary(mod))
  print(summary1)
  resids <- mod$resid
  shapiro.test(resids)
}
quadratic_model_test <- function(z){
  quad <- lm(log(n2o_flux)~total_n+I(total_n^2), data = filter(results, field ==z))
  summary2 <- (summary(quad))
  print(summary2)
  resids <- quad$resid
  shapiro.test(resids)
  }
model_test(1)
quadratic_model_test(1)
model_test(2)
quadratic_model_test(2)
model_test(3)
quadratic_model_test(3)
model_test(4)
quadratic_model_test(4)

p2 <- flux_tn
full_plot(1)
full_plot(2)
full_plot(3)
full_plot(4)

seq(j$field)
?predict
## cool way to view a bunch of predictions
linear <- lm(total_flux~total_n,data=flux_tn)
quad_n <- lm(total_flux~total_n+I(total_n^2),data = flux_tn)
summary(linear)
summary(quad_n)

log_n <- lm((log(total_flux)~total_n), data = flux_tn)
poly_n <- lm(total_flux~total_n+I(total_n^2) + I(total_n^3),flux_tn)#cubic
exp_n <- lm(total_flux~I(total_n^2), data = flux_tn)#exponential
        
new.data <- data.frame(total_n=seq(from=min(flux_tn$total_n),
                                           to=max(flux_tn$total_n),
                                                  length.out = 200))

pred_lm <- predict(linear,newdata=new.data)
pred_quad <- predict(quad_n,newdata=new.data)
pred_poly <- predict(poly_n,newdata=new.data)
pred_exp <- predict(exp_n,newdata=new.data)
pred_log <- predict(log_n,newdata=new.data)
preds <- data.frame(new.data,
                    lm = pred_lm,
                    quad= pred_quad,
                    poly=pred_poly,
                    exp=pred_exp)
                    #log=pred_log)

preds <- reshape2::melt(preds,
                        id.vars=1)

ggplot(data =preds) + 
  geom_line(aes(x=total_n,y=value, color = variable))+
  geom_point(data=flux_tn,aes(x = total_n, y = total_flux, color = field))
summary(quad_n)
summary(linear)
summary(poly_n)
summary(exp_n)
plot(exp_n)

summary(log_n)
augment(exp_n) %>% 
  arrange(-.cooksd)
#magrittr
#match.arg()
#extract()
#library(mgcv)
#generalized additive model
#gam(yield_kg_per_ha ~ s(year) + census_region, data = corn)
#predict(model, cases_to_predict, type = "response")
