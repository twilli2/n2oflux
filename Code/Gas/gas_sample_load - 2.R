library(readxl)
library(tidyverse)
library(broom)
library(tibble)
library(data.table)
library(naniar)
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 35/Data Field Visit 35 Repaired.xlsx",
sheet = "Slopes 6.11.19", na = "skip",
n_max = 370)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)
x <- x[-which(is.na(x$time_pnt)),]
x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                   condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)

x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)

s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)
s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2019-06-11", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,6,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1) %>% 
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2019-06-11", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)

z <- left_join(c,n, by = c("date","field","plot","chamber"))
final_flux <- (z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
########34

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 34/Data Field Visit 34.xlsx",
sheet = "Slopes 5.29.19", na = "skip",
n_max = 371)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x <- x[-which(is.na(x$time_pnt)),]
x <- x[-which(is.na(x$co2)),]
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)#x <- x[-which(is.na(x$n2o)),]
 
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)

id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2019-05-29", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,6,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1) %>% 
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2019-05-29", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

####33
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 33/Data Field Visit 33.xlsx",
sheet = "Slopes 5.15.19", na = "skip",
n_max = 372)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)
x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x <- x[-which(is.na(x$n2o)),]
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)

r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2019-05-15", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,6,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1) %>% 
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2019-05-15", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

###32
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 32/Data Field Visit 32.xlsx",
sheet = "Slopes 5.1.19", na = "skip",
n_max = 373)
x <- filter(x,Trip == 32)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)
#x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x <- x[-which(is.na(x$n2o)),]
#x <- x[-which(is.na(x$co2)),]
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)

id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2019-05-01", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,6,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1) %>% 
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2019-05-01", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
###31
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 31/Data Field Visit 31.xlsx",
sheet = "Sheet1",
n_max = 346)
x <- select(x,1,3,4,5,6,8,9,10)

#x <- x[-which(is.na(x$time_pnt)),]
#x <- x[-which(is.na(x$n2o)),]
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(2,3,4,6,7,9,10,12) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()

c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2019-04-17", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1)%>%
 select(2,3,4,9,10,12) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()

n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]

n$date <- as.Date("2019-04-17", format = ("%Y-%m-%d"))
n <- select(n,7,2,3,4,5,6)
z <- left_join(c,n, by = c("date","field","plot","chamber"))
#z <- arrange(z,field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber)

#z <- z %>%  group_by(date,field,plot,chamber,n2o_rsq) %>%   mutate(n2o_slope = mean(n2o_slope),co2_slope = mean(co2_slope)) %>%  select(1,2,3,4,5,6,8,9) %>%   distinct()

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

###30

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 30/Data Field Visit 30.xlsx",
sheet = "Slopes 4.3.19",
n_max = 371)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)

s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2019-04-3", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,6,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1)%>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2019-04-3", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
###29

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 29/Data Field Visit 29.xlsx",
sheet = "Slopes 3.20.19",
n_max = 371)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)

x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)

s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2019-03-20", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,6,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1)%>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2019-03-20", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
###28

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 28/Data Field Visit 28.xlsx",
sheet = "Slopes 3.6.19",
n_max = 371)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2019-03-06", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,6,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1)%>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2019-03-06", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

###27

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 27/Data Field Visit 27.xlsx",
sheet = "2.20.19 ConcSlopes",
n_max = 371)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x <- x[-which(is.na(x$co2)),]
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)

s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2019-02-20", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,6,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1)%>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2019-02-20", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

###26

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 26/Data Field Visit 26.xlsx",
sheet = "2.6.19 ConcSlopes",
n_max = 371)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)

s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2019-02-06", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,6,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1)%>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2019-02-06", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

###25

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 25/Data Field Visit 25.xlsx",
sheet = "1.23.19 ConcSlopes",
n_max = 371)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)

s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2019-01-23", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,6,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1)%>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2019-01-23", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

 
###24

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 24/Data Field Visit 24.xlsx",
sheet = "1.9.19 ConcSlopes",
n_max = 371)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)

s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4

s4 <- left_join(s4,r4)

y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2019-01-09", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,6,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1)%>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2019-01-09", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
###23

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 23/Data Field Visit 23.xlsx",
sheet = "12.12.18 ConcSlopes",
n_max = 371)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)

s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-12-12", format = ("%Y-%m-%d"))
c <- select(c,9,2,3,4,6,7,8)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1)%>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-12-12", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
##22
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 22/Data Field Visit 22.xlsx",
sheet = "11.28.18 ConcSlopes",
n_max = 371)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
#x <- x[-which(is.na(x$n2o)),]
#x <- x[-which(is.na(x$co2)),]

x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)

s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)

id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- (s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,4,5,7) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-11-28", format = ("%Y-%m-%d"))
c <- select(c,7,2,3,4,5,6)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- (s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1)%>%
 select(1,2,3,4,5,6,7) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-11-28", format = ("%Y-%m-%d"))
n <- select(n,8,2,3,4,5,6,7)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
##21

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 21/Data Field Visit 21.xlsx",
sheet = "11.14.18 ConcSlopes",
n_max = 371)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)

s <- x[-which(is.na(x$co2)),]

s1 <- s %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- s %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- s %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- s %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- s %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- s %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- s %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- s %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,8,9,11) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-11-14", format = ("%Y-%m-%d"))
c <- select(c,7,2,3,4,5,6)

##N2O
x <- x[-which(is.na(x$n2o)),]
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- (s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1)%>%
 select(1,2,3,4,5,6,7) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-11-14", format = ("%Y-%m-%d"))
n <- select(n,8,2,3,4,5,7)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,s,x,y,z)
##20

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 20/Data Field Visit 20.xlsx",
sheet = "Sheet2",
n_max = 277)
x <- select(x,3,4,5,6,8,9,10)

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
#x <- x[-which(is.na(x$time_pnt)),]
#x <- x[-which(is.na(x$co2)),]
x <- x[-which(is.na(x$n2o)),]
x %>%  
  group_by(field, plot, chamber) %>%
  tally() %>% 
  filter(n>4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally() %>% 
  filter(n>=4)
x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally() %>% 
  filter(n>=4)
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1, by = c("field","plot","chamber"))
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1, by= "id")
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2, by = "id")
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3, by = "id")
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  summarise(co2_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4, by = "id")
y <- bind_rows(y,s4)

z<- y %>%  filter(co2_rsq<1 & co2_rsq>0.1) %>%
  select(1,2,3,8,9,12) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-10-31", format = ("%Y-%m-%d"))
c <- select(c,7,2,3,4,5,6)

##N2O
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1$mod <- 1
y <-inner_join(x,s1)
r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2$mod <- 2

r2 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 4) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r2)
r2 <- cbind(id=id,r2)
r2$mod <- 2
s2 <- left_join(s2,r2)
y <- bind_rows(y,s2)


s3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 2) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s3)
s3 <- cbind(id=id,s3)
y <-left_join(y,s3)
s3$mod <- 3

r3 <- x %>%
  group_by(field, plot, chamber) %>% 
  filter(time_pnt != 2) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r3)
r3 <- cbind(id=id,r3)
r3$mod <- 3
s3 <- left_join(s3,r3)
y <- bind_rows(y,s3)

s4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s4)
s4 <- cbind(id=id,s4)
s4$mod <- 4

r4 <- x %>%
  group_by(field, plot, chamber) %>%
  filter(time_pnt != 1) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  summarise(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r4)
r4 <- cbind(id=id,r4)
r4$mod <- 4
s4 <- left_join(s4,r4)
y <- bind_rows(y,s4)

z<- y %>%  filter(n2o_rsq<1 & n2o_rsq>0.1)%>%
 select(1,2,3,8,9,11) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]

n$date <- as.Date("2018-10-31", format = ("%Y-%m-%d"))
n <- select(n,7,2,3,4,5,6)
n <- n %>% 
  group_by(date,field,plot,chamber,n2o_rsq) %>% 
  mutate(n2o_slope = mean(n2o_slope)) %>% 
  distinct()

z <- left_join(c,n, by = c("date","field","plot","chamber"))
z <- z %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber)
final_flux <- bind_rows(final_flux,z)
final_flux$season <- 2
final_flux2 <- final_flux

final_flux <- bind_rows(final_flux1,final_flux2)
final_flux <- final_flux [-7]
final_flux <- final_flux [-9]
final_flux <- final_flux [-10]
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z,final_flux1, final_flux2)
