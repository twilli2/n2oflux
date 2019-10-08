library(readxl)
library(tidyverse)
library(broom)
library(tibble)
library(data.table)
library(naniar)
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 19/Data Field Visit 19.xlsx",
sheet = "6.27.18 ConcSlopes", na = "skip",
n_max = 355)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)
x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"), condition = ~ (.x) <= 300)

x <- x[-which(is.na(x$n2o)),]
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-06-27", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-06-27", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)

z <- left_join(c,n, by = c("date","field","plot","chamber"))
z <- filter(z, field != 1)
final_flux <- (z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
########18

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 18/Data Field Visit 18.xlsx",
sheet = "6.13.18 ConcSlopes", na = "skip",
n_max = 355)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x <- x[-which(is.na(x$time_pnt)),]
#x <- x[-which(is.na(x$co2)),]
#x <- x[-which(is.na(x$n2o)),]
 
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-06-13", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-06-13", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
####17
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 17/Data Field Visit 17.xlsx",
sheet = "5.30.18 ConcSlopes", na = "skip",
n_max = 355)
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-05-30", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-05-30", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
###16
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 16/Data Field Visit 16.xlsx",
sheet = "5.16.2018 ConcSlopes", na = "skip",
n_max = 361)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)
x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-05-16", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-05-16", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
###15
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 15/Data Field Visit 15.xlsx",
sheet = "5.2.2018 ConcSlopes",
n_max = 371)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]
#x <- x[-which(is.na(x$n2o)),]
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-05-02", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-05-02", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))
#z <- z %>% 
#  group_by(date,field,plot,chamber,n2o_rsq) %>% 
#  transmute(n2o_slope = mean(n2o_slope), co2_slope = mean(co2_slope)) %>% 
#  distinct()

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

###14

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 14/Data Field Visit 14.xlsx",
sheet = "4.18.18 ConcSlopes",
n_max = 369)
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-04-18", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-04-18", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
###13

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 13/Data Field Visit 13.xlsx",
sheet = "4.4.18 ConcSlopes",
n_max = 365)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-04-04", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-04-04", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
###12

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 12/Data Field Visit 12.xlsx",
sheet = "3.21.18 ConcSlopes",
n_max = 277)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-03-21", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-03-21", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

###11

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 11/Data Field Visit 11.xlsx",
sheet = "3.7.18 ConcSlopes",
n_max = 369)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-03-07", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-03-07", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

###10

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 10/Data Field Visit 10.xlsx",
sheet = "2.21.18 ConcSlopes",
n_max = 369)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-02-21", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-02-21", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

###9

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 9/Data Field Visit 9.xlsx",
sheet = "1.31.18 ConcSlopes",
n_max = 369)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-01-31", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-01-31", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

 
###8

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 8/Data Field Visit 8.xlsx",
sheet = "1.17.18 ConcSlopes",
n_max = 369)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-01-18", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-01-18", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
###7

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 7/Data Field Visit 7.xlsx",
sheet = "1.2.18 ConcSlopes",
n_max = 369)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x[-which(is.na(x$time_pnt)),]

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-01-02", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-01-02", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

##6
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 6/Data Field Visit 6.xlsx",
sheet = "12.12 ConcSlopes",
n_max = 369)
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

z<- y %>%
  select(1,2,3,4,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2017-12-12", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2017-12-12", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))

final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
##5

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 5/Data Field Day 5.xlsx",
sheet = "11.28 ConcSlopes",
n_max = 369)
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2017-11-28", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2017-11-28", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))
z <- filter(z, field != 2)
z$field <- as.character(z$field)
z$chamber <- as.character(z$chamber)
final_flux <- bind_rows(final_flux,z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
##4

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 4/Data Field Day 4.xlsx",
sheet = "11.4.2018 ConcSlopes",
n_max = 369)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x <- x[-which(is.na(x$time_pnt)),]
#x <- x[-which(is.na(x$co2)),]
#x <- x[-which(is.na(x$n2o)),]
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2017-11-14", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
  
 
  n$date <- as.Date("2017-11-14", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))
#z <- z[-7]
#z <- z[-9]
#z <- z %>% 
#  group_by(date,field,plot,chamber,n2o_rsq) %>% 
#  transmute(n2o_slope = mean(n2o_slope), co2_slope = mean(co2_slope)) %>% 
#  distinct()
z <- z %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber)
final_flux <- bind_rows(final_flux,z)
final_flux$season <- 1
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
##3
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 3/Data Field Day 3.xlsx",
sheet = "10.31 ConcSlopes",
n_max = 369)
x <- rename(x, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,2,3,4,5,7,8,12)

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x <- x[-which(is.na(x$time_pnt)),]
#x <- x[-which(is.na(x$co2)),]
#x <- x[-which(is.na(x$n2o)),]
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2017-10-31", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
  
n$date <- as.Date("2017-10-31", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))
z <- z %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber)
z$field <- as.character(z$field)
z$chamber <- as.character(z$chamber)
final_flux <- bind_rows(final_flux,z)
final_flux$season <- 1
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)

#2
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 2/Data Field Day 2.xlsx",
sheet = "Slopes 10.17.19",
n_max = 357)
x <- rename(x, date = 'Sample Date', field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,3,4,5,6,8,9,15)
x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
#x <- x[-which(is.na(x$time_pnt)),]
#x <- x[-which(is.na(x$co2)),]
#x <- x[-which(is.na(x$n2o)),]
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2017-10-17", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]

n$date <- as.Date("2017-10-17", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))
#z <- z[-7]
#z <- z[-9]
#z <- z %>% 
#  group_by(date,field,plot,chamber,n2o_rsq) %>% 
#  transmute(n2o_slope = mean(n2o_slope), co2_slope = mean(co2_slope)) %>% 
#  distinct()
z <- z %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber)
final_flux <- bind_rows(final_flux,z)
final_flux$season <- 1
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z)
###1
x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 1/Field Visit 1.xlsx",
sheet = "10.03 ConcSlopes",
n_max = 357)
x <- rename(x, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- select(x,2,3,4,5,7,8,12)

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"),
                     condition = ~ (.x) <= 300)
x <- x[-which(is.na(x$time_pnt)),]
x <- x[-which(is.na(x$co2)),]
x <- x[-which(is.na(x$n2o)),]
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

z<- y %>%
  select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2017-10-03", format = ("%Y-%m-%d"))
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

z<- y %>%
 select(1,2,3,7,8,9,11,10) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]

n$date <- as.Date("2017-10-03", format = ("%Y-%m-%d"))
n <- select(n,9,2,3,4,6,7,8)
z <- left_join(c,n, by = c("date","field","plot","chamber"))
z <- z %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber)
z$field <- as.character(z$field)
z$chamber <- as.character(z$chamber)

final_flux <- bind_rows(final_flux,z)

final_flux <- final_flux [-11]
final_flux <- final_flux [-11]
final_flux <- final_flux [-11]
final_flux$season <- 1
final_flux1 <- final_flux
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z,final_flux)

