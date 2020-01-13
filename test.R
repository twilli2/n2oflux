?glm
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

x <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Gas Sample Data/Field Visit 19/Data Field Visit 19.xlsx",
sheet = "6.27.18 ConcSlopes", na = "skip",
n_max = 355)
x <- rename(x, date = `Sample Date`, field = 'FIELD #', plot = 'N ADDITION/TREATMENT', chamber = "CHAMBER", co2 = `[CO2] PPM`, time_pnt = `TIME POINT`, n2o = `[N2O] PPM`, time = 'Minutes')
x <- filter(x,Trip == "19")
x <- select(x,3,4,5,6,8,9,15)

x <- x %>% 
  replace_with_na_at(.vars = c("n2o"),
                     condition = ~ (.x) <= 0.20)
x <- x %>% 
  replace_with_na_at(.vars = c("co2"), condition = ~ (.x) <= 300)

x <- x[-which(is.na(x$n2o)),]
x <- x[-which(is.na(x$co2)),]
t1 <- x %>%  
  group_by(field, plot, chamber) %>%
  tally()
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time")%>%    
  select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)

id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1 <- left_join(s1,t1)
s1$mod <- 1
y <-inner_join(x,s1)

r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(co2~time,data = .)) %>%
  mutate(co2_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

t2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally()

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(co2 ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(co2_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2 <- left_join(s2,t2)
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
t3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally()
s3 <- left_join(s3,t3)
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
t4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally()
s4 <- left_join(s4,t4)
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
  filter(n>2) %>% 
  select(1,2,3,8,9,11,12) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
c <- setDT(z)[, .SD[which.max(co2_rsq)], id]
c$date <- as.Date("2018-06-27", format = ("%Y-%m-%d"))
c <- filter(c,co2_rsq > 0.2)
c <- select(c,8,2,3,4,5,7)

#n20
t1 <- x %>%  
  group_by(field, plot, chamber) %>%
  tally()
s1 <- x %>%  
  group_by(field, plot, chamber) %>%
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time")%>%    
  select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)

id <- rownames(s1)
s1 <- cbind(id=id, s1)
s1 <- left_join(s1,t1)
s1$mod <- 1
y <-inner_join(x,s1)

r1 <- x %>%
  group_by(field, plot, chamber) %>% 
  do(mod = lm(n2o~time,data = .)) %>%
  mutate(n2o_rsq = summary(mod)$r.squared)
id <- rownames(r1)
r1 <- cbind(id=id,r1)
r1$mod <- 1
s1 <- left_join(s1,r1)
y<- inner_join(y,s1)

t2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>%
  tally()

s2 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 4) %>% 
  do(tidy(lm(n2o ~ time, data = .))) %>%   
  filter(term == "time") %>%    select(field, plot, chamber, estimate) %>%   
  rename(n2o_slope = estimate)
id <- rownames(s2)
s2 <- cbind(id=id,s2)
s2 <- left_join(s2,t2)
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
t3 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 2) %>%
  tally()
s3 <- left_join(s3,t3)
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
t4 <- x %>%  
  group_by(field, plot, chamber) %>%
  filter(time_pnt  != 1) %>%
  tally()
s4 <- left_join(s4,t4)
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
  filter(n>2) %>% 
  select(1,2,3,8,9,11,12) %>% 
  arrange(field, match(plot, c("GN0","N25","N50", "N75", "100", "CNV", "PAG")), chamber) %>%
  distinct()
n <- setDT(z)[, .SD[which.max(n2o_rsq)], id]
n$date <- as.Date("2018-06-27", format = ("%Y-%m-%d"))
n <- filter(n,n2o_rsq > 0.2)
n <- select(n,8,2,3,4,5,7)

z <- left_join(c,n, by = c("date","field","plot","chamber"))
z <- filter(z, field != 1)
final_flux <- (z)
rm(c,id,n,r1,r2,r3,r4,s1,s2,s3,s4,x,y,z,t1,t2,t3,t4)
