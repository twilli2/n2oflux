install.packages("here")
library(here)
library(leaflet)
library(zoo)
library(reshape2)
library(lubridate)
library(tidyverse)

p <- here::here("data", "PRISM")
prcp <- list()

multmerge <- function(mypath){
  filenames <- list.files(path = mypath, full.names=TRUE)
  datalist <- lapply(filenames, function(x){read.csv(file = x, header = TRUE, skip = 10, stringsAsFactors = FALSE)})
  prcp <- do.call(rbind, datalist)
  return(prcp)
}
prcp <- multmerge(p) 


summary(prcp)

prcp <- read.csv(file = "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/R/Willamette Project/data/PRISM/PRISM_ppt_stable_4km_20180101_20180630.csv", header = TRUE, skip = 10, stringsAsFactors = FALSE)
prcp <- bind_rows(prcp, read.csv(file = "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/R/Willamette Project/data/PRISM/PRISM_ppt_stable_4km_20180701_20181231.csv", header = TRUE, skip = 10, stringsAsFactors = FALSE))
prcp <- bind_rows(prcp, read.csv(file = "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/R/Willamette Project/data/PRISM/PRISM_ppt_stable_4km_20190101_20190630.csv", header = TRUE, skip = 10, stringsAsFactors = FALSE))
prcp <- bind_rows(prcp, read.csv(file = "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/R/Willamette Project/data/PRISM/PRISM_ppt_provisional_4km_20190701_20191231.csv", header = TRUE, skip = 10, stringsAsFactors = FALSE))

names(prcp) <- c("field", "Lon", "Lat", "Elev", "date", "ppt_mm")
prcp  <- subset(prcp, select = -c(Lon, Lat, Elev))
prcp$date <- as.Date(prcp$date)

prcp_sum <- function(fromdate,todate){
  x <- prcp %>%
  filter(date >= fromdate & date <= todate) %>%
  group_by(field) %>% 
  summarize(total = sum(ppt_mm))
  precip <- x
  return(precip)
}
z <- prcp_sum('2018-02-28','2018-03-13')  
z$sample_date <- '2018-03-13'

z1 <- prcp_sum('2018-03-14','2018-03-27')
z1$sample_date <-  '2018-03-27'

z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-03-28','2018-04-10')
z1$sample_date <-'2018-04-10'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-04-11','2018-04-24')
z1$sample_date <-'2018-04-24'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-04-25','2018-05-08')
z1$sample_date <-'2018-05-08'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-05-09','2018-05-22')
z1$sample_date <-'2018-05-22'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-05-23','2018-06-05')
z1$sample_date <-'2018-06-05'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-06-06','2018-06-19')
z1$sample_date <-'2018-06-19'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-06-20','2018-07-03')
z1$sample_date <-'2018-07-03'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-07-04','2018-07-28')
z1$sample_date <-'2018-07-28'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-07-29','2018-08-03')
z1$sample_date <-'2018-08-03'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-08-04','2018-08-28')
z1$sample_date <-'2018-08-28'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-08-29','2018-09-03')
z1$sample_date <-'2018-09-03'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-09-04','2018-09-28')
z1$sample_date <-'2018-09-28'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-09-29','2018-10-03')
z1$sample_date <-'2018-10-03'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-10-04','2018-10-28')
z1$sample_date <-'2018-10-28'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-10-29','2018-11-03')
z1$sample_date <-'2018-11-03'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-11-04','2018-11-28')
z1$sample_date <-'2018-11-28'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-11-29','2018-12-03')
z1$sample_date <-'2018-12-03'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-12-04','2018-12-28')
z1$sample_date <-'2018-12-28'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2018-12-29','2019-01-03')
z1$sample_date <-'2019-01-03'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-01-04','2019-01-15')
z1$sample_date <-'2019-01-15'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-01-16','2019-01-29')
z1$sample_date <-'2019-01-29'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-01-30','2019-02-12')
z1$sample_date <-'2019-02-12'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-02-13','2019-03-04')
z1$sample_date <-'2019-03-04'
z <- bind_rows(z, z1)
                 
z1 <- prcp_sum('2019-03-05','2019-03-12')
z1$sample_date <-'2019-03-12'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-03-13','2019-03-26')
z1$sample_date <-'2019-03-26'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-03-27','2019-04-09')
z1$sample_date <-'2019-04-09'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-04-10','2019-04-15')
z1$sample_date <-'2019-04-15'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-04-16','2019-04-23')
z1$sample_date <-'2019-04-23'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-04-24','2019-05-07')
z1$sample_date <-'2019-05-07'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-05-08','2019-05-21')
z1$sample_date <-'2019-05-21'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-05-22','2019-06-04')
z1$sample_date <-'2019-06-04'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-06-05','2019-06-18')
z1$sample_date <-'2019-06-18'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-06-19','2019-07-01')
z1$sample_date <-'2019-07-01'
z <- bind_rows(z, z1)

z1 <- prcp_sum('2019-07-02','2019-07-16')
z1$sample_date <-'2019-07-16'
z <- bind_rows(z, z1)


write.csv(z, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/prcp.csv")

# --------- evapotranspiration ----------------
# specify a path to the reference evapotransipiration data file
p <- here::here("data", "Crop_ET", "daily_data.csv")
et <- read.csv(p, header = TRUE, skip = 13, stringsAsFactors = FALSE)
et$DATE <- as.Date(et$DATE, format = '%m/%d/%Y')
# select reference ET (ETr, method of ASCE-EWRI) for alfalfa and grass and convert inches --> mm
cf <- 25.4
et <- et %>%
  select(DATE) %>%
  mutate(ETr_alfalfa_mm = as.numeric(et$CRVO.ETRS)*cf) %>%
  mutate(ETr_grass_mm = as.numeric(et$CRVO.ETOS)*cf) 
colnames(et)[1] <- "Date"
# ---------- AgriMet crop water use -------------
# 2017
p <- here::here("data", "Crop_ET", "crvo17et.txt")
e1 <- read.table(p, header = TRUE, skip = 2, stringsAsFactors = FALSE)
e1 <- e1[-c(1),] %>%
  mutate(Date = as.Date(paste0(DATE,"/2017"), format = '%m/%d/%Y'))
e1[,2:ncol(e1)-1] <- as.data.frame(lapply(e1[,2:ncol(e1)-1], as.numeric))
# 2019
p <- here::here("data", "Crop_ET", "crvo19et.txt")
e2 <- read.table(p, header = TRUE, skip = 2, stringsAsFactors = FALSE)
e2 <- e2[-c(1),] %>%
  mutate(Date = as.Date(paste0(DATE,"/2019"), format = '%m/%d/%Y'))
e2[,2:ncol(e2)-1] <- as.data.frame(lapply(e2[,2:ncol(e2)-1], as.numeric))

# select crops and set non-reporting days to zero
e <- bind_rows(e1,e2)
e <- subset(e, select = c(Date, LAWN, GRSD, BLUB, WGRN, PPMT, PEAS, SPNC, SQSH, CBBG, HZLN))
names(e) <- c("Date", "LAWN", "GRASS SEED", "BLUEBERRIES", "WNTR GRN", "PPMT", "PEAS", "SPNC", "SQUASH", "CABBAGE", "HAZELNUT")
e <- melt(e, id.var = 'Date', variable.name = 'Cover', value.name = "AgriMET_ET")
e[is.na(e)] <- 0
# convert to mm
e$AgriMET_ET <- e$AgriMET_ET*cf
e$Cover <- as.character(e$Cover)
rm(e1,e2,e3,e4)        
# ------------ crop coefficients -----------------
p <- here::here("data", "Crop_ET", "crop_coeff.csv")
cc <- read.csv(p, header = FALSE, skip = 3, stringsAsFactors = FALSE)
cc <- cc[,2:23]
# ------------- crop schedules ------------------
p <- here::here("data", "Crop_ET", "crop_sched_gaps_SLP.csv")
cs <- read.csv(p, header = TRUE, stringsAsFactors = FALSE)
cs$SiteNum <- as.numeric(substr(cs$Site, 1, 4))
cs$Date <- as.Date(cs$Date, format = '%m/%d/%Y')
cssum <- cs %>% group_by(SiteNum) %>% summarise(sched = paste(unique(Cover), collapse = "|"))
# adjustments to crop schedule for continuous Kc calculations
cs$Crop[23:24] <- "HAYP"
cs$Growth[24] <- "emergence"
cs$Date[24] <- "2018-01-01"
cs <- cs[-92, ] 
cs$Crop[which(cs$Growth == "planted")] <- "BARE"
# ----------- soil moisture --------------------
sm <- joined_env_data %>% 
  select(1,2,12)
sm %>% 
  group_by(field)

write.csv(sm, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/soil_moisture.csv")
sm <- read.csv("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/soil_moisture.csv")
sm$date <- as.Date(sm$date)
sm_sum <- function(fromdate,todate){
  x <- sm %>%
  filter(date == fromdate | date == todate) %>%
  group_by(field) %>% 
  summarize(diff = diff(mean_avg_moist))
  return(x)
}

z <- sm_sum('2018-02-28','2018-03-13')  
z$sample_date <- '2018-03-13'

z1 <- sm_sum('2018-03-14','2018-03-27')
z1$sample_date <-  '2018-03-27'

z <- bind_rows(z, z1)

z1 <- sm_sum('2018-03-29','2018-04-10')
z1$sample_date <-'2018-04-10'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-04-11','2018-04-24')
z1$sample_date <-'2018-04-24'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-04-25','2018-05-08')
z1$sample_date <-'2018-05-08'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-05-09','2018-05-22')
z1$sample_date <-'2018-05-22'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-05-23','2018-06-05')
z1$sample_date <-'2018-06-05'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-06-06','2018-06-19')
z1$sample_date <-'2018-06-19'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-06-20','2018-07-03')
z1$sample_date <-'2018-07-03'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-07-04','2018-07-28')
z1$sample_date <-'2018-07-28'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-07-29','2018-08-03')
z1$sample_date <-'2018-08-03'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-08-04','2018-08-28')
z1$sample_date <-'2018-08-28'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-08-29','2018-09-03')
z1$sample_date <-'2018-09-03'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-09-04','2018-09-28')
z1$sample_date <-'2018-09-28'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-09-29','2018-10-03')
z1$sample_date <-'2018-10-03'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-10-04','2018-10-28')
z1$sample_date <-'2018-10-28'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-10-29','2018-11-03')
z1$sample_date <-'2018-11-03'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-11-04','2018-11-28')
z1$sample_date <-'2018-11-28'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-11-29','2018-12-03')
z1$sample_date <-'2018-12-03'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-12-04','2018-12-28')
z1$sample_date <-'2018-12-28'
z <- bind_rows(z, z1)

z1 <- sm_sum('2018-12-29','2019-01-03')
z1$sample_date <-'2019-01-03'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-01-04','2019-01-15')
z1$sample_date <-'2019-01-15'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-01-16','2019-01-29')
z1$sample_date <-'2019-01-29'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-01-30','2019-02-12')
z1$sample_date <-'2019-02-12'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-02-13','2019-03-04')
z1$sample_date <-'2019-03-04'
z <- bind_rows(z, z1)
                 
z1 <- sm_sum('2019-03-05','2019-03-20')
z1$sample_date <-'2019-03-12'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-03-20','2019-03-26')
z1$sample_date <-'2019-03-26'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-03-27','2019-04-09')
z1$sample_date <-'2019-04-09'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-04-10','2019-04-15')
z1$sample_date <-'2019-04-15'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-04-16','2019-04-23')
z1$sample_date <-'2019-04-23'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-04-24','2019-05-07')
z1$sample_date <-'2019-05-07'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-05-08','2019-05-21')
z1$sample_date <-'2019-05-21'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-05-22','2019-06-04')
z1$sample_date <-'2019-06-04'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-06-05','2019-06-18')
z1$sample_date <-'2019-06-18'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-06-19','2019-07-01')
z1$sample_date <-'2019-07-01'
z <- bind_rows(z, z1)

z1 <- sm_sum('2019-07-02','2019-07-16')
z1$sample_date <-'2019-07-16'
z <- bind_rows(z, z1)

write.csv(z, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/soil_moisture_calc.csv")
sm_final <- read.csv("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/soil_moisture_calc.csv")
sm_final$sample_date <- as.Date(sm_final$sample_date)

#sm_final<- sm_final %>%
#  group_by(field) %>% 
#  arrange(sample_date) %>% 
#  mutate(diff = total - lag(total, default = first(total), order_by = sample_date))
sm_final <- sm_final %>% 
  mutate(dS1000=(diff*1000))

# ------ first, planted fields ----------
Kc <- list()
for (i in 1:(nrow(cs)-1)){
  
  # determine crop and select crop coefficient data to use
  crop <- cs$Crop[i]
  crop_cc <- as.numeric(cc[match(crop, cc$V23), 1:21])
  
  # set Kc to zero when no crop curve is available
  if (is.na(max(crop_cc))){
    crop_cc <- rep(0, 21)
    crop_cc[-1] <- 0.001
  }
  
  # determine growth stage 
  growth <- cs$Growth[i]
  
  # determine julian days between now and the next date in the crop schedule
  t1 <- julian(cs$Date[i])[1]
  t2 <- julian(cs$Date[i+1])[1] - 1
  
  # specify which julian days to interpolate
  if (t2 > t1 && !is.na(crop)){
  
    xout <- seq(t1, t2, 1)
  
    # create a list of crop names and site numbers to merge with Kc estimates
    Crop <- rep(crop, length(xout))
    Cover <- rep(crop, length(xout))
    site <- rep(cs$SiteNum[i], length(xout))
  
    # interpolate along crop curve between changes in field cover 
    if (growth == "emergence"){
      # -- emergence to peak --
      y <- crop_cc[1:which.max(crop_cc)]
      x <- round(seq(t1,t2, length.out = length(y)))
      yout <- approx(x,y,xout)$y
    }else if (growth == "peak"){
      # -- peak to harvest --
      y <- crop_cc[which.max(crop_cc):21]
      x <- round(seq(t1,t2, length.out = length(y)))
      yout <- approx(x,y,xout)$y
    }else if (growth == "harvest"){  
      # -- interpolate between last and first points on crop curve --
      y <- c(crop_cc[21], crop_cc[1])
      x <- c(t1,t2)
      yout <- approx(x,y,xout)$y
    }else if (growth == "grass harvest"){  
      # -- set grass Kc back to 80% of peak after harvest --
      y <- c(0.69, crop_cc[1])
      x <- c(t1,t2)
      yout <- approx(x,y,xout)$y
    }else if (growth == "ppmt harvest"){
      # -- set peppermint back to 45% of peak after harvest --
      y <- c(0.43, crop_cc[1])
      x <- c(t1,t2)
      yout <- approx(x,y,xout)$y
    }else{
      # -- bare ground case, set to zero until bare ground calculations (below) --
      y <- c(0,0)
      x <- round(seq(t1,t2, length.out = length(y)))
      yout <- approx(x,y,xout)$y
    }
    
    Kc[[i]] <- data.frame(jday = xout, Kc = yout, Cover = Cover, SiteNum = site)
  }
}
Kc <- do.call(rbind, Kc)
Kc$Cover <- as.character(Kc$Cover)
ev <- read_excel("data/Jake's Water Budget-TW.xls",
sheet = "evt")
ev <- gather(ev, value = et_mm, field, 'F1 ET (mm)':'F4 ET (mm)')
ev$field[ev$field == "F1 ET (mm)"] <- 1
ev$field[ev$field == "F2 ET (mm)"] <- 2
ev$field[ev$field == "F3 ET (mm)"] <- 3
ev$field[ev$field == "F4 ET (mm)"] <- 4

ev$date <- as.Date(ev$date)

ev_sum <- function(fromdate,todate){
  x <- ev %>%
  filter(date >= fromdate & date <= todate) %>%
  group_by(field) %>% 
  summarize(total_et = sum(et_mm))
  return(x)
}
z <- ev_sum('2018-02-28','2018-03-13')  
z$sample_date <- '2018-03-13'

z1 <- ev_sum('2018-03-14','2018-03-27')
z1$sample_date <-  '2018-03-27'

z <- bind_rows(z, z1)

z1 <- ev_sum('2018-03-28','2018-04-10')
z1$sample_date <-'2018-04-10'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-04-11','2018-04-24')
z1$sample_date <-'2018-04-24'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-04-25','2018-05-08')
z1$sample_date <-'2018-05-08'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-05-09','2018-05-22')
z1$sample_date <-'2018-05-22'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-05-23','2018-06-05')
z1$sample_date <-'2018-06-05'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-06-06','2018-06-19')
z1$sample_date <-'2018-06-19'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-06-20','2018-07-03')
z1$sample_date <-'2018-07-03'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-07-04','2018-07-28')
z1$sample_date <-'2018-07-28'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-07-29','2018-08-03')
z1$sample_date <-'2018-08-03'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-08-04','2018-08-28')
z1$sample_date <-'2018-08-28'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-08-29','2018-09-03')
z1$sample_date <-'2018-09-03'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-09-04','2018-09-28')
z1$sample_date <-'2018-09-28'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-09-29','2018-10-03')
z1$sample_date <-'2018-10-03'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-10-04','2018-10-28')
z1$sample_date <-'2018-10-28'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-10-29','2018-11-03')
z1$sample_date <-'2018-11-03'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-11-04','2018-11-28')
z1$sample_date <-'2018-11-28'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-11-29','2018-12-03')
z1$sample_date <-'2018-12-03'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-12-04','2018-12-28')
z1$sample_date <-'2018-12-28'
z <- bind_rows(z, z1)

z1 <- ev_sum('2018-12-29','2019-01-03')
z1$sample_date <-'2019-01-03'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-01-04','2019-01-15')
z1$sample_date <-'2019-01-15'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-01-16','2019-01-29')
z1$sample_date <-'2019-01-29'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-01-30','2019-02-12')
z1$sample_date <-'2019-02-12'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-02-13','2019-03-04')
z1$sample_date <-'2019-03-04'
z <- bind_rows(z, z1)
                 
z1 <- ev_sum('2019-03-05','2019-03-12')
z1$sample_date <-'2019-03-12'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-03-13','2019-03-26')
z1$sample_date <-'2019-03-26'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-03-27','2019-04-09')
z1$sample_date <-'2019-04-09'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-04-10','2019-04-15')
z1$sample_date <-'2019-04-15'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-04-16','2019-04-23')
z1$sample_date <-'2019-04-23'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-04-24','2019-05-07')
z1$sample_date <-'2019-05-07'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-05-08','2019-05-21')
z1$sample_date <-'2019-05-21'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-05-22','2019-06-04')
z1$sample_date <-'2019-06-04'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-06-05','2019-06-18')
z1$sample_date <-'2019-06-18'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-06-19','2019-07-01')
z1$sample_date <-'2019-07-01'
z <- bind_rows(z, z1)

z1 <- ev_sum('2019-07-02','2019-07-16')
z1$sample_date <-'2019-07-16'
z <- bind_rows(z, z1)
z <- z %>% 
  mutate(total_et=total_et*2.54)

write.csv(z, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/et.csv")
prcp <- read_csv("~/Dropbox/Lab data/S Willamette GWMA Dropbox/prcp.csv")
et <- read_csv("~/Dropbox/Lab data/S Willamette GWMA Dropbox/et.csv")
#irrigation
i <- joined_env_data %>% 
  select(1,2,3)
i_sum <- function(fromdate,todate){
  x <- i %>%
  filter(date >= fromdate & date <= todate) %>%
  group_by(field) %>% 
  summarize(total_i = sum(total_precip))
  return(x)
}
z <- i_sum('2018-02-28','2018-03-13')  
z$sample_date <- '2018-03-13'

z1 <- i_sum('2018-03-14','2018-03-27')
z1$sample_date <-  '2018-03-27'

z <- bind_rows(z, z1)

z1 <- i_sum('2018-03-28','2018-04-10')
z1$sample_date <-'2018-04-10'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-04-11','2018-04-24')
z1$sample_date <-'2018-04-24'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-04-25','2018-05-08')
z1$sample_date <-'2018-05-08'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-05-09','2018-05-22')
z1$sample_date <-'2018-05-22'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-05-23','2018-06-05')
z1$sample_date <-'2018-06-05'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-06-06','2018-06-19')
z1$sample_date <-'2018-06-19'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-06-20','2018-07-03')
z1$sample_date <-'2018-07-03'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-07-04','2018-07-28')
z1$sample_date <-'2018-07-28'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-07-29','2018-08-03')
z1$sample_date <-'2018-08-03'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-08-04','2018-08-28')
z1$sample_date <-'2018-08-28'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-08-29','2018-09-03')
z1$sample_date <-'2018-09-03'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-09-04','2018-09-28')
z1$sample_date <-'2018-09-28'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-09-29','2018-10-03')
z1$sample_date <-'2018-10-03'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-10-04','2018-10-28')
z1$sample_date <-'2018-10-28'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-10-29','2018-11-03')
z1$sample_date <-'2018-11-03'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-11-04','2018-11-28')
z1$sample_date <-'2018-11-28'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-11-29','2018-12-03')
z1$sample_date <-'2018-12-03'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-12-04','2018-12-28')
z1$sample_date <-'2018-12-28'
z <- bind_rows(z, z1)

z1 <- i_sum('2018-12-29','2019-01-03')
z1$sample_date <-'2019-01-03'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-01-04','2019-01-15')
z1$sample_date <-'2019-01-15'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-01-16','2019-01-29')
z1$sample_date <-'2019-01-29'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-01-30','2019-02-12')
z1$sample_date <-'2019-02-12'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-02-13','2019-03-04')
z1$sample_date <-'2019-03-04'
z <- bind_rows(z, z1)
                 
z1 <- i_sum('2019-03-05','2019-03-12')
z1$sample_date <-'2019-03-12'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-03-13','2019-03-26')
z1$sample_date <-'2019-03-26'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-03-27','2019-04-09')
z1$sample_date <-'2019-04-09'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-04-10','2019-04-15')
z1$sample_date <-'2019-04-15'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-04-16','2019-04-23')
z1$sample_date <-'2019-04-23'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-04-24','2019-05-07')
z1$sample_date <-'2019-05-07'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-05-08','2019-05-21')
z1$sample_date <-'2019-05-21'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-05-22','2019-06-04')
z1$sample_date <-'2019-06-04'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-06-05','2019-06-18')
z1$sample_date <-'2019-06-18'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-06-19','2019-07-01')
z1$sample_date <-'2019-07-01'
z <- bind_rows(z, z1)

z1 <- i_sum('2019-07-02','2019-07-16')
z1$sample_date <-'2019-07-16'
z <- bind_rows(z, z1)
write.csv(z, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/i.csv")
i <- read.csv("C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/i.csv")
i$field <- as.factor(i$field)
i$sample_date <- as.Date(i$sample_date)

water_budget <- left_join(prcp,et, by = c("field", "sample_date"))
water_budget<- water_budget[-1]
water_budget<- water_budget[-4]
water_budget$field <- as.factor(water_budget$field)
water_budget$sample_date <- as.Date(water_budget$sample_date)
water_budget <- left_join(water_budget,i)
water_budget<- rename(water_budget, total_precip = total)
water_budget<- water_budget %>% 
  mutate(i = total_i-total_precip)
water_budget$i[water_budget$i <= 0] <- 0
water_budget$i[water_budget$field == 1] <- 0
water_budget$i[water_budget$field == 3] <- water_budget$i[water_budget$field == 4]
water_budget <- water_budget[-6]
sm_final$field <- as.factor(sm_final$field)
water_budget <- left_join(water_budget,sm_final)
c <- water_budget %>% 
  mutate(ie = total_et-total_precip)
c %>% 
if_else(ie > 0,0,.)

drainage <- water_budget %>% 
  mutate(Dmm = -1*(dS1000-total_precip+total_et-i)) %>% 
  select(3,1,7)
drainage$Dmm[drainage$Dmm<0] <- 0
