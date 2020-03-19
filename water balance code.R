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
names(prcp) <- c("Site", "Lon", "Lat", "Elev", "Date", "ppt_mm")
prcp  <- subset(prcp, select = -c(Lon, Lat, Elev))

p1 <- prcp %>%
  mutate(SiteNum = as.numeric(Site)) %>% 
  arrange(SiteNum, Date) %>%
  filter(!is.na(ppt_mm))

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
cs$Crop[100:101] <- "HAYP"
cs$Growth[100] <- "emergence"
cs$Date[69] <- "2014-01-01"
cs <- cs[-92, ] 
cs$Crop[which(cs$Growth == "planted")] <- "BARE"

