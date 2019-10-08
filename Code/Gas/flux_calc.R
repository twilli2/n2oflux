library(readxl)
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "6-11-19", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2019-06-11", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- f
###
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "5-29-19", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2019-05-29", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field)
flux <- bind_rows(flux,f)

##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "5-15-19", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2019-05-15", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "5-1-19", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2019-05-01", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "4-17-19", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2019-04-17", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "4-3-19", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2019-04-03", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "3-20-19", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2019-03-20", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "3-6-19", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2019-03-06", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "2-20-19", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2019-02-20", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "2-6-19", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2019-02-06", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "1-23-19", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2019-01-23", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "1-9-19", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2019-01-09", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "12-12-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-12-12", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "11-28-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-11-28", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "11-14-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-11-14", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "10-31-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-10-31", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
flux <- bind_rows(flux,f)
flux2 <- flux
flux2$season <- 2
rm(flux,f)
###
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "06-27-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-6-27", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
flux <- (f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "06-13-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-6-13", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
flux <- bind_rows(flux,f)
###
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "5-30-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-5-30", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
flux <- bind_rows(flux,f)
###
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "05-16-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-5-16", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
f$temp <- as.numeric(f$temp)
flux <- bind_rows(flux,f)
###
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "05-02-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-5-02", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
f$temp <- as.numeric(f$temp)
flux <- bind_rows(flux,f)
###
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "4-18-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-4-18", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
f$temp <- as.numeric(f$temp)
flux <- bind_rows(flux,f)
###
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "4-4-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-4-4", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
f$temp <- as.numeric(f$temp)
flux <- bind_rows(flux,f)
###
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "3-21-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-3-21", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
f$temp <- as.numeric(f$temp)
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "3-7-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-3-7", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
f$temp <- as.numeric(f$temp)
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "2-21-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-2-21", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
f$temp <- as.numeric(f$temp)
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "1-31-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-1-31", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
f$temp <- as.numeric(f$temp)
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "1-18-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-1-18", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
f$temp <- as.numeric(f$temp)
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "1-2-18", skip = 16)
f <- select(f,1,2,3,4,7,9,11,13,16)
f$date=as.Date("2018-1-2", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
f$temp <- as.numeric(f$temp)
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "12-12", skip = 16)
f <- select(f,1,2,3,4,7,9,10,12,15)
f$date=as.Date("2017-12-12", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "11-28", skip = 16)
f <- select(f,1,2,3,4,7,9,10,12,15)
f$date=as.Date("2017-11-28", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "11-14", skip = 16)
f <- select(f,1,2,3,4,7,9,10,12,15)
f$date=as.Date("2017-11-14", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = Plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "10-31", skip = 16)
f <- select(f,1,2,3,4,9,11,12,14,17)
f$date=as.Date("2017-10-31", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
flux <- bind_rows(flux,f)
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "10-17", skip = 16)
f <- select(f,1,2,3,4,7,9,10,12,15)
f$date=as.Date("2017-10-17", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "X__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
flux <- bind_rows(flux,f)
##
##
f <- read_excel("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Flux Data/Slopes and Flux.xls",
sheet = "10-3", skip = 16)
f <- select(f,1,2,3,4,9,11,12,14,20)
f$date=as.Date("2017-10-3", format = ("%Y-%m-%d"))
f<- f[-1,]
f$chamber<- as.character(f$chamber)
f <- rename(f, field = Field, plot = plot, vol = "Vol (L)", area = "A (m^2)", temp = "Temp K", soil_temp = "Temp C__1",atm = "(atm)")
f$field <- as.character(f$field) 
#f <- filter(f, field != 1) 
f$soil_temp <- as.numeric(f$soil_temp)
flux <- bind_rows(flux,f)
flux$season <- 1
flux <- bind_rows(flux,flux2)
rm(f,flux2)
##
s <- select(final_flux, date,field,plot,chamber,co2_slope,co2_rsq,n2o_slope,n2o_rsq, season)
s$plot[s$plot == "GN0"] <- "N0"
s$plot[s$plot == "100"] <- "N100"
s$plot[s$plot == "CNV"] <- "Conv"
s$plot[s$plot == "PAG"] <- "PA"
s$plot[s$plot == "50"] <- "N50"
s$plot[s$plot == "75"] <- "N75"
s$plot[s$plot == "C"] <- "Conv"
s$plot[s$plot == "P"] <- "PA"
s$plot[s$plot == "25"] <- "N25"
s$plot[s$plot == "0"] <- "N0"

p <- left_join(s,flux)
p <- mutate(p,co2_flux = ((co2_slope*vol*atm*(12.011)*(60)*(1*10^-6))/(temp*area*(0.08206))))
p <- mutate(p,n2o_flux = ((n2o_slope*vol*atm*(28.0134)*(60)*(1*10^3))/(temp*(area*10000)*(0.08206))))
p$plot[p$plot == "N0"] <- "0"
p$plot[p$plot == "N25"] <- "25"
p$plot[p$plot == "N50"] <- "50"
p$plot[p$plot == "N75"] <- "75"
p$plot[p$plot == "N100"] <- "100"
p$plot[p$plot == "Conv"] <- "C"
p$plot[p$plot == "PA"] <- "P"
flux_data <- p
rm(f,s,flux)
