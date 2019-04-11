# full comparison of VIPER energy data
# M. Loranty
# 4 April 2019

# load packages
library(lubridate)
library(dplyr)

# set working directory to lab server 
# files should also be posted at Arctic Data Ctr
setwd('/Volumes/data/data_repo/field_data/viperData/sensor/')

# read radiometer data
nr <- read.csv('campbell/radiation/netR.csv',header = T)

# aggregate to daily
nr.day <- nr %>%
  group_by(year,doy,site,loc,sensorZ) %>%
  summarise(SRup = mean(SR01Up_Avg),
            SRdn = mean(SR01Dn_Avg),
            IRup = mean(IR01UpCo_Avg),
            IRdn = mean(IR01DnCo_Avg))

# subset by site/canopy level
nr.day.hd100 <- nr.day %>%
  filter(site=="hd", sensorZ==100) 
nr.day.hd100$date <- strptime(paste(nr.day.hd100$year,nr.day.hd100$doy,sep="_"),
                        format = "%Y_%j",tz="GMT")

nr.day.ld100 <- nr.day %>%
  filter(site=="ld", sensorZ==100) 
nr.day.ld100$date <- strptime(paste(nr.day.ld100$year,nr.day.ld100$doy,sep="_"),
                              format = "%Y_%j",tz="GMT")

nr.day.hd800 <- nr.day %>%
  filter(site=="hd", sensorZ==800) 
nr.day.hd800$date <- strptime(paste(nr.day.hd800$year,nr.day.hd800$doy,sep="_"),
                              format = "%Y_%j",tz="GMT")

nr.day.ld800 <- nr.day %>%
  filter(site=="ld", sensorZ==800) 
nr.day.ld800$date <- strptime(paste(nr.day.ld800$year,nr.day.ld800$doy,sep="_"),
                              format = "%Y_%j",tz="GMT")


# read soil heatflux data
hf <- read.csv('campbell/heatflux/heatflux.csv', header = T)

hf.day <- hf %>%
  group_by(year,doy,site) %>%
  summarise(shf = mean(shf,na.rm=T),
            stdev = sd(shf,na.rm=T))
head(hf.day)

hf.day.hd <- hf.day %>% 
  filter(site == "hd") 
hf.day.hd$date <- strptime(paste(hf.day.hd$year,hf.day.hd$doy,sep="_"),
                              format = "%Y_%j",tz="GMT")

hf.day.ld <- hf.day %>% 
  filter(site == "ld") 
hf.day.ld$date <- strptime(paste(hf.day.ld$year,hf.day.ld$doy,sep="_"),
                           format = "%Y_%j",tz="GMT")

################################################
# make some plots - set working dir to EGU pres
# timeseries of soil heat flux
plot(hf.day.hd$date,hf.day.hd$shf,col="red",type = "l")
lines(hf.day.ld$date,hf.day.ld$shf,pch=3)

# plot radiometer data
plot(nr.day.hd800$date,nr.day.hd800$SRup,type="p",
     ylim=c(0,400),col="red")
points(nr.day.hd100$date,nr.day.hd100$SRup, col="red",pch=3)
points(nr.day.ld800$date,nr.day.ld800$SRup)
points(nr.day.ld100$date,nr.day.ld100$SRup,pch=3)

#calculate and plot cumulative soil heat flux
hf.day.hd16 <- hf.day %>% 
  filter(year ==2016, site == "hd", 151 < doy & doy < 213) 
hf.day.hd16$cs <- cumsum(hf.day.hd16$shf)

hf.day.hd17 <- hf.day %>% 
  filter(year ==2017, site == "hd", 151 < doy & doy < 213) 
hf.day.hd17$cs <- cumsum(hf.day.hd17$shf)

hf.day.ld16 <- hf.day %>% 
  filter(year ==2016, site == "ld", 151 < doy & doy < 213) 
hf.day.ld16$cs <- cumsum(hf.day.ld16$shf)

hf.day.ld17 <- hf.day %>% 
  filter(year ==2017, site == "ld", 151 < doy & doy < 213) 
hf.day.ld17$cs <- cumsum(hf.day.ld17$shf)

# plot cumulative soil heat flux
pdf("/Volumes/GoogleDrive/Do")
plot(hf.day.ld17$doy,hf.day.ld17$cs*0.0036,type="l",
     ylab = "Cumulative G (MJ)",xlab="")
lines(hf.day.hd17$doy,hf.day.hd17$cs*0.0036,col="red")
lines(hf.day.hd16$doy,hf.day.hd16$cs*0.0036,col="red",lty="dashed")
lines(hf.day.ld16$doy,hf.day.ld16$cs*0.0036,lty="dashed")

#
plot(hf.day.hd16$doy,hf.day.hd16$shf,col="red",lty="solid",lwd=2,type="l",
     xlim = c(160,300),
     ylim = c(-5,17),
     xlab = "DOY",
     ylab = expression(paste("G (W ",m^-2,")",sep="")))

lines(hf.day.ld16$doy,hf.day.ld16$shf,col="black",lty="solid",lwd=2)

plot(hf.day.hd17$doy,hf.day.hd17$shf,col="red",lty="solid",lwd=2,type="l",
     xlim = c(60,300),
     ylim = c(-5,20),
     xlab = "DOY",
     ylab = expression(paste("G (W ",m^-2,")",sep="")))

lines(hf.day.ld17$doy,hf.day.ld17$shf,col="black",lty="solid",lwd=2)