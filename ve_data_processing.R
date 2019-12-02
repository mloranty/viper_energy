#######################################
# full comparison of VIPER energy data
# 
# initially for EGU & AGU 2019
# & eventually for manuscript
# M. Loranty
# 25 Nov 2019
#######################################

# load packages
library(lubridate)
library(dplyr)
library(tidyr)
# set working directory to lab server 
# files should also be posted at Arctic Data Ctr
setwd('L:/data_repo/field_data/viperData/sensor/')
# from Mac
setwd('/Volumes/data/data_repo/field_data/viperData/sensor/')
rm(list=ls())

############### read and aggregate net radiometer data ###############
# note this includes more data than are posted to Arctic Data Center
nr <- read.csv('campbell/radiation/netR.csv',header = T)

# set negative shortwave values to NA
nr$SR01Up_Avg[which(nr$SR01Up_Avg<0)] <- NA
nr$SR01Dn_Avg[which(nr$SR01Dn_Avg<0)] <- NA

# calculate albedo
nr$alb <- nr$SR01Dn_Avg/nr$SR01Up_Avg

# filter erronious albedo values at low radiation
nr$alb[which(nr$alb>1)] <- NA

# calculate surface temp
nr$Tsurf <- ((nr$IR01DnCo_Avg/(5.67*10^-8))^0.25)-273.15

# aggregate to daily - only calculate the mean if there are more than 36 half-hourly values
nr.day <- nr %>%
  group_by(year,doy,site,loc,sensorZ) %>%
  summarise(SRup = ifelse(n()>36,mean(SR01Up_Avg, na.rm = T),NA),
            SRdn = ifelse(n()>36,mean(SR01Dn_Avg, na.rm = T),NA),
            IRup = ifelse(n()>36,mean(IR01UpCo_Avg, na.rm = T),NA),
            IRdn = ifelse(n()>36,mean(IR01DnCo_Avg, na.rm = T),NA),
            Tsrf = ifelse(n()>36,mean(Tsurf, na.rm = T),NA))

# get albedo values from local noon 
alb <- nr %>%
  filter(hour==12.0)

# join the albedo data to the daily data set
nr.day <- full_join(nr.day,alb[,c(1,2,10:13)])

# remove albedo dataframe
rm(alb)

# crerate a wide data set for looking at differences between sites
nr.dh1 <- nr.day %>%
  filter(site=="hd", sensorZ==100) 
colnames(nr.dh1)[6:11] <- paste(colnames(nr.day)[6:11],"H1", sep="")
nr.dh1 <- nr.dh1[,-c(3:5)]  
  
nr.dl1 <- nr.day %>%
  filter(site=="ld", sensorZ==100) 
colnames(nr.dl1)[6:11] <- paste(colnames(nr.day)[6:11],"L1", sep="")
nr.dl1 <- nr.dl1[,-c(3:5)]  

nr.dh8 <- nr.day %>%
  filter(site=="hd", sensorZ==800) 
colnames(nr.dh8)[6:11] <- paste(colnames(nr.day)[6:11],"H8", sep="")
nr.dh8 <- nr.dh8[,-c(3:5)]  

nr.dl8 <- nr.day %>%
  filter(site=="ld", sensorZ==800) 
colnames(nr.dl8)[6:11] <- paste(colnames(nr.day)[6:11],"L8", sep="")
nr.dl8 <- nr.dl8[,-c(3:5)]  

# merge all data together
all.day <- merge(nr.dh8,nr.dl8, all = T)
all.day <- merge(all.day, nr.dh1, all = T)
all.day <- merge(all.day, nr.dl1, all = T)

#remove unnecessary data frames
rm(nr.dh1,nr.dh8,nr.dl1,nr.dl8)
############### read and aggregate soil heat flux data ###############
hf <- read.csv('campbell/heatflux/heatflux.csv', header = T)

# aggregate to daily, by sensor, for days with at least 36 half hourly obs
hf.d <- hf %>%
  group_by(year,doy,site,sensorID) %>%
  summarise(shf = ifelse(n()>36,mean(shf, na.rm = T),NA))

# aggregate to daily by site
hf.day <- hf.d %>%
  group_by(year,doy,site) %>%
  summarise(shf = mean(shf, na.rm = T),
            sd = sd(shf,na.rm=T)) # for some reason standard deviation isn't working

# remove intermediate hf.d dataframe
rm(hf.d)

# make a wide copy to add to the big data frame
hfh <- hf.day %>%
  filter(site == "hd")
hfh <- hfh[,-3]
colnames(hfh)[3:4] <- paste(colnames(hfh)[3:4],"H", sep = "")

hfl <- hf.day %>%
  filter(site == "ld")
hfl <- hfl[,-3]
colnames(hfl)[3:4] <- paste(colnames(hfl)[3:4],"L", sep = "")

all.day <- merge(all.day, hfh, all = T)
all.day <- merge(all.day, hfl, all = T)

rm(hfh, hfl)

############### read and aggregate canopy ndvi data ###############
# note that this relies on 
# red band - down facing sensor with field stop
dr <- read.csv("decagon/ndvi/630nm.SRSnr.csv",header = T) %>%
  filter(sensorZ == 700)
# red band - up facing sensor with hemispherical fov
ur <- read.csv("decagon/ndvi/630nm.SRSni.csv", header = T) %>%
  filter(sensorZ == 700)
# nir band - down facing sensor with field stop
dn <- read.csv("decagon/ndvi/800nm.SRSnr.csv",header = T) %>%
  filter(sensorZ == 700)
# nir band - up facing sensor with hemispherical fov
un <- read.csv("decagon/ndvi/800nm.SRSni.csv", header = T) %>%
  filter(sensorZ == 700)
# alpha 
# a <- read.csv("decagon/ndvi/alpha.SRSni.csv", header = T) %>%
#   filter(sensorZ == 700)

# join the down facing r and nir data, and omit extraneous columns
d <- full_join(dr[,c(1:5,8:10,12)],dn[,c(1:5,8:10,12)])
u <- full_join(ur[,c(1:5,8:10,12)],un[,c(1:5,8:10,12)])

# join by year, say hour - there is 1 upward facing sensor & 4 down
rs <- full_join(d,u,by = c("doy","year","hour"))
# correct ndvi for low-density with alpha values from high-density
# see for http://library.metergroup.com/Manuals/14597_SRS_Web.pdf for info on method
rs$ndvi <- ((rs$X800nm.SRSnr/rs$X800nm.SRSni)-(rs$X630nm.SRSnr/rs$X630nm.SRSni))/
           ((rs$X800nm.SRSnr/rs$X800nm.SRSni)+(rs$X630nm.SRSnr/rs$X630nm.SRSni))

rs.day <- rs %>%
  filter(hour==12) %>% 
  group_by(year, doy,site.x) %>%
  summarise(ndvi = mean(ndvi, na.rm = T)) %>%
  rename(site = site.x)
  
rsh <- rs.day %>%
  filter(site == 'hd') %>%
  rename(ndvi.H = ndvi)

rsl <- rs.day %>%
  filter(site == 'ld') %>%
  rename(ndvi.L = ndvi)

# join to the mondo data frame
all.day <- left_join(all.day,rsh[,c(1:2,4)], )
all.day <- left_join(all.day,rsl[,c(1:2,4)])

# remove unecessary data
rm(d,dr,dn,u, ur, un, rs, rsh, rsl)
############### read and aggregate met & soil data ###############
# appogee radiometric surface temp
Tsrf <- read.csv("decagon/met/CTargetTemp.SI411.csv", header = T)
# soil temperature from Decagon GS-3 and 5TM sensors, respectively
ts1 <- read.csv("decagon/soil/tempS.GS3.csv", header = T)
ts2 <- read.csv("decagon/soil/tempS.5TM.csv", header = T)
# soil moisture from Decagon GS-3 and 5TM sensors, respectively
sm1 <- read.csv("decagon/soil/vwc.GS3.csv", header = T)
sm2 <- read.csv("decagon/soil/vwc.5Tm.csv", header = T)

####################################################################################################
# subset by site/canopy level

################################################
# make some plots - set working dir to EGU pres
setwd("/Volumes/GoogleDrive/My Drive/Documents/research/presentations/annual_meetings/EGU")
# timeseries of soil heat flux
pdf(file="soil_heat_flux_ts.pdf",8,5)
plot(hf.day.hd$date,hf.day.hd$shf,col="red",type = "p",
     ylim = c(-5,22),xlab="",
     ylab = expression(paste("G (W ",m^-2,")",sep="")))
points(hf.day.ld$date,hf.day.ld$shf,pch=3)
abline(h=0,lty = "dashed")
dev.off()

# plot radiometer data
plot(nr.day.hd800$date,nr.day.hd800$SRup,type="p",
     ylim=c(0,410),col="red",xlab="",
     ylab = expression(paste("SW (W ",m^-2,")",sep="")))
#points(nr.day.hd100$date,nr.day.hd100$SRup, col="red",pch=3)
points(nr.day.ld800$date,nr.day.ld800$SRup)

plot(nr.day.hd100$date,nr.day.hd100$SRup,type="p",
     ylim=c(0,410),col="red",pch=3,xlab="",
     ylab = expression(paste("SW (W ",m^-2,")",sep="")))
points(nr.day.ld100$date,nr.day.ld100$SRup,col="blue")

# plot albedo
plot(nr.day.hd800$date,nr.day.hd800$SRdn/nr.day.hd800$SRup,type="p",
     ylim=c(0,1),col="red",xlab="",
     ylab = "Albedo")
#points(nr.day.hd100$date,nr.day.hd100$SRup, col="red",pch=3)
points(nr.day.ld800$date,nr.day.ld800$SRdn/nr.day.ld800$SRup)
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
pdf(file="soil_heat_flux_cum.pdf",5,5)
plot(hf.day.ld17$doy,hf.day.ld17$cs*0.0036,type="l",lwd=2,
     ylab = expression(paste("Cumulative G (MJ",m^-2,")",sep="")),xlab="")
lines(hf.day.hd17$doy,hf.day.hd17$cs*0.0036,col="red",lwd=2)
lines(hf.day.hd16$doy,hf.day.hd16$cs*0.0036,col="red",lty="dashed",lwd=2)
lines(hf.day.ld16$doy,hf.day.ld16$cs*0.0036,lty="dashed",lwd=2)
legend("topleft",c("LD 2017", "HD 2017", "LD 2016", "HD 2016"),
       lwd=2, lt = c("solid", "solid","dashed", "dashed"),
       col=c("black","red"),bty="n")
dev.off()
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