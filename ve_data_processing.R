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

# set working directory to lab server 
# files should also be posted at Arctic Data Ctr
setwd('L:/data_repo/field_data/viperData/sensor/')
# from Mac
setwd('/Volumes/data/data_repo/field_data/viperData/sensor/')
rm(list=ls())

############### read and aggregate net radiometer data ###############
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

# add date var
nr.day$date <- strptime(paste(nr.day$year,nr.day$doy,sep="_"),
                        format = "%Y_%j",tz="GMT")

# get albedo values from local noon 
alb <- nr %>%
  filter(hour==12.0)

# join the albedo data to the daily data set
nr.day <- full_join(nr.day,alb[,c(1,2,10:13)])

# remove albedo dataframe
rm(alb)

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

# add date var
hf.day$date <- strptime(paste(hf.day$year,hf.day$doy,sep="_"),
                        format = "%Y_%j",tz="GMT")

# remove intermediate hf.d dataframe
rm(hf.d)

############### read and aggregate canopy ndvi data ###############
# note that this relies on 
# red band - down facing sensor with field stop
rd <- read.csv("decagon/ndvi/630nm.SRSnr.csv",header = T)
# red band - up facing sensor with hemispherical fov
ru <- read.csv("decagon/ndvi/630nm.SRSni.csv", header = T)
# nir band - down facing sensor with field stop
nd <- read.csv("decagon/ndvi/800nm.SRSnr.csv",header = T)
# nir band - up facing sensor with hemispherical fov
nu <- read.csv("decagon/ndvi/800nm.SRSni.csv", header = T)
# alpha 
#a <- read.csv("decagon/ndvi/alpha.SRSni.csv", header = T)

# join the down facing r and nir data, and omit extraneous columns
dn <- full_join(rd[,c(1:5,8:10)],nd[,c(1:5,8:10)])
up <- full_join(ru[,c(1:5,8:10)],nu[,c(1:5,8:10)])
# correct ndvi for low-density with alpha values from high-density
# see for http://library.metergroup.com/Manuals/14597_SRS_Web.pdf for info on method



############### read and aggregate met & soil data ###############
# appogee radiometric surface temp
Tsrf <- read.csv("decagon/met/CTargetTemp.SI411.csv", header = T)
# soil temperature from Decagon GS-3 and 5TM sensors, respectively
ts1 <- read.csv("decagon/soil/tempS.GS3.csv", header = T)
ts2 <- read.csv("decagon/soil/tempS.5TM.csv", header = T)
# soil moisture from Decagon GS-3 and 5TM sensors, respectively
sm1 <- read.csv("decagon/soil/vwc.GS3.csv", header = T)
sm1 <- read.csv("decagon/soil/vwc.5Tm.csv", header = T)

####################################################################################################
# subset by site/canopy level
nr.day.hd100 <- nr.day %>%
  filter(site=="hd", sensorZ==100) 

nr.day.ld100 <- nr.day %>%
  filter(site=="ld", sensorZ==100) 

nr.day.hd800 <- nr.day %>%
  filter(site=="hd", sensorZ==800) 

nr.day.ld800 <- nr.day %>%
  filter(site=="ld", sensorZ==800) 
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