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
#setwd('/Volumes/data/data_repo/field_data/viperData/sensor/')
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
all.day <- left_join(all.day,rsh[,c(1:2,4)])
all.day <- left_join(all.day,rsl[,c(1:2,4)])

# remove unecessary data
rm(d,dr,dn,u, ur, un, rs, rsh, rsl)

############### read and aggregate met & soil data ###############
# just getting AGU data for now
# appogee radiometric surface temp
#Tsrf <- read.csv("decagon/met/CTargetTemp.SI411.csv", header = T)
# soil temperature from Decagon GS-3 and 5TM sensors, respectively
ts1 <- read.csv("decagon/soil/tempS.GS3.csv", header = T)
ta <- read.csv('decagon/met/TempC.VP4.csv', header = T)

#ts2 <- read.csv("decagon/soil/tempS.5TM.csv", header = T)
# soil moisture from Decagon GS-3 and 5TM sensors, respectively
#sm1 <- read.csv("decagon/soil/vwc.GS3.csv", header = T)
#sm2 <- read.csv("decagon/soil/vwc.5Tm.csv", header = T)

# aggregate daily air temp and join to wide data frame
ta.day <- ta %>%
  filter(sensorLoc == 'overstory') %>%
  group_by(year, doy, site) %>%
  summarise(Tair = mean(TempC.VP4, na.rm = T))
  
ta.hd <- ta.day %>%
  filter(site == 'hd') %>%
  rename(Tair.H = Tair)

ta.ld <- ta.day %>%
  filter(site == 'ld') %>%  
  rename(Tair.L = Tair)
  
all.day <- left_join(all.day,ta.hd[,c(1:2,4)])
all.day <- left_join(all.day,ta.ld[,c(1:2,4)])  

rm(ta.ld, ta.hd)  
  
# read airport precip and join to wide data frame
pr <- read.csv('airport/airport.csv', header = T)

all.day <- left_join(all.day,pr[,c(1:2,4)])

rm(pr)

# aggregate daily soil temp and join to wide data frame
ts.day <- ts1 %>%
  group_by(year, doy, site, sensorZ) %>%
  summarise(Tsoil = mean(tempS.GS3, na.rm = T))

ts.hd5 <- ts.day %>%
  filter(site == 'hd', sensorZ == 5) %>%
  rename(Tsoil.H5 = Tsoil)

ts.hd50 <- ts.day %>%
  filter(site == 'hd', sensorZ == 50) %>%
  rename(Tsoil.H50 = Tsoil)

ts.ld5 <- ts.day %>%
  filter(site == 'ld', sensorZ == 5) %>%
  rename(Tsoil.L5 = Tsoil)

ts.ld50 <- ts.day %>%
  filter(site == 'ld', sensorZ == 50) %>%
  rename(Tsoil.L50 = Tsoil)

all.day <- left_join(all.day,ts.hd5[,c(1:2,5)])
all.day <- left_join(all.day,ts.hd50[,c(1:2,5)])
all.day <- left_join(all.day,ts.ld5[,c(1:2,5)])
all.day <- left_join(all.day,ts.ld50[,c(1:2,5)]) 

rm(ts.ld5, ts.ld50, ts.hd5, ts.hd50)  

  

all.day$date <- strptime(paste(all.day$year, all.day$doy, sep = '-'),
                          format = '%Y-%j')


all.day$dy <- ifelse(leap_year(all.day$year),
                     all.day$year+(all.day$doy/366),
                     all.day$year+(all.day$doy/365))




