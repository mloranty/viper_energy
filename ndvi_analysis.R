#################################
#
# analyze NDVI data from   
# high- and low-density larch
# stands for VIPER project
#
# MML 06/10/17
#################################

rm(list=ls())
setwd("/Users/mloranty/Google Drive/Documents/Research/NSF_VIPER_2015-18/viper_energy/")

hd <- read.csv(file="HIGH_DENSITY_ALL_VARS_DAILY.csv",header=T)
ld <- read.csv(file="LOW_DENSITY_ALL_VARS_DAILY.csv",header=T)

y16 <- which(hd$year==2016 & hd$month < 10 & hd$month > 4)
y17 <- which(hd$year==2017 & hd$month < 7 & hd$month > 4)

# plot all ndvi data for the 2016 growing season
plot(hd$jday[y16],hd$ndvi.1.7m[y16],
     type="l",col="blue",lwd=1.5,
     ylim=c(0,0.8),ylab="NDVI")
lines(hd$jday[y16],hd$ndvi.2.7m[y16],
      col="blue",lwd=1.5)
lines(hd$jday[y16],hd$ndvi.1.1m[y16],
      col="blue",lwd=1.5,lty="dashed")
lines(hd$jday[y16],hd$ndvi.2.1m[y16],
      col="blue",lwd=1.5,lty="dashed")

lines(ld$jday[y16],ld$ndvi.1.7m[y16],
      col="red",lwd=1.5)
lines(ld$jday[y16],ld$ndvi.2.7m[y16],
      col="red",lwd=1.5)
lines(ld$jday[y16],ld$ndvi.1.1m[y16],
      col="red",lwd=1.5,lty="dashed")
lines(ld$jday[y16],ld$ndvi.2.1m[y16],
      col="red",lwd=1.5,lty="dashed")


# plot all ndvi data for the 2017 growing season
plot(hd$jday[y17],hd$ndvi.1.7m[y17],
     type="l",col="blue",lwd=1.5,
     ylim=c(0,0.8),ylab="NDVI")
lines(hd$jday[y17],hd$ndvi.2.7m[y17],
      col="blue",lwd=1.5)
lines(hd$jday[y17],hd$ndvi.1.1m[y17],
      col="blue",lwd=1.5,lty="dashed")
lines(hd$jday[y17],hd$ndvi.2.1m[y17],
      col="blue",lwd=1.5,lty="dashed")

lines(ld$jday[y17],ld$ndvi.1.7m[y17],
      col="red",lwd=1.5)
lines(ld$jday[y17],ld$ndvi.2.7m[y17],
      col="red",lwd=1.5)
lines(ld$jday[y17],ld$ndvi.1.1m[y17],
      col="red",lwd=1.5,lty="dashed")
lines(ld$jday[y17],ld$ndvi.2.1m[y17],
      col="red",lwd=1.5,lty="dashed")



