#################################
#
# Preliminary data check 
# spring 2017 radiation  
# data from VIPER sites
#
# MML 05/19/17
#################################

## notes ##
# prior to processing the following steps are performed: 
# 1) date elements extracted in excel due to inconsistencies in formating
# 2) various NA, No Data, and Inf data flags are removed
# 3) column headers are updated manualy - perhaps update in CS code for future

rm(list=ls())
library('lubridate')
library('dplyr')

setwd('/Users/mloranty/Google Drive/Documents/Research/NSF_VIPER_2015-18/viper_energy/field_data_raw_2017/spring_energy_prelim/')

## read in data
nr.dat <- list()
nr.dat[[1]] <- read.csv('low_9m_nr.csv',skip=3,header=T)
nr.dat[[2]] <- read.csv('low_1m_nr.csv',skip=3,header=T)
nr.dat[[3]] <- read.csv('high_9m_nr.csv',skip=3,header=T)
nr.dat[[4]] <- read.csv('high_1m_nr.csv',skip=3,header=T)

## create a copy of raw data for reference
nr.raw <- nr.dat 

## clean up shortwave and recalculate albedo
for(i in 1:length(nr.dat))
{
  # set negative SW values to zero
  r <- which(nr.dat[[i]]$SW.in < 0)
  nr.dat[[i]]$SW.in[r] <- 0
  r <- which(nr.dat[[i]]$SW.out < 0)
  nr.dat[[i]]$SW.out[r] <- 0
  # recalculate net SW
  nr.dat[[i]]$SW.net <- nr.dat[[i]]$SW.in-nr.dat[[i]]$SW.out
  # recalculate albedo
  nr.dat[[i]]$alb <- nr.dat[[i]]$SW.out/nr.dat[[i]]$SW.in
  # filter erronious values
  r <- which(nr.dat[[i]]$alb > 1)
  nr.dat[[i]]$alb[r] <- 0
  # calculate correct net LW
  nr.dat[[i]]$LW.net <- nr.dat[[i]]$LW.in.corr-nr.dat[[i]]$LW.out.corr
  # calculate correct net RAD
  nr.dat[[i]]$net.RAD <- nr.dat[[i]]$SW.net+nr.dat[[i]]$LW.net
  #calculate surface temps
  nr.dat[[i]]$Tsurf <- ((nr.dat[[i]]$LW.out.corr/(5.67*10^-8))^0.25)-273.15
  # create a formatted date variable
  nr.dat[[i]]$date <- ymd_hm(paste(nr.dat[[i]]$year,nr.dat[[i]]$month,
                                   nr.dat[[i]]$day,nr.dat[[i]]$hour,
                                   nr.dat[[i]]$min))
}


## apend site id to column names for join
site.id <- c("ld9","ld1","hd9","hd1")
n <- names(nr.dat[[1]])
for(i in 1:length(nr.dat))
{
  n[7:24] <- paste(site.id[i],names(nr.dat[[i]][7:24]),sep=".")
  names(nr.dat[[i]]) <- n
}

## join may of this into a bigg happy data frame
nr.all <- join(nr.dat[[1]],nr.dat[[2]],by="date",type="inner")
nr.all <- join(nr.all,nr.dat[[3]],by="date",type="inner")
nr.all <- join(nr.all,nr.dat[[4]],by="date",type="inner")
## calculate albedo for ld using SW.in from hd
nr.all$ld9.alb2 <- nr.all$ld9.SW.out/nr.all$hd9.SW.in
nr.all$ld9.alb3 <- nr.all$ld9.SW.out/nr.all$ld1.SW.in

## subset 10 days in May to look closely at diurnal patterns
nr.may <- subset(nr.all,month==5 & day<10)

#### MAKE SOME PLOTS ######
## plot data to assess patterns
par(mfcol=c(3,1),mar=c(0,5,3,2))
plot(nr.may$date,nr.may$ld1.alb,type="l")
lines(nr.may$date,nr.may$hd1.alb,col="red")
lines(nr.may$date,nr.may$hd9.alb,col="blue")
lines(nr.may$date,nr.may$ld9.alb3,col="orange")

# above canopy SW in
par(mfcol=c(3,1),mar=c(0,5,0,2),new=T)
plot(nr.may$date,nr.may$ld9.SW.in,type='l',xlab="",ylim=c(0,700),lwd=1.5,
     ylab=expression(paste("Downwelling SW (W ",m^-2,")",sep="")))
lines(nr.may$date,nr.may$hd9.SW.in,col='red',lwd=2.5)
lines(nr.may$date,nr.may$ld1.SW.in,col='blue',lwd=1.5)
lines(nr.may$date,nr.may$hd1.SW.in,col='orange',lwd=1.5)

# subcanopy SW in
par(mfcol=c(3,1),mar=c(3,5,0,2),new=T)
plot(nr.may$date,nr.may$ld1.SW.in,type='l',xlab="",ylim=c(0,700),lwd=1.5,
     ylab=expression(paste("Subcanopy SW (W ",m^-2,")",sep="")))
lines(nr.may$date,nr.may$hd1.SW.in,col='red',lwd=1.5)
legend("topleft",c("Low Density","High Density"),bty="n",lwd=2,col=c("black","red"),cex=0.8)
#######
## example to share with Markus & Nick ##
pdf("hd_larch_radiation.pdf",10,5)
# above canopy LW in
par(mfcol=c(2,1),mar=c(0,5,3,2))
plot(nr.may$date,nr.may$hd9.LW.in.corr,type='l',xlab="",ylim=c(150,400),lwd=1.5,
     xaxt="n",ylab=expression(paste(LW[IN], " (W ",m^-2,")",sep="")))
lines(nr.may$date,nr.may$hd1.LW.in.corr,col='red',lwd=1.5)
legend("topleft",c("8m","1m"),bty="n",lwd=2,col=c("black","red"),cex=0.8)

par(mfcol=c(2,1),mar=c(3,5,0,2),new=T)
plot(nr.may$date,nr.may$hd9.SW.in,type='l',xlab="",ylim=c(0,700),lwd=1.5,
     ylab=expression(paste(SW[IN], " (W ",m^-2,")",sep="")))
lines(nr.may$date,nr.may$hd1.SW.in,col='red',lwd=1.5)
dev.off()

# above canopy Tsurf
plot(nr.all$date,nr.all$ld9.Tsurf,type='l',xlab="",ylim=c(-25,20),lwd=1.5,
     ylab=expression(paste(T[surf]," (",C*degree,")",sep="")))
lines(nr.all$date,nr.all$hd9.Tsurf,col='red',lwd=1.5)
legend("topleft",c("Low Density","High Density"),bty="n",lwd=2,col=c("black","red"),cex=0.8)

# subcanopy LW in
plot(nr.may$date,nr.may$ld1.LW.in.corr,type='l',xlab="",ylim=c(150,400),lwd=1.5,
     ylab=expression(paste("Downwelling LW (W ",m^-2,")",sep="")))
lines(nr.may$date,nr.may$hd1.LW.in.corr,col='red',lwd=1.5)
legend("topleft",c("Low Density","High Density"),bty="n",lwd=2,col=c("black","red"),cex=0.8)

# subcanopy net LW in
plot(nr.may$date,nr.may$ld1.LW.net,type='l',xlab="",ylim=c(-100,50),lwd=1.5,
     ylab=expression(paste("Subcanopy Net LW (W ",m^-2,")",sep="")))
lines(nr.may$date,nr.may$hd1.LW.net,col='red',lwd=1.5)
legend("topleft",c("Low Density","High Density"),bty="n",lwd=2,col=c("black","red"),cex=0.8)



