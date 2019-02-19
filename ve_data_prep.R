#################################
#
# collate energy balance and  
# associated env. data for   
# high- and low-density larch
# stands for VIPER project
#
# MML 06/05/17
#################################

## notes ##
# this script relies on master files concatenated from multiple logger downloads
# prior to processing the following steps are performed manually: 
# 1) dates in some files are adjusted to rectify timezone issues
# 2) date elements extracted in excel due to inconsistencies in formating
# 3) various NA, No Data, and Inf data flags are removed
# 4) column headers are updated manualy - perhaps update in CS code for future

rm(list=ls())
require('lubridate')
require('plyr')

# make sequence of continuous date/times to help merge and check for missingness
ts <- as.data.frame(seq(ISOdate(2015,6,1),ISOdate(2017,9,1),by="min"))
names(ts) <- "timestamp"
ts$year <- year(ts$timestamp) 
ts$month <- month(ts$timestamp)
ts$day <- day(ts$timestamp)
ts$hour <- hour(ts$timestamp)
ts$min <- minute(ts$timestamp)
ts$jday <- yday(ts$timestamp)
# extract half hourly values to match data
r <- which(ts$min==0 | ts$min==30)
ts <- ts[r,]
rm(r)
#################################
# START WITH NET RADIOMETER DATA
#################################
## read in data
setwd('/Users/mloranty/Google Drive/Documents/Research/NSF_VIPER_2015-18/viper_energy/CR1000/energy_aggregated_ml/')
nr.dat <- list()
nr.dat[[1]] <- read.csv('hd_nr_TableNR_master.csv',skip=3,header=T)
nr.dat[[2]] <- read.csv('hd_tdp_TableNR_master.csv',skip=3,header=T)
nr.dat[[3]] <- read.csv('ld_nr_TableNR_master.csv',skip=3,header=T)
nr.dat[[4]] <- read.csv('ld_tdp_TableNR_master.csv',skip=3,header=T)

names(nr.dat) <- c("hd9","hd1","ld9","ld1")

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
  # filter eronious values
  r <- which(nr.dat[[i]]$alb > 1)
  nr.dat[[i]]$alb[r] <- 0
  # calculate correct net LW
  nr.dat[[i]]$LW.net <- nr.dat[[i]]$LW.in.corr-nr.dat[[i]]$LW.out.corr
  # calculate correct net RAD
  nr.dat[[i]]$net.RAD <- nr.dat[[i]]$SW.net+nr.dat[[i]]$LW.net
  #calculate surface temps
  nr.dat[[i]]$Tsurf <- ((nr.dat[[i]]$LW.out.corr/(5.67*10^-8))^0.25)-273.15
  
  # make an excessively large data frame 
  # note join keeps the order of the first argument, so ts is first
  nr.dat[[i]] <- join(ts,nr.dat[[i]],type="full",
                      by=c("year","month","day","hour","min"))
  # create a formatted date variable
  nr.dat[[i]]$date <- ymd_hm(paste(nr.dat[[i]]$year,nr.dat[[i]]$month,
                                   nr.dat[[i]]$day,nr.dat[[i]]$hour,
                                   nr.dat[[i]]$min))
  nr.dat[[i]]$jday <- yday(nr.dat[[i]]$date)
  rm(r)
}

# aggregate to daily 
nr.day <- list()

for(i in 1:length(nr.dat))
{
  #aggregate to daily
  nr.day[[i]] <- ddply(nr.dat[[i]],.(year,month,day),summarize,
                      mean(SW.in),
                      mean(SW.out),
                      mean(LW.in.corr),
                      mean(LW.out.corr),
                      mean(Tsurf))
  names(nr.day[[i]])[4:8] <- c("SW.in","SW.out","LW.in","LW.out","Tsurf")
  #create date vars
  nr.day[[i]]$date <- ymd(paste(nr.day[[i]]$year,nr.day[[i]]$month,nr.day[[i]]$day))
  nr.day[[i]]$jday <- yday(nr.day[[i]]$date)
  # get daily albedo value as noon local time
  rec <- which(nr.dat[[i]]$hour==12 & nr.dat[[i]]$min==0)
  n <- match(c("date","jday","alb"),colnames(nr.dat[[i]]))
  alb <- nr.dat[[i]][rec,n]
  nr.day[[i]] <- join(nr.day[[i]],alb)
  rm(alb)
}  
names(nr.day) <- names(nr.dat)
# omit unwanted vars and create unique colnames for merge
dat.vars <- c("date","year","month","hour","min","jday","SW.in","SW.out","alb","LW.in.corr","LW.out.corr","Tsurf")
for(i in 1:length(nr.dat))
{
  c <- match(dat.vars,colnames(nr.dat[[i]]))
  nr.dat[[i]] <- nr.dat[[i]][,c]
}
colnames(nr.dat[[1]])[7:12] <- c("SW.in.9m","SW.out.9m","alb.9m","LW.in.9m","LW.out.9m","Tsurf.9m")
colnames(nr.dat[[2]])[7:12] <- c("SW.in.1m","SW.out.1m","alb.1m","LW.in.1m","LW.out.1m","Tsurf.1m")
colnames(nr.dat[[3]])[7:12] <- c("SW.in.9m","SW.out.9m","alb.9m","LW.in.9m","LW.out.9m","Tsurf.9m")
colnames(nr.dat[[4]])[7:12] <- c("SW.in.1m","SW.out.1m","alb.1m","LW.in.1m","LW.out.1m","Tsurf.1m")

hd.all.hh <- join(nr.dat[[1]],nr.dat[[2]])
ld.all.hh <- join(nr.dat[[3]],nr.dat[[4]])

colnames(nr.day[[1]])[c(4:8,11)] <- c("SW.in.9m","SW.out.9m","LW.in.9m","LW.out.9m","Tsurf.9m","alb.9m")
colnames(nr.day[[2]])[c(4:8,11)] <- c("SW.in.1m","SW.out.1m","LW.in.1m","LW.out.1m","Tsurf.1m","alb.1m")
colnames(nr.day[[3]])[c(4:8,11)] <- c("SW.in.9m","SW.out.9m","LW.in.9m","LW.out.9m","Tsurf.9m","alb.9m")
colnames(nr.day[[4]])[c(4:8,11)] <- c("SW.in.1m","SW.out.1m","LW.in.1m","LW.out.1m","Tsurf.1m","alb.1m")

hd.all.dy <- join(nr.day[[1]],nr.day[[2]])
ld.all.dy <- join(nr.day[[3]],nr.day[[4]])
rm(nr.dat,nr.day)
######################################
#
#   SOIL HEAT FLUX DATA 
#
######################################
## read in data
hf.dat <- list()
#read & join high density data
hf.dat[[1]] <- read.csv('hd_nr_TableHF_master.csv',skip=3,header=T)
hf.dat[[2]] <- read.csv('hd_tdp_soil_hf_master.csv',skip=3,header=T)
hf.dat[[1]] <- join(hf.dat[[1]][,4:10],hf.dat[[2]][,3:13],
                    by=c("year","month","day","hour","min"))

#read & join low density data
hf.dat[[2]] <- read.csv('ld_nr_TableHF_master.csv',skip=3,header=T)
hf.dat[[3]] <- read.csv('ld_tdp_soil_hf_master.csv',skip=3,header=T)
hf.dat[[2]] <- join(hf.dat[[2]][,3:9],hf.dat[[3]][,3:13])

hf.dat[[3]] <- NULL
names(hf.dat) <- c("hd","ld")

for(i in 1:length(hf.dat))
{
  hf.dat[[i]] <- join(ts,hf.dat[[i]],type="full",
                      by=c("year","month","day","hour","min"))
}
names(hf.dat) <- c("hd","ld")
## aggregate to daily 
hf.day <- list()

for(i in 1:length(hf.dat))
{
  #aggregate to daily (not including wonky thermocouple data for now)
  hf.day[[i]] <- ddply(hf.dat[[i]],.(year,month,day),summarize,
                       mean(shf.n1),
                       mean(shf.n2),
                       mean(shf.t1),
                       mean(shf.t2),
                       mean(shf.t3),
                       mean(shf.t4))
  names(hf.day[[i]])[4:9] <- c("shf.n1","shf.n2","shf.t1","shf.t2","shf.t3","shf.t4")
  #create date vars
  hf.day[[i]]$date <- ymd(paste(hf.day[[i]]$year,hf.day[[i]]$month,hf.day[[i]]$day))
  hf.day[[i]]$jday <- yday(hf.day[[i]]$date)
}  
names(hf.day) <- c("hd","ld")

# join to site level data frames
hd.all.hh <- join(hd.all.hh,hf.dat[[1]][,2:15])
ld.all.hh <- join(ld.all.hh,hf.dat[[2]][,2:15])

hd.all.dy <- join(hd.all.dy,hf.day[[1]])
ld.all.dy <- join(ld.all.dy,hf.day[[2]])

######################################
#
#   CANOPY & MET DATA 
#   
######################################
setwd('/Users/mloranty/Google Drive/Documents/Research/NSF_VIPER_2015-18/viper_energy/Decagon/ml_aggregated/')
# this will be a bit messier because the loggers are mixed a bit differently
# start with canopy data - will need a little work because ndvi sensors were shifted
# the ndvi sensors were moved on 6/28/16; they were moved ~30m within the stand and attached to a stronger mount
can.dat <- list()
can.dat[[1]] <- read.csv("DAV_canopy_2015_master.csv",header=T)
can.dat[[2]] <- read.csv("DAV_canopy_master.csv",header=T)
can.dat[[3]] <- read.csv("LDF2_canopy_2015_master.csv")
can.dat[[4]] <- read.csv("LDF2_canopy_master.csv",header=T)

# we have only one upward/ref sensor for both sites
# so need to append ref sensor and correct ndvi data for one site in each year
up.15 <- can.dat[[3]][,10:17]
up.16 <- can.dat[[2]][,c(3:5,18:22)]

can.dat[[1]] <- join(can.dat[[1]],up.15)
can.dat[[4]] <- join(can.dat[[4]],up.16)
rm(up.15,up.16)
r <- c(1,4)
for(i in 1:length(r))
 {
   can.dat[[i]]$ndvi.1 <- ((can.dat[[i]]$up.alpha*can.dat[[i]]$down1.800-can.dat[[i]]$down1.630)/
                             (can.dat[[i]]$up.alpha*can.dat[[i]]$down1.800+can.dat[[i]]$down1.630))
   
   can.dat[[i]]$ndvi.2 <- ((can.dat[[i]]$up.alpha*can.dat[[i]]$down2.800-can.dat[[i]]$down2.630)/
                             (can.dat[[i]]$up.alpha*can.dat[[i]]$down2.800+can.dat[[i]]$down2.630))
 }

# now concatenate the two data sets
# note the the sensors were moved on 6/27/16
# there will be a gap in the data here that reflects this
can.dat[[1]] <- rbind.fill(can.dat[[1]],can.dat[[2]])
can.dat[[2]] <- rbind.fill(can.dat[[3]],can.dat[[4]])
can.dat[[4]] <- NULL
can.dat[[3]] <- NULL
names(can.dat) <- c("hd","ld")
# now exapnd to the common date range
# note that the 2015 data is hourly, and only includes ndvi and Tsurf
# beginning in 2016 met data were included and data were logged half hourly
# we will use noon ndvi values, and we are going to filter out erroneous ndvi here
for(i in 1:length(can.dat))
{
  ifelse(can.dat[[i]]$ndvi.1<0|can.dat[[i]]$ndvi.1>1,NA,can.dat[[i]]$ndvi.1)
  ifelse(can.dat[[i]]$ndvi.2<0|can.dat[[i]]$ndvi.2>1,NA,can.dat[[i]]$ndvi.2)
  can.dat[[i]] <- join(ts,can.dat[[i]],type="full",
                      by=c("year","month","day","hour","min"))
}
names(can.dat) <- c("hd","ld")
# now aggregate to daily
# START WITH HIGH DENSITY
can.day <- list()

can.day[[1]] <- ddply(can.dat[[1]],.(year,month,day),summarize,
                       mean(t.air),
                       mean(rh),
                       mean(pressure),
                       mean(t.surf))

rec <- which(can.dat[[1]]$hour==12 & can.dat[[1]]$min==0)
ndvi <- can.dat[[1]][rec,c(2:4,12,15)]
can.day[[1]] <- join(can.day[[1]],ndvi)
rm(ndvi)
# assign col names and create date vars
names(can.day[[1]])[4:9] <- c("t.air","rh","pressure","t.surf","ndvi.1","ndvi.2")
# 
# NOW FOR LOW DENSITY - DATA WILL DIFFER SLIGHTLY (INCLUDES LWS)
#
can.day[[2]] <- ddply(can.dat[[2]],.(year,month,day),summarize,
                      mean(t.air),
                      mean(rh),
                      mean(pressure),
                      mean(t.surf),
                      sum(min.wet.450),
                      sum(min.wet.460))

rec <- which(can.dat[[2]]$hour==12 & can.dat[[2]]$min==0)
ndvi <- can.dat[[2]][rec,c(2:4,12,15)]
can.day[[2]] <- join(can.day[[2]],ndvi)
rm(ndvi)
# assign col names and create date vars
names(can.day[[2]])[4:11] <- c("t.air","rh","pressure","t.surf",
                              "min.wet.450","min.wet.460","ndvi.1","ndvi.2")

for(i in 1:length(can.day))
{
  can.day[[i]]$date <- ymd(paste(can.day[[i]]$year,can.day[[i]]$month,can.day[[i]]$day))
  can.day[[i]]$jday <- yday(can.day[[i]]$date)
}
names(can.day) <- c("hd","ld")
## now aggregate to large data frames
colnames(can.dat[[1]])[8:23] <- paste(colnames(can.dat[[1]])[8:23],"7m",sep=".")
hd.all.hh <- join(hd.all.hh,can.dat[[1]][,c(2:8,12,15,20:22)])

colnames(can.dat[[2]])[8:22] <- paste(colnames(can.dat[[2]])[8:22],"7m",sep=".")
ld.all.hh <- join(ld.all.hh,can.dat[[2]][,c(2:8,12,15,20:24)])

colnames(can.day[[1]])[4:9] <- paste(colnames(can.day[[1]])[4:9],"7m",sep=".")
hd.all.dy <- join(hd.all.dy,can.day[[1]])

colnames(can.day[[2]])[4:11] <- paste(colnames(can.day[[2]])[4:11],"7m",sep=".")
ld.all.dy <- join(ld.all.dy,can.day[[2]])
######################################
#
#   UNDERSTORY NDVI 
#   
######################################
# start with canopy data - will need a little work because ndvi sensors were shifted
# the ndvi sensors were moved on 6/27/16; they were moved ~30m within the stand and attached to a stronger mount
# the MDF4 (medium density) sensors were not moved. 
us.dat <- list()
us.dat[[1]] <- read.csv("DAV_understory_veg_master.csv",header=T)
us.dat[[2]] <- read.csv("DAV_understory_veg_2015_master.csv",header=T)
us.dat[[3]] <- read.csv("LDF2_understory_veg_master.csv",header=T)
us.dat[[4]] <- read.csv("LDF2_understory_veg_2015_master.csv",header=T)

us.dat[[1]] <- rbind.fill(us.dat[[2]],us.dat[[1]])
us.dat[[1]]$timestamp <- NULL
# note that these data switch from half hourly to hourly around 6/28/16 
# when the sensors were moved
us.dat[[2]] <- rbind.fill(us.dat[[4]],us.dat[[3]])
us.dat[[2]]$timestamp <- NULL
us.dat[[3]] <- read.csv("MDF4_understory_master.csv")
us.dat[[3]]$timestamp <- NULL
us.dat[[4]] <- NULL

## get rid of erroneous values and join to common date range
for(i in 1:length(us.dat))
{
  ifelse(us.dat[[i]]$ndvi.1<0|us.dat[[i]]$ndvi.1>1,NA,us.dat[[i]]$ndvi.1)
  ifelse(us.dat[[i]]$ndvi.2<0|us.dat[[i]]$ndvi.2>1,NA,us.dat[[i]]$ndvi.2)
  us.dat[[i]] <- join(ts,us.dat[[i]],type="full",
                       by=c("year","month","day","hour","min"))
}
names(us.dat) <- c("hd","ld","md")
# aggregate to daily
us.day <- list()
# high density
rec <- which(us.dat[[1]]$hour==12 & us.dat[[1]]$min==0)
us.day[[1]] <- us.dat[[1]][rec,c(2:5,11,14)]

# daily leaf wetness sums, just for this site
lws <- ddply(us.dat[[1]],.(year,month,day),summarize,
             sum(min.wet.450),
             sum(min.wet.460))
names(lws)[4:5] <- c("min.wet.450","min.wet.460")
us.day[[1]] <- join(us.day[[1]],lws)
rm(lws)

# low density
rec <- which(us.dat[[2]]$hour==12 & us.dat[[2]]$min==0)
us.day[[2]] <- us.dat[[2]][rec,c(2:5,11,14)]

# med density
rec <- which(us.dat[[3]]$hour==12 & us.dat[[3]]$min==0)
us.day[[3]] <- us.dat[[3]][rec,c(2:5,11,14)]
names(us.day) <- c("hd","ld","md")

# aggregate with mondo data frames
colnames(us.dat[[1]])[9:17] <- paste(colnames(us.dat[[1]])[9:17],"1m",sep=".")
hd.all.hh <- join(hd.all.hh,us.dat[[1]][,c(2:7,11,14,18:19)])

colnames(us.dat[[2]])[9:17] <- paste(colnames(us.dat[[2]])[9:17],"1m",sep=".")
ld.all.hh <- join(ld.all.hh,us.dat[[2]][,c(2:7,11,14)])

colnames(us.day[[1]])[5:6] <- paste(colnames(us.day[[1]])[5:6],"1m",sep=".")
hd.all.dy <- join(hd.all.dy,us.day[[1]])

colnames(us.day[[2]])[5:6] <- paste(colnames(us.day[[2]])[5:6],"1m",sep=".")
ld.all.dy <- join(ld.all.dy,us.day[[2]])
######################################
#
#   SOIL MOISTURE 
#   
######################################
# need to get sensor depths from Heather
sm.dat <- list()
sm.dat[[1]] <- read.csv("DAV_soil_moisture_master.csv",header=T)
sm.dat[[2]] <- read.csv("LDF2_soil_moisture_master.csv",header=T)
names(sm.dat) <- c("hd","ld")
# remove data for sensor 3 at low density before 7/28/16
r <- which(sm.dat[[2]]$year==2016 & sm.dat[[2]]$month==7 & sm.dat[[2]]$day<28)
sm.dat[[2]]$sm3[r] <- NA
sm.dat[[2]]$tsoil3[r] <- NA
sm.dat[[2]]$cond3[r] <- NA

for(i in 1:length(sm.dat))
{
  sm.dat[[i]] <- join(ts,sm.dat[[i]],type="full",
                       by=c("year","month","day","hour","min"))
}

# now aggregate this to daily timestep
sm.day <- list()
for(i in 1:length(sm.dat))
{
  sm.day[[i]] <- ddply(sm.dat[[i]],.(year,month,day),summarize,
                       mean(sm1),
                       mean(tsoil1),
                       mean(cond1),
                       mean(sm2),
                       mean(tsoil2),
                       mean(cond2),
                       mean(sm3),
                       mean(tsoil3),
                       mean(cond3))
  names(sm.day[[i]])[4:12] <- c("sm1","tsoil1","cond1","sm2","tsoil2","cond2","sm3","tsoil3","cond3")
  #create date vars
  sm.day[[i]]$date <- ymd(paste(sm.day[[i]]$year,sm.day[[i]]$month,sm.day[[i]]$day))
  sm.day[[i]]$jday <- yday(sm.day[[i]]$date)
}
names(sm.day) <- c("hd","ld")

# aggregate with big data from - leave column names for now
hd.all.hh <- join(hd.all.hh,sm.dat[[1]][,c(2:7,9:17)])
ld.all.hh <- join(ld.all.hh,sm.dat[[2]][,c(2:7,9:17)])

hd.all.dy <- join(hd.all.dy,sm.day[[1]][,1:12])
ld.all.dy <- join(ld.all.dy,sm.day[[2]][,1:12])

## NOT JOINING UNDERSTORY MET FOR NOW ##
# write to csv
setwd("/Users/mloranty/Google Drive/Documents/Research/NSF_VIPER_2015-18/viper_energy/")
write.csv(hd.all.hh,file="HIGH_DENSITY_ALL_VARS_30MIN.csv",row.names = F)
write.csv(ld.all.hh,file="LOW_DENSITY_ALL_VARS_30MIN.csv",row.names = F)
write.csv(hd.all.dy,file="HIGH_DENSITY_ALL_VARS_DAILY.csv",row.names = F)
write.csv(ld.all.dy,file="LOW_DENSITY_ALL_VARS_DAILY.csv",row.names = F)
######################################
#
#   Understory MET 
#   this is part of a boreal 
#   project data set that could 
#   collected at the sites
#
######################################
setwd("/Users/mloranty/Google Drive/Documents/Research/NSFBorealFireCherskii_2014-17/Loranty_data/DGF_understory_met_ML/")

met.dat <- list()
met.dat[[1]] <- read.csv("aggregated_records/DAV_us_met_master.csv",header=T)
met.dat[[2]] <- read.csv("aggregated_records/LDF2_us_met_master.csv",header=T)
names(met.dat) <- c("hd","ld")
# join with common date range
# note this omits 2014 data preceding ViPER energy project
# this also omits the excel timestamps - wonky formatting issues
# joining by year, month, day, hour, min is better
for(i in 1:length(met.dat))
{
  met.dat[[i]] <- join(ts,met.dat[[i]][,3:18],type="left")  
  met.dat[[i]]$date <- ymd(paste(met.dat[[i]]$year,met.dat[[i]]$month,met.dat[[i]]$day))
  met.dat[[i]]$jday <- yday(met.dat[[i]]$date)
}

# aggregate this to daily
met.day <- list()
for(i in 1:length(met.dat))
{
  met.day[[i]] <- aggregate(met.dat[[i]][,8:18],by=list(met.dat[[i]]$date),FUN=mean)
  met.day[[i]]$date <- ymd(met.day[[i]]$Group.1)
  met.day[[i]]$year <- year(met.day[[i]]$date)
  met.day[[i]]$month <- month(met.day[[i]]$date)
  met.day[[i]]$day <- day(met.day[[i]]$date)
  met.day[[i]]$jday <- yday(met.day[[i]]$date)
}
names(met.day) <- c("hd","ld")

#################################################################################################
#################################################################################################
# gee dang, this is a lot of data, perhaps now put it together and write some csv files
# this stuff will be the basis for many analyses. 
 # stopping here
#save.image("~/Documents/GitHub/viper_energy/ve_data_prep.RData")




# NO JOIN FOR NOW
# ## apend site id to column names for join
# site.id <- c("hd9","hd1","ld9","ld1")
# n <- names(nr.dat[[1]])
# for(i in 1:length(nr.dat))
# {
#   n[7:24] <- paste(site.id[i],names(nr.dat[[i]][7:24]),sep=".")
#   names(nr.dat[[i]]) <- n
# }
# 
# ## join may of this into a bigg happy data frame
# nr.all <- join(nr.dat[[1]],nr.dat[[2]],by="date",type="inner")
# nr.all <- join(nr.all,nr.dat[[3]],by="date",type="inner")
# nr.all <- join(nr.all,nr.dat[[4]],by="date",type="inner")



