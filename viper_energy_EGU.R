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
nr.day

# read soil heatflux data
hf <- read.csv('campbell/heatflux/heatflux.csv', header = T)

hf.day <- hf %>%
  group_by(year,doy,site, sensorID) %>%
  summarise(shf = mean(shf),
            sd = sd(shf,na.rm=T))
hf.day
# read
