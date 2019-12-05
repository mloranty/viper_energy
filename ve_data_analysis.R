#######################################
# analysis of VIPER energy data
# 
# for AGU 2019 & JGR-B manuscript
# M. Loranty
# 2 Dec 2019
#######################################

# this script relies on data pre-processing
# in ve_data_processing.R script
setwd('C:/Users/mloranty/Documents/GitHub/viper_energy/')

# this will eventually work
# first we should update all data for the Arctic Data Center
#source('ve_data_processing.R')

###################################################
# make a massive timeseries plot showing all data #
###################################################

#define plotting variables
acx <- 2.75
lw <- 3
hc <- 'blue'
lc <- 'red'

pdf("figures/canopy_radiation.pdf",28,12)
par(mfrow = c(3,1), mar=c(0,5,0,2), oma=c(6,2,2,2),cex.axis = acx)

# subcanopy SW and LW
plot(all.day$dy,all.day$IRupL1, type = 'l',lwd = lw,
     ylim = c(0,max(all.day$IRupH8,na.rm=T)),
     xaxt = 'n',yaxt = 'n', ylab = '',xlab = '', col = lc,)
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = F, tick = T, cex.axis = acx)

lines(all.day$dy, all.day$IRupH1,lwd = lw, col = hc )
lines(all.day$dy, all.day$SRupH1,lwd = lw, col = hc )
lines(all.day$dy, all.day$SRupL1,lwd = lw, col = lc )


# differences in incident downwelling subcanopy
plot(all.day$dy,(all.day$IRupL1+all.day$SRupL1)-(all.day$IRupH1+all.day$SRupH1), 
     type = 'l',lwd = lw,
#     ylim = c(0,max(all.day$IRupH8,na.rm=T)),
     yaxt = 'n', xaxt = 'n', ylab = '',xlab = '', col = 'black')
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = F, tick = T, cex.axis = acx)
abline(h=0,col = "gray", lty = 'dashed', lwd = 2)

# albedo
plot(all.day$dy,all.day$albL8, 
     type = 'l',lwd = lw,
     #     ylim = c(0,max(all.day$IRupH8,na.rm=T)),
     ylab = '',
     xaxt = 'n',yaxt = 'n',xlab = '', col = lc,)
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = T, tick = T, cex = acx, gap.axis = 2)

lines(all.day$dy, all.day$albH8,lwd = lw, col = hc )
lines(all.day$dy, all.day$ndvi.L,lwd = lw, col = lc, lty = 'dashed' )
lines(all.day$dy, all.day$ndvi.H,lwd = lw, col = hc, lty = 'dashed' )
dev.off()

#####################################################################
# Air/Soil T and Soil HEat Flux

plot(all.day$dy,all.day$Tair.L, 
     type = 'l',lwd = lw,
     #     ylim = c(0,max(all.day$IRupH8,na.rm=T)),
     xaxt = 'n',yaxt = 'n', ylab = '',xlab = '', col = lc,)
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = T, tick = T, cex.axis = acx)

lines(all.day$date, all.day$Tair.H,lwd = lw, col = hc )






# difference in ground heat flux
lines(all.day$date, all.day$shfL-all.day$shfL,lwd = lw, col = hc )

# ground heat flux
plot(all.day$date,all.day$shfH, type = 'l',lwd = lw,
     xaxt = 'n',yaxt = 'n', ylab = '',xlab = '', col = hc,)
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = F, tick = T, cex.axis = acx)

lines(all.day$date, all.day$shfL,lwd = lw, col = lc )

# air temp, soil temp, & precip
plot(all.day$date,all.day$Tair.L, 
     type = 'l',lwd = lw,
     #     ylim = c(0,max(all.day$IRupH8,na.rm=T)),
     xaxt = 'n',yaxt = 'n', ylab = '',xlab = '', col = lc,)
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = F, tick = T, cex.axis = acx)

lines(all.day$date, all.day$Tair.H,lwd = lw, col = hc )

# plot soil temps
plot(all.day$date,all.day$Tsoil.H5, 
     type = 'l',lwd = lw,
     #     ylim = c(0,max(all.day$IRupH8,na.rm=T)),
     xaxt = 'n',yaxt = 'n', ylab = '',xlab = '', col = hc,)
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = F, tick = T, cex.axis = acx)

lines(all.day$date, all.day$Tsoil.L5,lwd = lw, col = lc )
lines(all.day$date, all.day$Tsoil.L50,lwd = lw, col = lc, lty = 'dashed' )
lines(all.day$date, all.day$Tsoil.H50,lwd = lw, col = hc, lty = 'dashed' )

# plot albedo & NDVI


dev.off()

#################################################################################
############### don't need this now because of the wide data set ################
#################################################################################
# first examine net radiometer data for 2016
################################################
# subset the data
h8 <- nr.day %>%
  filter(year ==2016, site=="hd", sensorZ==800) 

h8 <- nr.day[which(nr.day$year==2016 & nr.day$site =='hd' & nr.day$sensorZ==800 ),]
l8 <- nr.day[which(nr.day$year==2016 & nr.day$site =='ld' & nr.day$sensorZ==800 ),]
h1 <- nr.day[which(nr.day$year==2016 & nr.day$site =='hd' & nr.day$sensorZ==100 ),]
l1 <- nr.day[which(nr.day$year==2016 & nr.day$site =='ld' & nr.day$sensorZ==100 ),]
lh <- hf.day[which(hf.day$year==2016 & hf.day$site =='ld'),]
hh <- hf.day[which(hf.day$year==2016 & hf.day$site =='hd'),]

# create a four panel plot
##############################
xl <- c(180,310)
acx <- 1.75
#par(mai=c(1,1,1,1))
#n <- layout(matrix(1:4),10,rep(2,4))
#layout.show(n)
pdf("figures/understory_energy_2016.pdf",10,10)
par(mfrow = c(3,1), mar=c(0,5,0,2), oma=c(6,2,2,2))
# plot LW
plot(h1$doy,h1$IRup,type = 'l',lwd = 2,
     xlim = xl,
     ylim = c(0,max(nr.day$IRup,na.rm=T)),
     xaxt = 'n',yaxt = 'n', ylab = '')
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = F, tick = T, cex.axis = acx)

lines(l1$doy, l1$IRup, col = 'red',lwd = 2)
#lines(h8$doy, h8$IRup, col = 'black', lty = 'dashed')
#lines(l8$doy,l8$IRup,col = 'red', lwd = 1)

#plot SW on same plot
lines(l1$doy,l1$SRup,
      col = 'red', lwd = 2)
lines(h1$doy,h1$SRup,
      col = 'black', lwd = 2)
legend('topright', bty = 'n', 'Subcanopy LW',cex = acx)
legend('bottomright', bty = 'n', 'Subcanopy SW',cex = acx)
# plot sub canopy SW
# plot(l1$doy,l1$SRup,
#      type = 'l', col = 'red', lwd = 2,lty = 'dashed',
#      xlim = xl,
#      xlab = 'Day of Year',ylab = '',
#      xaxt = 'n',yaxt = 'n')
# axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
# axis(side = 1, labels = F, tick = T, cex.axis = acx)
# legend('topright', bty = 'n', 'Subcanopy SW',cex = acx)
# 
# lines(h1$doy,h1$SRup,
#       col = 'black', lwd = 2, lty = 'dashed')

# total sub canopy radiation
#par(mfrow = c(4,1),mar=c(0,5,0,2),new=T)
plot(l1$doy,l1$SRup+l1$IRup,
     type = 'l', col = 'red', lwd = 2,
     xlim = xl,
     xlab = 'Day of Year',
     xaxt = 'n',yaxt = 'n',ylab = '')
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = F, tick = T, cex.axis = acx)
legend('topright', bty = 'n', 'Total Downwelling Subcanopy Radiation ',cex = acx)

lines(h1$doy,h1$SRup+h1$IRup,
      col = 'black', lwd = 2)

#plot subcanopy total
# plot sub canopy SW
#par(mfcol = c(4,1),mar=c(0,5,0,2),new=T)
# plot soil heat flux for 2016
plot(lh$doy,lh$shf,type = 'l',
     col='red', lwd=2,
     xlim = xl,
     xaxt = 'n',yaxt = 'n',
     ylab = '')
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = T, tick = T, cex.axis = acx)
legend('topright', bty = 'n', 'Soil Heat Flux ',cex = acx)

lines(hh$doy,hh$shf,col='black',lwd=2)
dev.off()


# now for 2017
################################################
# subset the data
h8 <- nr.day %>%
  filter(year ==2017, site=="hd", sensorZ==800) 

h8 <- nr.day[which(nr.day$year==2017 & nr.day$site =='hd' & nr.day$sensorZ==800 ),]
l8 <- nr.day[which(nr.day$year==2017 & nr.day$site =='ld' & nr.day$sensorZ==800 ),]
h1 <- nr.day[which(nr.day$year==2017 & nr.day$site =='hd' & nr.day$sensorZ==100 ),]
l1 <- nr.day[which(nr.day$year==2017 & nr.day$site =='ld' & nr.day$sensorZ==100 ),]
lh <- hf.day[which(hf.day$year==2017 & hf.day$site =='ld'),]
hh <- hf.day[which(hf.day$year==2017 & hf.day$site =='hd'),]

# create a four panel plot
##############################
xl <- c(120,310)
acx <- 1.75
#par(mai=c(1,1,1,1))
#n <- layout(matrix(1:4),10,rep(2,4))
#layout.show(n)
pdf("figures/understory_energy_2017.pdf",10,10)
par(mfrow = c(3,1), mar=c(0,5,0,2), oma=c(6,2,2,2))
# plot LW
plot(h1$doy,h1$IRup,type = 'l',lwd = 2,
     xlim = xl,
     ylim = c(0,max(nr.day$IRup,na.rm=T)),
     xaxt = 'n',yaxt = 'n', ylab = '')
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = F, tick = T, cex.axis = acx)

lines(l1$doy, l1$IRup, col = 'red',lwd = 2)
#lines(h8$doy, h8$IRup, col = 'black', lty = 'dashed')
#lines(l8$doy,l8$IRup,col = 'red', lwd = 1)

#plot SW on same plot
lines(l1$doy,l1$SRup,
      col = 'red', lwd = 2)
lines(h1$doy,h1$SRup,
      col = 'black', lwd = 2)
legend('topright', bty = 'n', 'Subcanopy LW',cex = acx)
legend('bottomright', bty = 'n', 'Subcanopy SW',cex = acx)
# plot sub canopy SW
# plot(l1$doy,l1$SRup,
#      type = 'l', col = 'red', lwd = 2,lty = 'dashed',
#      xlim = xl,
#      xlab = 'Day of Year',ylab = '',
#      xaxt = 'n',yaxt = 'n')
# axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
# axis(side = 1, labels = F, tick = T, cex.axis = acx)
# legend('topright', bty = 'n', 'Subcanopy SW',cex = acx)
# 
# lines(h1$doy,h1$SRup,
#       col = 'black', lwd = 2, lty = 'dashed')

# total sub canopy radiation
#par(mfrow = c(4,1),mar=c(0,5,0,2),new=T)
plot(l1$doy,l1$SRup+l1$IRup,
     type = 'l', col = 'red', lwd = 2,
     xlim = xl,
     xlab = 'Day of Year',
     xaxt = 'n',yaxt = 'n',ylab = '')
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = F, tick = T, cex.axis = acx)
legend('topright', bty = 'n', 'Total Downwelling Subcanopy Radiation ',cex = acx)

lines(h1$doy,h1$SRup+h1$IRup,
      col = 'black', lwd = 2)

#plot subcanopy total
# plot sub canopy SW
#par(mfcol = c(4,1),mar=c(0,5,0,2),new=T)
# plot soil heat flux for 2016
plot(lh$doy,lh$shf,type = 'l',
     col='red', lwd=2,
     xlim = xl,
     xaxt = 'n',yaxt = 'n',
     ylab = '')
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = T, tick = T, cex.axis = acx)
legend('topright', bty = 'n', 'Soil Heat Flux ',cex = acx)

lines(hh$doy,hh$shf,col='black',lwd=2)
dev.off()

###############################################################################################

# subcanopy Rad difference vs. soil heat flux difference
plot((all.day$SRupL1+all.day$IRupL1)-(all.day$SRupH1+all.day$IRupH1),
     all.day$shfL-all.day$shfH)

# LW difference vs. SW difference
plot(all.day$SRupH1-all.day$SRupL1, all.day$IRupH1-all.day$IRupL1)



