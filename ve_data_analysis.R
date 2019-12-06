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
setwd('/Users/mloranty/Documents/GitHub/viper_energy/')
# this will eventually work
# first we should update all data for the Arctic Data Center
#source('ve_data_processing.R')

#

###################################################
# make a massive timeseries plot showing all data #
###################################################

#define plotting variables
acx <- 3.5
lw <- 3
hc <- 'blue'
lc <- 'red'
xl <- c(2016.5,2018.5)

# x axis location and labels for the 15th of every month
t <- which(day(all.day$date) == 15)
tx <- all.day$dy[which(day(all.day$date) == 15)]
tl <- month(all.day$date[t], label = T, abbr = T)
rm(t)
# x axis location and labels for each year
yx <- na.omit(unique(year(all.day$date)))+0.5
yl <- na.omit(unique(year(all.day$date)))

pdf("figures/canopy_radiation.pdf",28,12)
par(mfrow = c(3,1), mar=c(0,10,0,2), oma=c(12,6,2,2),mgp = c(7,1,0), 
    cex.axis = acx, cex.lab = acx, cex.axis = acx)

# subcanopy SW and LW
plot(all.day$dy,all.day$IRupL1, type = 'l',lwd = lw,
     ylim = c(0,450),
     xlim = xl, 
     ylab = expression(paste("W",m^-2, sep="")),
     xaxt = 'n',yaxt = 'n', xlab = '', col = lc,)
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(at = tx, side = 1, labels = F, tick = T, cex.axis = acx)

lines(all.day$dy, all.day$IRupH1,lwd = lw, col = hc )
lines(all.day$dy, all.day$SRupH1,lwd = lw/2, col = hc )
lines(all.day$dy, all.day$SRupL1,lwd = lw/2, col = lc )

text(2016.5,430,'Mean Daily Subcanopy Radiation',cex = acx, pos = 4, offset = 0)
text(2016.85,300,'LW',cex = acx, pos = 4, offset = 0)
text(2016.85,50,'SW',cex = acx, pos = 4, offset = 0)

legend(2016.9,300, c('High Density', 'Low Density'), col = c (hc,lc),lwd = lw, bty='n',cex = acx)

# differences in incident downwelling subcanopy
plot(all.day$dy,(all.day$IRupL1+all.day$SRupL1)-(all.day$IRupH1+all.day$SRupH1), 
     type = 'l',lwd = lw,
     xlim = xl, 
     ylim = c(-50,75),
     ylab = expression(paste("W",m^-2, sep="")),
     yaxt = 'n', xaxt = 'n',xlab = '', col = 'black')

# ground heat flux difference
#lines(all.day$dy, all.day$shfL-all.day$shfH,lwd = lw, col = 'black', lty = 'dashed' )

axis(side = 2, at = seq(-40,60,20),labels = T, tick = T, las = 2, cex.axis = acx)
axis(at = tx, side = 1, labels = F, tick = T, cex.axis = acx)
abline(h=0,col = "gray", lty = 'dashed', lwd = 2)

text(2016.5,70,'Subcanopy Radiation Difference: Low Density - High Density',
     cex = acx, pos = 4, offset = 0, bg = 'white')

# albedo/ndvi
plot(all.day$dy,all.day$albL8, 
     type = 'l',lwd = lw,
     ylim = c(0,1),
     xlim = xl, 
     ylab = 'Albedo/NDVI',
     xaxt = 'n',yaxt = 'n',xlab = '', col = lc,)
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(at = tx, side = 1, labels = F, tick = T, cex.axis = acx, outer =T)
mtext(tl, side = 1, line = 2.5, at = tx, cex = acx-1)
mtext(yl, side = 1, line = 5.5, at = yx, cex = acx-1)

lines(all.day$dy, all.day$albH8,lwd = lw, col = hc )
lines(all.day$dy, all.day$ndvi.L,lwd = lw, col = lc, lty = 'dashed' )
lines(all.day$dy, all.day$ndvi.H,lwd = lw, col = hc, lty = 'dashed' )

legend(2016.85,0.8, c('High Density NDVI', 'Low Density NDVI', 'High Density Albedo', 'Low Density Albedo'), 
       col = c (hc,lc),lwd = lw, , lty = rep(c('dashed', 'solid'), each=2), bty='n',cex = acx-1)

dev.off()

#####################################################################
# Air/Soil T and Soil Heat Flux
pdf("figures/temp_plots.pdf",28,12)
par(mfrow = c(3,1), mar=c(0,10,0,2), oma=c(12,6,2,2),mgp = c(7,1,0), 
    cex.axis = acx, cex.lab = acx, cex.axis = acx)

############################### Air Temp & precip
plot(all.day$dy,all.day$Tair.L, 
     type = 'l',lwd = lw,
     xlim = xl, 
     ylim = c(-42, 22),
     xaxt = 'n',yaxt = 'n', ylab = '',xlab = '', col = lc,)
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = F, tick = T, cex.axis = acx)

lines(all.day$dy, all.day$Tair.H,lwd = lw, col = hc )

# 1m radiometric surface temp - hard to differentiate
#lines(all.day$dy, all.day$TsrfH1,lwd = lw/2, col = hc, lty = 'dashed' )
#lines(all.day$dy, all.day$TsrfL1,lwd = lw/2, col = lc, lty = 'dashed'  )

abline(h=0,col = "gray", lty = 'dashed', lwd = 2)

legend('bottomleft', 'Air Temperature', bty = 'n', cex = acx)


############################### ground heat flux
plot(all.day$dy,all.day$shfH, type = 'l',lwd = lw,
     xlim = xl, 
     ylim = c(-5,27),
     ylab = expression(paste("W",m^-2, sep="")),
     xaxt = 'n',yaxt = 'n',xlab = '', col = hc,)
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = F, tick = T, cex.axis = acx)

lines(all.day$dy, all.day$shfL,lwd = lw, col = lc )
# difference in ground heat flux
#lines(all.day$dy, all.day$shfL-all.day$shfH,lwd = lw, col = 'black' )
abline(h=0,col = "gray", lty = 'dashed', lwd = 2)
legend('topleft', 'Ground Heat Flux', bty = 'n', cex = acx)

############################### soil & surface Temp


plot(all.day$dy,all.day$Tsoil.H5, 
     type = 'l',lwd = lw,
     xlim = xl, 
     ylim = c(-12,12),
     xaxt = 'n',yaxt = 'n', ylab = '',xlab = '', col = hc,)
axis(side = 2, labels = T, tick = T, las = 2, cex.axis = acx)
axis(side = 1, labels = F, tick = T, cex.axis = acx)
mtext(tl, side = 1, line = 2.5, at = tx, cex = acx-1)
mtext(yl, side = 1, line = 5.5, at = yx, cex = acx-1)

lines(all.day$dy, all.day$Tsoil.L5,lwd = lw, col = lc )
lines(all.day$dy, all.day$Tsoil.L50,lwd = lw, col = lc, lty = 'dashed' )
lines(all.day$dy, all.day$Tsoil.H50,lwd = lw, col = hc, lty = 'dashed' )
legend('bottomleft', 'Soil Temperature', bty = 'n', cex = acx)
legend(2017.5,0, c('High Density 5cm', 'Low Density 5cm', 'High Density 50cm', 'Low Density 50cm'), 
       col = c (hc,lc),lwd = lw, , lty = rep(c('solid', 'dashed'), each=2), bty='n',cex = acx-1)

dev.off()

#####################################################################
# cumulative Soil Heat Flux
plot(hf.day.ld17$doy,hf.day.ld17$cs*0.0036,type="l",
     ylab = "Cumulative G (MJ)",xlab="")
lines(hf.day.hd17$doy,hf.day.hd17$cs*0.0036,col="red")
lines(hf.day.hd16$doy,hf.day.hd16$cs*0.0036,col="red",lty="dashed")
lines(hf.day.ld16$doy,hf.day.ld16$cs*0.0036,lty="dashed")

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



