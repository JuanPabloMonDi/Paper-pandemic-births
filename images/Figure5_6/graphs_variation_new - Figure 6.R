## graphs variations
dat = read.csv('dat_var_new.csv')




# var 1
windows(height = 10,width = 15)
par(mfrow=c(2,2))

#BRA
with(subset(dat, country =='bra'& educ =='less_7'), plot(1:4,var, pch=3, col='blue', ylim=c(-40,20), main='Brazil, comparison of ASFR variations\nprojected vs. estimated', ylab = "% Variations in ASFR", xlab='Age groups', axes = F))
axis(1,at=1:4,labels=c('15-24','25-29','30-34','35+'))
axis(2, at=(c(-40,-30,-20,-10,0,10,20)))
abline(h=0, col='red', lwd=2)
with(subset(dat, country =='bra'& educ =='8_11'), points(1:4,var, pch=6, ylim=c(-65,20))) 
with(subset(dat, country =='bra'& educ =='12_more'), points(1:4,var, pch=9, col='forestgreen', ylim=c(-65,20)))
box()
legend('bottomright',legend = c("less than 7 years", "8 to 11 years", "12+ years"), col=c('blue','black','forestgreen'), pch=c(3,6,9), lwd=1, cex=.9, bty='n')

#CHI

with(subset(dat, country =='chi'& educ =='less_7'), plot(1:4,var, pch=3, col='blue', ylim=c(-40,20), main='Chile, comparison of ASFR variations\nprojected vs. estimated', ylab = "% Variations in ASFR", xlab='Age groups', axes = F))
axis(1,at=1:4,labels=c('15-24','25-29','30-34','35+'))
axis(2, at=(c(-40,-30,-20,-10,0,10,20)))
abline(h=0, col='red', lwd=2)
with(subset(dat, country =='chi'& educ =='8_11'), points(1:4,var, pch=6, ylim=c(-65,20))) 
with(subset(dat, country =='chi'& educ =='12_more'), points(1:4,var, pch=9, col='forestgreen', ylim=c(-65,20))) 
legend('bottomright',legend = c("less than 7 years", "8 to 11 years", "12+ years"), col=c('blue','black','forestgreen'), pch=c(3,6,9), lwd=1, cex=.9, bty='n')
box()

#CRC
with(subset(dat, country =='crc'& educ =='less_7'), plot(1:4,var, pch=3, col='blue', ylim=c(-40,20), main='Costa Rica, comparison of ASFR variations\nprojected vs. estimated', ylab = "% Variations in ASFR", xlab='Age groups', axes = F))
axis(1,at=1:4,labels=c('15-24','25-29','30-34','35+'))
axis(2, at=(c(-40,-30,-20,-10,0,10,20)))
abline(h=0, col='red', lwd=2)
with(subset(dat, country =='crc'& educ =='8_11'), points(1:4,var, pch=6, ylim=c(-65,20))) 
with(subset(dat, country =='crc'& educ =='12_more'), points(1:4,var, pch=9, col='forestgreen', ylim=c(-65,20))) 
legend('bottomright',legend = c("less than 7 years", "8 to 11 years", "12+ years"), col=c('blue','black','forestgreen'), pch=c(3,6,9), lwd=1, cex=.9, bty='n')
box()

#MEX
with(subset(dat, country =='mex'& educ =='less_7'), plot(1:4,var, pch=3, col='blue', ylim=c(-40,20), main='Mexico, comparison of ASFR variations\nprojected vs. estimated', ylab = "% Variations in ASFR", xlab='Age groups', axes = F))
axis(1,at=1:4,labels=c('15-24','25-29','30-34','35+'))
axis(2, at=(c(-40,-30,-20,-10,0,10,20)))
abline(h=0, col='red', lwd=2)
with(subset(dat, country =='mex'& educ =='8_11'), points(1:4,var, pch=6, ylim=c(-65,20))) 
with(subset(dat, country =='mex'& educ =='12_more'), points(1:4,var, pch=9, col='forestgreen', ylim=c(-65,20))) 
legend('bottomright',legend = c("less than 7 years", "8 to 11 years", "12+ years"), col=c('blue','black','forestgreen'), pch=c(3,6,9), lwd=1, cex=.9, bty='n')
box()
savePlot("Figure6_Comparison",type="pdf")
savePlot("Figure6_Comparison",type="eps")
savePlot("Figure6_Comparison",type="png")

