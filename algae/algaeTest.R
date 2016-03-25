library(DMwR)
data(algae)

library(car)
par(mfrow=c(1,2))
hist(algae$mxPH,prob=T,ylim = 0:1)
lines(density(algae$mxPH,na.rm = T))
qq.plot(algae$mxPH)