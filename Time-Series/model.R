#Xiaojun Sun.
#2014-12-19

library(astsa)
library(tseries)
library(forecast)
library(FinTS)
library(parallel)
library(rugarch)

setwd("E:/RSpace/TimeSeries/HomeWork")
# load("E:/RSpace/TimeSeries/HomeWork/data.RData")
# save.image("E:/RSpace/TimeSeries/HomeWork/data.RData")

Sys.setlocale("LC_TIME", "English")
# dpfcl <- diff(pfcl)
#
# cf <- acf2(dpfcl,na.action = na.pass, 20)

cf <- acf2(as.numeric(pfret),na.action = na.pass, 20)

Acf(as.numeric(pfret), 20, na.action = na.pass)

# unit root test
message("\n The p-value of ADF test is ",adf.test(pfret)$p.value, ". The p-value of PP test is ",pp.test(pfret)$p.value, ". \n The p-value of KPSS test is ",kpss.test(pfret)$p.value,".")

arima(pfret, order = c(1,0,1))

am <- arima(pfret, order = c(2,0,2))
am <- arima(pfret, order = c(1,0,0))

tsdiag(am)
resid <- na.approx(am$residuals)
ArchTest(resid, lags=5, demean = TRUE)

plot(resid)
plot(pfret^2,ylab="Squared Daily Log Return", xlab="Time")

auto.arima(pfret)

pred <- predict(am, n.ahead = 5)
plot(pred$pred)
ts.plot(pred$pred)

fcst <- forecast(am, h=10)
plot(forecast(am, h=10))
plot(fcst$mean)

h10 <- diff(log(window(pf.zoo, start=as.POSIXct("2014-01-01"), end=as.POSIXct("2014-01-16"))))

plot(as.numeric(h10),type="b")
lines(as.numeric(fcst$mean))

# fit garch
garch11.spec <- ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(garchOrder = c(1,0), model = "sGARCH"), distribution.model = "norm")

garch.fit <- ugarchfit(garch11.spec, data = pfret, fit.control=list(scale=TRUE))

print(garch.fit)
plot(garch.fit)

forcst <- ugarchforecast(garch.fit, data = NULL, n.ahead = 10, n.roll= 0, out.sample = 0)

plot(forcst@forecast$seriesFor)

##
pffull <- diff(log(window(pf.zoo, start=as.POSIXct("2009-01-01"), end=as.POSIXct("2014-11-27"))))

cl <- makeCluster(detectCores())

pfroll <- ugarchroll(garch11.spec, data = pffull, n.ahead = 1, forecast.length =220, refit.every = 5, refit.window = "recursive", solver = "hybrid", fit.control = list(), solver.control = list(), calculate.VaR = FALSE, cluster=cl)

plot(pfroll)

stopCluster(cl)

str(pfroll@forecast)
str(pfroll@model)
plot(pfroll@forecast$density$Realized, type="l", ylab="Returns", ylim=c(-0.05, 0.1))
lines(pfroll@forecast$density$Mu, col="red")
title(main="The Forecast of Return Series(2014-01-01--2014-11-27)")

# plot(pfroll@forecast$density$Mu, type="l", col="red", ylab="Returns")
# lines(pfroll@forecast$density$Realized)

# mse
mean((pfroll@forecast$density$Realized-pfroll@forecast$density$Mu)^2)

plot(pfroll@forecast$density$Sigma, type="l")


