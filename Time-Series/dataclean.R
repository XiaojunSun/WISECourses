#Xiaojun Sun.
#2014-12-19

library(dplyr)
library(zoo)
library(quantmod)
library(forecast)

setwd("E:/RSpace/TimeSeries/HomeWork")

Sys.setlocale("LC_TIME", "English")

pufa <- read.csv("pufa.csv", header= TRUE, stringsAsFactors = FALSE)

pf.zoo <- zoo(pufa$Clsprc, order.by=as.POSIXct(pufa$Trddt,format="%Y/%m/%d"))

pfcl <- window(pf.zoo, start=as.POSIXct("2009-01-01"), end=as.POSIXct("2013-12-31"))

chartSeries(pf.zoo, name="Daily Adjusted Close Price for PuFa", theme="white",type = "lines")

chartSeries(pfcl, name="Daily Adjusted Close Price for PuFa", theme="white",type = "lines")

# return series

pfret <- diff(log(pfcl))

chartSeries(pfret, name="Log Daily Returns for PuFa", theme="white")




