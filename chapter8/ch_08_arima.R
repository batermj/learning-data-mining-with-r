#clean the workspace and memory
rm( list=ls() )
gc()

library(forecast)
library(zoo)
library(xts)

#data003 <- read.csv("data/datasets006.csv", header=TRUE)
data003 <- WWWusage

arima.model <- Arima(data003,order=c(3,1,0),fixed=c(0,NA,0,NA))
tsdiag(arima.model)
print(arima.model)
forecast(arima.model,h=20)

arima.model <- auto.arima(data003)
tsdiag(arima.model)
print(arima.model)
forecast(arima.model,h=20)
