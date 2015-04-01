setwd("~/Documents/Delma/Births")
# Total and national fertility/crude birth rates are highly correlated.
# Thus, I will use total birth rates given the increase in the amount of historical
# data.
d <- read.csv("rate2.csv")
str(d)
dts <- ts(d$rate, start=c(1960, 1), end=c(2012, 1), frequency=1) 
dts <- ts(d)
plot(dts)

# simple exponential - models level
fitHW <- HoltWinters(dts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fitHW2 <- HoltWinters(dts, gamma=FALSE)
fitHW2.2 <- forecast.HoltWinters(fitHW2, h=7)
plot.forecast(fitHW2.2)

# predictive accuracy
install.packages("forecast")
library(forecast)
accuracy(fitHW)

# predict next three future values
forecast(fitHW2, 7)
plot(forecast(fitHW2, 7))

fitHW3 <- HoltWinters(dts, gamma=FALSE, l.start=48.290, b.start=-0.672)
plot(fitHW)

plot(forecast(fitHW3,7))
forecast(fitHW3, 7)

acf(fitHW2.2$residuals, lag.max=20)
Box.test(fitHW2.2$residuals, lag=20, type="Ljung-Box")

plot.ts(fitHW2.2$residuals)
plotForecastErrors(fitHW2.2$residuals)

###ARIMA
acf(dts)
pacf(dts)
dts.diff <- diff(dts, differences=1)
plot.ts(dts.diff)

install.packages("fUnitRoots")
library(fUnitRoots)
install. packages("fpp")
library(fpp)
############################
pop.ts <- ts(d$total.pop, start=c(1960, 1), end=c(2012, 1), frequency=1) 

# simple exponential - models level
popHW <- HoltWinters(pop.ts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
popHW2 <- HoltWinters(pop.ts, gamma=FALSE)
popHW2.2 <- forecast.HoltWinters(popHW2, h=7)
plot.forecast(popHW2.2)

# predictive accuracy
install.packages("forecast")
library(forecast)
accuracy(fitHW)

# predict next three future values
forecast(popHW2, 7)
plot(forecast(fitHW2, 7))

fitHW3 <- HoltWinters(dts, gamma=FALSE, l.start=48.290, b.start=-0.672)
plot(fitHW)

plot(forecast(fitHW3,7))
forecast(fitHW3, 7)

acf(fitHW2.2$residuals, lag.max=20)
Box.test(fitHW2.2$residuals, lag=20, type="Ljung-Box")

plot.ts(fitHW2.2$residuals)
plotForecastErrors(fitHW2.2$residuals)

###ARIMA
acf(dts)
pacf(dts)

plot(diff(log(dts)))
ddts <- diff(log(dts))

acf(ddts, lag.max=20)
acf(ddts, lag.max=20, plot=FALSE)
pacf(ddts, lag.max=20) # plot a partial correlogram
pacf(ddts, lag.max=20, plot=FALSE) 
auto.arima(ddts)

ddts.arima <- arima(kingstimeseries, order=c(0,1,1)) # fit an ARIMA(0,1,1) model

audts.diff <- diff(dts, differences=3)

