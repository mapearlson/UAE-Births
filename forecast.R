setwd("~/Documents/Delma/Births")
d <- read.csv("rate.csv")
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
dts.diff <- diff(dts, differences=3)
plot.ts(dts.diff)
