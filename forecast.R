library(forecast)
library(fpp)
library(ggplot2)
require(latticeExtra)
require(ggplot2)
require(reshape2)
require(devtools)
suppressPackageStartupMessages(
  require(googleVis)
)
require(quantmod)
require(PerformanceAnalytics)
require(xtsExtra)
require(devtools)
install_github('rCharts', 'ramnathv')

# Total and national fertility/crude birth rates are highly correlated.
# Thus, I will use total birth rates given the increase in the amount of historical
# data.
d <- read.csv("rate2.csv")
str(d)
dts <- ts(d$rate, start=c(1960, 1), end=c(2012, 1), frequency=1) 
dts <- ts(d)
plot(dts) # giant trend dowanward.  Have to difference.

# simple exponential 
fitHW <- HoltWinters(dts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fitHW2 <- HoltWinters(dts, gamma=FALSE)
fitHW2.2 <- forecast.HoltWinters(fitHW2, h=7)
plot.forecast(fitHW2.2)

# predictive accuracy
accuracy(fitHW)

# predict next seven future values
forecast(fitHW2, 7)
plot(forecast(fitHW2, 7))

fitHW3 <- HoltWinters(dts, gamma=FALSE, l.start=48.290, b.start=-0.672)
plot(fitHW)

plot(forecast(fitHW3,7))
forecast(fitHW3, 7)

acf(fitHW2.2$residuals, lag.max=20)
Box.test(fitHW2.2$residuals, lag=20, type="Ljung-Box")
# Box-Ljung test
# data:  fitHW2.2$residuals
# X-squared = 310.6668, df = 20, p-value < 2.2e-16

plot.ts(fitHW2.2$residuals)
plotForecastErrors(fitHW2.2$residuals)

# Must use ARIMA.  Above steps are pretty unecessary, given that the plot is
# obviously non-stationary, but I wanted to go through them.  
# ARIMA
acf(dts)
pacf(dts)
dts.diff <- diff(dts, differences=1)
plot.ts(dts.diff)

############################
pop.ts <- ts(d$total.pop, start=c(1960, 1), end=c(2012, 1), frequency=1) 

# simple exponential - models level
popHW <- HoltWinters(pop.ts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
popHW2 <- HoltWinters(pop.ts, gamma=FALSE)
popHW2.2 <- forecast.HoltWinters(popHW2, h=7)
plot.forecast(popHW2.2)

# predictive accuracy

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
auto.arima(dts)

ddts.arima <- arima(dts, order=c(4,2,3))# fit an ARIMA(4,2,3) model
ddts.arima2 <- arima(dts, order=c(2,1,2))

arima.forecast <- forecast.Arima(ddts.arima, h=10)
arima.forecast
summary(arima.forecast)
plot(arima.forecast)
accuracy(arima.forecast)

acf(arima.forecast$residuals, lag.max=20)
Box.test(arima.forecast$residuals, lag=20, type="Ljung-Box")
# Box-Ljung test
# data:  arima.forecast$residuals
# X-squared = 27.1241, df = 20, p-value = 0.1318

plot.ts(arima.forecast$residuals) # make time plot of forecast errors
plotForecastErrors(arima.forecast$residuals) # make a histogram

fert.ts <- ts(d$fert, start=c(1960, 1), end=c(2012, 1), frequency=1)

#graphs
par(mfrow = c(3,1))
plot(dts, ylab="Crude birth rate (1,000)") 
plot(fert.ts, ylab="Fertility Rate")
plot(pop.ts, ylab="Total Population")

ggplot() + geom_line(data=dts, aes(x = time, y = "Crude Birth Rate", color = isin)) 
stat_smooth(data=arima.forecast$, aes(x = time, y = M, color = isin))

# although slightly out of chronology
# I'll also use theEconomist from latticeExtra
par(mfrow = c(3,1))
asTheEconomist(
  xyplot(
    dts,
    scales = list( y = list( rot = 0 ) ),
    main = "Birth Rate"  
  )
)

asTheEconomist(
  xyplot(
    fert.ts,
    scales = list( y = list( rot = 0 ) ),
    main = "Fertility Rate"  
  )
)

asTheEconomist(
  xyplot(
    pop.ts,
    scales = list( y = list( rot = 0 ) ),
    main = "Total Population"  
  )
)

#
d2<-d
d$year <- format(d$year, "%Y")
m1 <- mPlot(
  rate ~ year,
  data = d,
  type = "Line"
)
m1$set( pointSize = 0 )
m1$set( hideHover = "auto" )
m1$print("chart2")

#
d2 <- d
d2$year <- as.character(d2$year)
m1 <- mPlot(x = "year", y = "rate", type = "Line", data = d2)
m1$set(pointSize = 0, lineWidth = 1)
m1$print("chart2")
m1

nat <- read.csv("national.csv")
plot(nat$pop.n)



