### Time Series Analysis
library(ggplot2)
library(dplyr)
library(e1071)
library(forecast)
library(lubridate)
library(quantmod)
library(xts)
library(PerformanceAnalytics)

### plot data

data("AirPassengers")
AP <- AirPassengers
plot(AP)      ### This graph has trend as well as seasonality

plot(AP,type="o")

### log transform
AP <- log(AP)
### This graph is similar to scatter plot
### lag1 means , we are comparing one point with previous one
### lag2 means , we are camparing one point with previous second month

lag.plot(AP,lags = 6,do.lines = F)
### This plot can be done in another way
acf(AP)
### acf is corelaton coefficeinet
### this is similar to correlalogram
### there is another plot
pacf(AP)
### This is non stationary time series

### when mean , sd ,and variance change with time
### it is called non stationary time series.


### Differencing for changing the non stat to stationary time series.
### Decomposition
diff(5:25,2)   ### example for differencing
d <- diff(AP)
plot(d)
acf(d)
pacf(d)

### Decompostion
 decomp <- decompose(AP)
plot(decomp)

### ARIMA - Auto generated integrated model
model <- auto.arima(AP)
acf(model$residuals)
pacf(model$residuals)
### 1st part , non seaonal (0,1,1) (p,d,q)
### 2nd part, Seasonal  (0,1,1)(P,D,Q)
### p is the orer of AR

summary(model)

### Box -l-jung Test
Box.test(model$residuals,lag = 20,type = "Ljung-Box")

### Residual Plot
hist(model$residuals,
     col = "lightblue",
     xlab = "error",
     main = "histogram of residuls",
     freq = FALSE)
### Now we can use it for forcasting
f <- forecast(model,24)
autoplot(f)

f$x <- exp(f$x)
f$mean <- exp(f$mean)
autoplot(f)
f$lower <- exp(f$lower)
f$upper <- exp(f$upper)







































