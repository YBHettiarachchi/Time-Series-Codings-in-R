library(fpp)
install.packages("fUnitRoots")
library(fUnitRoots)

plot(chicken)
x=chicken
length(chicken)

adfTest(x,lags=1,type=c("c"))
trainingSet = window(ts(chicken),22,60)
testingSet = window(ts(chicken),61,70)
trainingSet = ts(trainingSet,frequency=1,start = 1945)
testingSet = ts(testingSet,frequency=1,start = 1946)
plot(trainingSet)

###Identify the order of non-seasonal differencing
D1Chick = diff(trainingSet,1)    ###de-trend
plot(D1Chick)

#Augmented Dickey Fuller test - statistical test for checking the stationarity of a time series.
# If the p-value is below a certain significance level (e.g., 0.05), you may conclude that the data is stationary (i.e., it does not have a unit root). 
#If the p-value is above the significance level, you may conclude that the data is non-stationary.adfTest(D1Chick,lags=1,type=c("c"))
Acf(D1Chick,100) #0,1,2,3
Pacf(D1Chick,100) #1

###Parameter estimation
#ncludes an autoregressive (AR) component of order 1, a differencing (I) of order 1, and no moving average (MA) component.
fitAR1=Arima(trainingSet,order=c(1,1,0),include.mean = TRUE)
summary(fitAR1)
Box.test(residuals(fitAR1),lag=12,type="Ljung")
coeftest(fitAR1)

fitMA1=Arima(trainingSet,order=c(0,1,1),include.mean = TRUE)
summary(fitMA1)
Box.test(residuals(fitMA1),lag=12,type="Ljung")
coeftest(fitMA1)

fitMA2=Arima(trainingSet,order=c(0,1,2),include.mean = TRUE)
summary(fitMA2)
Box.test(residuals(fitMA2),lag=12,type="Ljung")
coeftest(fitMA2)

fitMA3=Arima(trainingSet,order=c(0,1,3),include.mean = TRUE)
summary(fitMA3)
Box.test(residuals(fitMA1),lag=12,type="Ljung")
coeftest(fitMA3)

forecast(fitMA2)
plot(forecast(fitMA2))

#explanation of fitMA1
#p: This is the order of the autoregressive (AR) component. The autoregressive component captures the relationship between the current value of the time series and its past values. A higher "p" means that the model considers more past observations in predicting the current value. A value of 0 means there is no autoregressive component.
#d: This is the order of differencing required to make the time series stationary. Differencing involves subtracting the previous value from the current value to remove any trend or seasonality. The value "d" indicates how many times differencing is applied. For example, d = 1 means you are differencing the time series once.
#q: This is the order of the moving average (MA) component. The moving average component captures the relationship between the current value of the time series and past forecast errors (residuals). A higher "q" means that the model considers more past forecast errors in predicting the current value. A value of 0 means there is no moving average component.

#p = 0: There is no autoregressive component in your model. This means the current value of the time series is not influenced by its past values.
#d = 1: You have applied first-order differencing to the time series. This suggests that you are working with a non-stationary time series and have differenced it once to make it stationary.
#q = 1: You have a first-order moving average (MA) component in your model. This means the current value of the time series is influenced by the past forecast error (residual) with a lag of 1 time period.

