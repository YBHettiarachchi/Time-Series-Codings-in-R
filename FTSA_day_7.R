library(fpp)
?wmurders
plot(wmurders)
length(wmurders)

###Partitioning
trainingSet = window(ts(wmurders),1,43)
testingSet = window(ts(wmurders),44,55)
trainingSet = ts(trainingSet,frequency=12,start = 1950)
testingSet = ts(testingSet,frequency=12,start = 2004)
plot(trainingSet)

###Make variance constant by transforming
LWmurd=log(trainingSet)
plot(LWmurd)
Acf(LWmurd,100)

###Identify the order of non-seasonal differencing
D1LWmurd = diff(LWmurd,1)
plot(D1LWmurd)
Acf(D1LWmurd,100)#all the lags are within the confident intevel, (boundry) and all the lags are insignificant, 
#it will be represent the white note process

Pacf(D1LWmurd,100)

#Box.test - it helps determine whether there is significant autocorrelation in the data.
Box.test(D1LWmurd, lag=12, type="Ljung")
#The result of this test will typically include a test statistic and a p-value. You would interpret the p-value to determine whether the data exhibits significant autocorrelation. 
#If the p-value is below a certain significance level (e.g., 0.05), you may reject the null hypothesis, 
#suggesting that there is evidence of autocorrelation in the data.
#In summary, this command is used to perform a statistical test to assess 
#whether there is significant autocorrelation in the first derivative of the data represented by D1LWmurd, up to a lag of 12 time periods, using the Ljung-Box test.

###Modeling using the recent set of data
trainingSet = window(ts(wmurders),22,50)
testingSet = window(ts(wmurders),51,55)
plot(trainingSet)
Acf(trainingSet,100)#like a stationary series since by 3rd point it cuts off and close to the zero
Pacf(trainingSet,100)

###Parameter estimation
# fitting an ARIMA(1,0,0) model, which means it has an autoregressive (AR) component of order 1 and no differencing (I) or moving average (MA) components.
fit1 = Arima(trainingSet,order=c(1,0,0),include.mean=TRUE)
# summary typically includes information about the model's coefficients, standard errors, and various statistical tests.
summary(fit1)
# It provides p-values for each coefficient, which can help you assess whether each parameter is statistically significant in explaining the time series data.
coeftest(fit1) #significance of parameters
