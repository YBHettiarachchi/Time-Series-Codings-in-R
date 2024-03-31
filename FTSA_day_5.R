library(fpp)
library(forecast)

plot(austourists)

#the autocorrelation function (ACF) of the time series data "austourists" with a lag range of 0 to 50.
#50-max lag
a <- Acf(austourists,50)
b <- Pacf(austourists,50)

#To calculate first-order differencing at lag 1
# can be useful for removing trends or seasonality in the data or for preparing it for further analysis.
D1y = diff(austourists,1)
D1y
plot(D1y)

#To calculate lag 4 difference
D4y = diff(austourists,4)
D4y
plot(D4y)

#To calculate 2nd order differencing
DD1y = diff(D1y,1)
DD1y
   #or
DD1y = diff(diff(austourists,1),1)
DD1y

#to get the de-seasonalized series with lag 1
D1D4y=diff(diff(austourists,4),1)  #or
D1D4y=diff(D4y,1)
D1D4y
plot(D1D4y)

#to get the de-trended series with lag 4
D4D1y = diff(D1y,4)
D4D1y
plot(D4D1y)

#provides insights into the autocorrelation structure of the respective time series data at different lags, 
#which can be useful for understanding seasonality, trends, and potential patterns in the data. 
Acf(austourists,50)
Acf(D4y,50)
Acf(D1y,50)
Acf(D4D1y,50)
Acf(AirPassengers,100)


#L-jung box test

#the Box.test function is used to check whether the "austourists" time series data exhibits serial correlation up to a lag of 12 or if it resembles white noise.
Box.test(austourists,12)
?Box.test
