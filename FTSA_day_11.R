library(fpp)
plot(euretail)
?euretail

#calculates and plots the autocorrelation function (ACF) for the "euretail" time series up to lag 100. The ACF shows the correlation between the series and its past values at different lags, helping to identify potential seasonality or patterns.
Acf(euretail,100)

Deuretail=diff(euretail,1)
Acf(Deuretail,100)
Pacf(Deuretail,100)

#classical decompo
fit <- stl(euretail,s.window ="periodic",robust = TRUE)
plot(fit)

D4D1 = diff(Deuretail,4)
Acf(D4D1,100)  #q=1
Pacf(D4D1,100)
auto.arima(euretail)

?Arima
fit1 = Arima(euretail,order=c(0,1,3),seasonal = c(0,1,1))
summary(fit1)
coeftest(fit1)
?coeftest

Box.test(residuals(fit1))
forecast(fit1,4)
plot(forecast(fit1,4))
fitted(fit1)
