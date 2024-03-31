library(fpp)

?beer
plot(beer)
length(beer)

#creates a training set by extracting the time series data from "beer" starting from the first data point up to the 44th data point.
trainingSet = window(ts(beer),1,44)
testingSet = window(ts(beer),45,56)

#converts the time series data in the variable "trainingSet" into a seasonal time series 
#by specifying a frequency of 12 (indicating monthly data) and setting the start date to January 1991.
trainingSet = ts(trainingSet,frequency=12,start=c(1991,1))
#9-month,19994-year
testingSet = ts(testingSet,frequency=12,start=c(1994,9))

plot(trainingSet)

###########################fit a suitable Exponential Smoothing Model using the training set

#fits a time series forecasting model using the Holt-Winters method with additive seasonality
fit1 <- hw(trainingSet,seasonal="additive")

#alpha = 0.1, beta = 0.1, and gamma = 0.5 are the smoothing parameters for the level, trend, and seasonal components, respectively.
fit2 <- hw(trainingSet,seasonal="multiplicative",alpha=0.1,beta=0.1,gamma=0.5)

#damped = TRUE specifies that the model includes damping on the trend component, which means that 
#the trend is expected to level off and become stable over time.
fit3 <- hw(trainingSet,damped = TRUE,seasonal="multiplicative")

# assess how well the model's predictions align with the actual observed values in the time series. 
accuracy(fit1)
accuracy(fit2)
accuracy(fit3)
#select the model with less error(MAPE-uses error as a percentage,its fit3)

#can see the forecasted values by,
fit3
#to extract only the point forecasts(mean,mode,fitted values,errors,etc)
fit3$mean
fit3$fitted
fit3$residuals

t <- seq(1,length(trainingSet),1)
matplot(t,cbind(trainingSet,fit3$fitted),type="l",col=c("red","green"),lty=c(1,1))
#can use to draw multiple columns in a dataframe
t <-seq(1,length(trainingSet),1)
matplot(t,cbind(testingSet,fit3$mean[1:12]),type = "l",col= c("red","green"),lty=c(1,1))

###############################Classical decomposition approach
?decompose
#only do extimation of the model#This code decomposes the time series data stored in the variable trainingSet 
#into its additive components using classical decomposition.
ClassicalModel <- decompose(trainingSet,type="additive")

#This code calculates the fitted values by adding together the seasonal and trend components from the decomposition model stored in ClassicalModel.
#The addition of these two components gives you the fitted values, 
#which represent the estimated values of the original time series after removing the seasonal and trend patterns.
fittedval = ClassicalModel$seasonal + ClassicalModel$trend

t <-seq(1,length(trainingSet))
matplot(t,cbind(trainingSet,fittedval),type="l",col=c("red","green"),lty=c(1,1))

?stl
##for forecasting  #better use stl func to use decomposition as decompose func do not forecast values
#stl: This is a function used for performing seasonal and trend decomposition on time series data.
#s.window = "periodic": the type of seasonal decomposition to be used. "Periodic" means that the seasonal component is treated as a periodic function.
#robust = TRUE:the STL decomposition should be robust, which means it will be less sensitive to outliers or extreme values in the data, helping to reduce their impact on the decomposition results.
fit <- stl(trainingSet,s.window = "periodic",robust=TRUE)
plot(fit)

fore <- forecast(trainingSet)
plot(fore)

fit
fore <- forecast(fit)
plot(fore)

install.packages("MLmetrics")
library(MLmetrics)

# This extracts the time series components (seasonal and trend) obtained from the STL decomposition model
fit$time.series

#calculates the fitted values by adding together the first column ( seasonal component) and the second column (trend component) of the time series components obtained from the decomposition model.
fittedval = fit$time.series[,1] + fit$time.series[,2]

#calculates the Mean Absolute Percentage Error (MAPE) between the original trainingSet time series and the fittedval (fitted values) obtained from the decomposition model. -gets %
MAPEValma = MAPE(trainingSet,fittedval)*100
MAPEValma
