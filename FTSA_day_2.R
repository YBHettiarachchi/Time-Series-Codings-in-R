library(fpp)

?wmurders
plot(wmurders)
After1975 = window(ts(wmurders),25,55)
After1975 = ts(After1975,start = 1974,end = 2004)
plot(After1975)

##################moving average approach
MA_5 = ma(After1975,5)
plot(MA_5)
#plot actuals and fitted values using matplot
t = seq(1,length(After1975),1)
#This code generates a sequence of numbers that starts from 1, goes up to the length of the object After1975,
#and increments by 1 for each step. Essentially, it creates a sequence of consecutive integers from 1 to the length of After1975.

matplot(t, cbind(After1975,MA_5),type="l",col=c("red","green"),lty=c(1,1))
#create a line plot with two lines (red and green) representing the data in the After1975 and MA_5 vectors, 
#respectively, with t on the x-axis as the sequence of numbers.
?matplot


#######################Regression approach
t = seq(1,length(After1975),1)
fit = lm(After1975 ~ t)
#fits a linear regression model to the data, using After1975 as the dependent variable and t as the independent variable 
fits <- fit$fitted.values

matplot(t,cbind(After1975,fits),type="l",col=c("red","green"),lty=c(1,1))
#it will create a line plot with two lines. One line represents the actual data in the After1975 vector (in red), 
#and the other line represents the predicted values from the fits vector (in green).
summary(fit)


#########################Single exponential smoothing Approach
library(fpp)
plot(oil)
?oil
FitSES <- ses(oil,h=12,alpha=0.8)
#will contain the SES model object, 
#which you can use to make forecasts for the next 12 time periods based on the "oil" data and the chosen smoothing parameter.
t = seq(1,length(oil),1)
FitSES$fitted
FitSESFitted <- FitSES$fitted
matplot(t,cbind(oil,FitSESFitted),type="l",col=c("red","green"),lty=c(1,1))


############################Double exponential smoothing approach
plot(ausair)
?ausair

#This code fits a Holt exponential smoothing model 
#to the 'ausair' time series data with a forecasting horizon of 12 time periods.
FitHolt <- holt(ausair,h=12)

#This code retrieves the parameters and specifications of the Holt exponential smoothing model fitted to the 'ausair' data.
FitHolt$model

#This code provides the fitted values produced by the Holt exponential smoothing model for the 'ausair' data, which are the model's predictions for the observed time series.
FitHolt$fitted

########################################Tripple Exponential Smoothing Approach
plot(austourists)

#fits a Holt-Winters exponential smoothing model with multiplicative seasonality to the 'austourists' time series data.
fitHW <- hw(austourists,seasonal="multiplicative")

#stores the fitted values produced by the Holt-Winters model in the 'fitHWfitted' variable.
fitHWfitted <- fitHW$fitted

# creates a sequence of time indices 't' from 1 to the length of the 'austourists' data.
t = seq(1,length(austourists),1)

#creates a line plot that overlays the original 'austourists' data in red and the fitted values from the Holt-Winters model in green, with solid lines for both, to visualize how well the model fits the data
matplot(t,cbind(austourists,fitHWfitted),type="l",col=c("red","green"),lty=c(1,1))