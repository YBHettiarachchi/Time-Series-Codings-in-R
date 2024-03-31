install.packages("AirPassengers")
library(fpp)

?AirPassengers
plot(AirPassengers)
length(AirPassengers)

#partition the dataset into training set and testing set.
trainingSet = window(ts(AirPassengers),1,132)
testingSet = window(ts(AirPassengers),133,144)

#convert the testing and training data into time series datasets
trainingSet = ts(trainingSet,frequency = 12,start = c(1949,1))
testingSet = ts(testingSet,frequency = 12,start = c(1960,1))

#plot train and test data 
plot(trainingSet)
plot(testingSet)

########################################Exponential Smoothing Model
#fit a suitable Exponential Smoothing Model using the training set
fitmodel1 = hw(trainingSet,seasonal = 'multiplicative',alpha = 0.1,beta = 0.1,gamma = 0.5)
?hw
fitmodel2 = hw(trainingSet,damped = TRUE, seasonal = 'multiplicative')
fitmodel3 = hw(trainingSet,seasonal = 'multiplicative')

#Select the less in MAPE value,which gives highest accuracy
#It's fitModel2
accuracy(fitmodel1)
accuracy(fitmodel2)
accuracy(fitmodel3)
?accuracy

#get forecasted values
fitmodel2
fitmodel2$mean

#plotting actual vs predicted values
t = seq(1,length(trainingSet),1)
#plot for model 2
matplot(t,cbind(trainingSet,fitmodel2$fitted),type='l',col=c('red','green'),lty=c(1,1))

t <- seq(1,length(trainingSet),1)
matplot(t,cbind(testingSet,fitmodel2$mean[1:12]),type = "l",col= c("red","green"),lty=c(1,1))

##########################################Classical decomposition approach
#fit a suitable Classical Decomposition Model using the training set
?decompose
ClassicalModel <- decompose(trainingSet,type = "multiplicative")
ClassicalModel

fitted_Deco = ClassicalModel$seasonal + ClassicalModel$trend
fitted_Deco

#plot actual vs.forecasted values
t <- seq(1,length(trainingSet))
matplot(t,cbind(trainingSet,fitted_Deco),type = "l",col= c("red","green"),lty=c(1,1))

fit <- stl(trainingSet,s.window ="periodic",robust = TRUE)
plot(fit)
fit
fore <- forecast(trainingSet)
plot(fore)


#calculate MAPE for the training period for each model.
install.packages("MLmetrics")
library(MLmetrics)

#for Classical Decomposition Model
fit$time.series
fitted_Deco = fit$time.series[,1] + fit$time.series[,2]
MAPEValma = MAPE(trainingSet,fitted_Deco)*100
MAPEValma

#for Exponential Smoothing Model
fitmodel2$time.series
Fitted_Expo = fitmodel2$time.series[,1]+fitmodel2$time.series[,2]
MAPEValMA = MAPE(trainingSet,Fitted_Expo)*100
MAPEValMA

#When it comes to the Mean Absolute Percentage Error (MAPE) value,
#a lower value indicates better forecasting accuracy.
#Therefore, a model with a lower MAPE is generally considered more suitable.

#MAPEValma of Classical Decomposition Model = 5.157174
#MAPEValma of Exponential Smoothing Model = 2.877911


#So,Exponential Smoothing Model is more suitable as it shows a lower MAPE.