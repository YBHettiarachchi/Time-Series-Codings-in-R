#Reg No : D/DBA/21/0007
#Name : HAYB Hettiarachchi
#Module : Financial Time Series Analysis
#Assignment 02
#Submission date : 03/10/2023

#I have set working directory as a folder containing two data files.

#loading the libraries
library(fpp)
library(forecast)

#scan the dat files in to x and y time series varibles 
x <- ts(scan("econpredictor.dat"))
y <- ts(scan("econmeasure.dat"))

#fit a linear regression model for two datasets,econmeasure and econpredictor.
model_0007 <- lm(y~x)
model_0007

plot(model_0007)
summary(model_0007)

Box.test(residuals(model_0007), lag=12,type = "Ljung-Box")
#As the residuals of the linear regression model is lesser than 0.05,(3.556e-06 < 0.05) , we do not have evidences to say that the errors are independent. 
#since errors are not independent, we have to go for the Regression model with ARIMA errors

# Calculate the residuals from the linear regression model
linear_regression_residuals <- residuals(model_0007)

# Fiting an appropriate ARIMA model to the residuals of the linear regression model using'auto.arima'function.
res_arima <- auto.arima(linear_regression_residuals)

# Get the ARIMA-model residuals
arima_residuals <- residuals(res_arima)

# Create the final model fo forecasting
final_model_0007 <- lm(y~x)

# Set the residuals to the arima residuals.
final_model_0007$residuals <- arima_residuals

summary(final_model_0007)

Box.test(residuals(final_model_0007), lag=12,type="Ljung-Box")
#Finally,the p-value of final model is 0.8544 which is greater than the significance level.(0.8544>0.55) 
#So the residials of the final model id independent.
#we can use this model for forecasting. 