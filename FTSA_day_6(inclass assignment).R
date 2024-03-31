#HAYB Hettiarachchi
#D/DBA/21/0007
#CM3023- Financial Time Series Analysis
#In Class Activity
#02.08.2023

#Loading the library'fpp'
library(fpp)

#Use 'elec' dataset
data("elec")

#as my index no is 0007
#last digit of my index no = 7
index <- 07
last_digit <- index %% 10
index
last_digit

#taking the range (351-400) corresponding to 7
start_bound <- (last_digit * 50) + 1
end_bound <- (last_digit + 1) * 50
if (last_digit == 7) {
  end_bound <- 400 
}

#Use window fuction to extract data 
time_series <- window(ts(elec), start = start_bound, end = end_bound)
time_series

#Partition the dataset into training and testing data
#last 6 digits as testing set
trainingSet = window(ts(elec),351,394)
testingSet = window(ts(elec),395,400)

plot(trainingSet,main = "Electricity Production in Australia", ylab = "Million kilowatt-hours", col = "red")

#Plot the sample correlogram
acf(trainingSet, main = "Sample Autocorrelation Function (ACF)")
#The graph has a  trend and also it has a seasonal cycle of lag 12.
#So this is not a stationary one.

#Plotting the sample correlogram of first-order differences at lag 1
diff_trainingSet <- diff(trainingSet,1)
acf(diff_trainingSet,main = "Sample ACF of First Order Differences")
#The graph does not depict any trend or any seasonal cycles.So this is a stationary one.

#Plot the sample correlogram of first-order seasonal differences
diff_seasonal_trainingSet <- diff(trainingSet, lag = 12)
acf(diff_seasonal_trainingSet, main = "Sample ACF of First Order Seasonal Differences")
#the mean and variance remains constantovertime and ACF decay to zero.
#The graph does not depict any trend or any seasonal cycles.So this is a stationary one.

#Obtain a stationary series by applying differencing appropriately
W <- diff_seasonal_trainingSet

#Applying a suitable statistical test to test whether W is a white noise process
#use the Ljung-Box test
l_junk <- Box.test(W, lag = 20, type = "Ljung")
l_junk
p_value <- l_junk$p.value
p_value 
alpha <- 0.05

cat("Ljung-Box Test:")
cat("\nHypothesis: The series is a white noise process.")
cat("\nP-value:", p_value)
if (p_value < alpha) {
  cat("\nDecision: Reject the null hypothesis. The series is not a white noise process.")
} else {
  cat("\nDecision: Fail to reject the null hypothesis. The series is a white noise process.")
}

#Fit a classical time series model using 'stl' function and obtain the fitted values
model <- stl(trainingSet, s.window = "periodic")
fitted_values <- seasadj(model)

#Find MAPE value
MAPE <- mean(abs((fitted_values - trainingSet) / trainingSet)) * 100

#Plot the actual values and fitted values
matplot(cbind(trainingSet, fitted_values), type = "l", col = c("blue", "red"), lty = 1,
        main = "Actual vs. Fitted Values", ylab = "Million kilowatt-hours",
        legend = c("Actual", "Fitted"))

