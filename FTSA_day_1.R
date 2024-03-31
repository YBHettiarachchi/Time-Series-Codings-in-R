install.packages("fpp")
library(fpp)

?wmurders
plot(wmurders)

#ts(wmurders): This part of the code converts the wmurders data 
#into a time series object

#25 and 55 are parameters provided to the window function. These parameters
#specify the start and end points of the window.
After1975 = window(ts(wmurders),25,55)

#So, the code is creating a window that spans from time point 25 to time point 55 in the wmurders time series data. 
#This operation likely extracts a subset of the data within this time range for further analysis.

After1975 = ts(After1975,start=1974,end=2004)

plot(After1975)

MA_5 = ma(After1975,5)
#The result of this code will be a new time series stored in the MA_5 variable, 
#where each data point is the moving average of the original After1975 time series data over a window of 5 points.

plot(MA_5)
