# Now we are moving to next stage of the task
# Here the goal is to come up with forecasts for multiple Products 
# We have to use a for loop to accomplish that task
# I am removing several comments from previous section as
# you are already aware of them. Keeping only those that are incremental to this section


#reading data set
# make sure you setup the right directory where the file is by using setwd("Path")

input_data <- read.csv("input.csv", header= TRUE)

head(input_data)

library("forecast")
library("DBI")
library("RPostgreSQL")
library("lubridate")

# taking distinct Products from the data set as we have to run the forecast for each product

Products <- unique(input_data["Product"][,])

# study this for loop. there are two inner curley brackets 

for (i in Products) {
  
  # please note the remaining code is same as before , except the way subset the data set is diffent here
  
  train <- head(input_data["Units_sold"][input_data["Product"]==i] , 90)
  
  train     <- ts(train[1:(length(train))],frequency = 7)
  
  fc1 = auto.arima(train)  # forecast 1 
  pred1 = forecast( fc1) # forecast is from forecast package
  fit1_acry   = accuracy(pred1) #   accuracy from forecast package see page number 10
  
  
  fc2 = HoltWinters (train)   # forecast 2
  pred2 = forecast( fc2 )
  fit2_acry = accuracy(pred2 ) # fit1 accuracy 
  
  
  MAPE <- data.frame ( fit1_MAPE = fit1_acry[,'MAPE'], 
                       fit2_MAPE = fit2_acry[,'MAPE']
  )
  
  best <-  which.min(MAPE)
  
  BestModel = get(paste('fc',best,sep="")) 
  
  forecastoutput <- data.frame(forecast(BestModel, h=8  )) # creating the forecasts for next 8 peroids
  
  
  plot(forecastoutput$Point.Forecast ,type = "l" )
  
  # Here we can use same write.csv.  However, as we have multiple Products and 
  # we are creating multiple files for each product understand below line
  
  write.csv(forecastoutput, file=paste0('output', i, '.csv'))
  
  
}


# Are we good? any questions?
# Yes.. what if we have 500 products, is it good to generate 500 files ? May be not.
# Then how can I put all the data in one file and give another column named product?
# http://stackoverflow.com/questions/25026573/how-do-i-store-for-loop-results-from-r-into-a-csv-file
# can I generate date column for these as they are daily forecast
