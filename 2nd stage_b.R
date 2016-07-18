# Now we are moving to next stage of the task
# Here the goal is to come up with forecasts for multiple Products 
# We have to use a for loop to accomplish that task
# I am removing several comments from previous section as
# you are already aware of them. Keeping only those that are incremental to this section


#reading data set
# make sure you setup the right directory where the file is by using setwd("Path")
#setwd("C:/Users/Data Science/Documents/R/R forecast cable/process and sample/batchforecasting")

input_data <- read.csv("input.csv", header= TRUE)

head(input_data)

library("forecast")
library("DBI")
library("RPostgreSQL")
library("lubridate")

# taking distinct Products from the data set as we have to run the forecast for each product

Products <- unique(input_data["Product"][,])

# study this for loop. there are two inner curley brackets 

output           <- matrix(0,nrow=(length(Products)*(1)),ncol=7)
# we are using *1 because we want 1 forecasts for each Product.. 
#this will not work if we use more than one forecast. 

# Giving the names to the columns of the matrix

colnames(output) <-
  
  c(
    "Product",
    "Date",
    "Forecast",
    "Lo_80",
    "Hi_80 ",
    "Lo_95",
    "Hi_95"
  )


# Now see the magic of this loop. We simply kept the code inside the loop with small changes.

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
  
  forecastoutput <- data.frame(forecast(BestModel, h=1  )) # creating the forecasts for next 8 peroids
  
  # we wanted to give date also as variable.
  
  forecast_date <- tail(input_data["date"][input_data["Product"]==i],1)
  # well I leave it to you if you want to give the date as next day, write that formula
  
  # defining the column and row where the value should go and fit here. 
  row_index = which(Products==i)
  
  output[row_index,1]   <- i
  output[row_index,2]   <- forecast_date
  output[row_index,3]   <- (round(forecastoutput$Point.Forecast,2))
  output[row_index,4]   <- as.numeric(round(forecastoutput$Lo.80,2))
  output[row_index,5]   <- as.numeric(round(forecastoutput$Hi.80,2))
  output[row_index,6]   <- as.numeric(round(forecastoutput$Lo.95,2))
  output[row_index,7]   <- as.numeric(round(forecastoutput$Hi.95,2))
  
# converting the matrix into the dataframe.
  
  output_onestep <- data.frame(output)
  
  write.csv(output_onestep , "output_2a.csv")
  
# write.csv(forecastoutput, file=paste0('output', i, '.csv'))
  
  
}


# Here is one more challenge for you to work on. We are using one forecast (h=1), if we use more than
# one ( h= 2 or so) then we cannot write the same way. 
# Use the below link and further research and accomplish the task
# http://stackoverflow.com/questions/25026573/how-do-i-store-for-loop-results-from-r-into-a-csv-file
# can I generate date column for these as they are daily forecast
# I will move on to Stage 3 of converting above stage2 code into a function so that you can use the function
