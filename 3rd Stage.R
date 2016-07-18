# Now we are moving to next stage of the task
# Here the goal is convert this code into a function
# so that we can apply the function for other columns in the data
# In this case once we convert this code written for "Units sold" to be 
# generic code that can be applied for "revenue" as well 
# we will call the function with "units sold " and then with "revenue"

# reading data set
# make sure you setup the right directory where the file is by using setwd("Path")

input_data <- read.csv("input.csv", header= TRUE)

head(input_data)

library("forecast")
library("DBI")
library("RPostgreSQL")
library("lubridate")

# Development of function is as simple just replace "units_sold" with 'target'
# put the entire code with in the curley bracket 
# and create function named target_value




target_value  <- function(target){

# taking distinct Products from the data set as we have to run the forecast for each product
 
Products <- unique(input_data["Product"][,])

# study this for loop. there are two inner curley brackets 

for (i in Products) {
  
  # please note the remaining code is same as before , except the way subset the data set is diffent here
  
  train <- head(input_data[target][input_data["Product"]==i] , 90)
  
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
  
  write.csv(forecastoutput, file=paste0('output', i, target,  '.csv'))

# I have added target to make the file name different    
# If every time you run this if you want the history to be recorded, what would you do
# Clue: add the time stamp to the filename in the write.csv statement!  
    
  }

}


# now we are calling the function two times as shown below. 
# check the files are stored in your working directory 
 
target_value("Units_sold")
target_value("Revenue")
