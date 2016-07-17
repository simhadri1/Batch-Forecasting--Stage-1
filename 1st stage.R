# This is a very straigt forward code with very basic things however it offers several learnings
# we are taking one product and developing a forecast
# reading, subsetting some arthemetic operations
# using functions like head, tail, dim , get, paste , which , min
# selecting specific columns from output of data
# using functions from forecast package
# Ploting the output.
# there are no bells and whistles in this code. This is plain vanilla code. 


#reading data set
input_data <- read.csv("input.csv", header= TRUE)

head(input_data)

dim(input_data)

#subset data and take only milk

input_milk = subset(input_data, Product == 'Milk', select = Units_sold)

head(input_milk)

dim(input_milk) 

library("forecast")
library("DBI")
library("RPostgreSQL")
library("lubridate")

#  devide it into train and test 

train <- head (input_milk$Units_sold , 0.7*length(input_milk$Units_sold ))
test <- tail(input_milk$Units_sold , 0.3*length(input_milk$Units_sold)) 

# converting the test train to time series data using function ts from forecast package
train     <- ts(train[1:(length(train))],frequency = 7)
test      <- ts(test[1:length(test)], frequency=7)

# here I am fitting two models, you may use more models  instead from forecast package 

fc1 = auto.arima(train)  # forecast 1 
pred1 = forecast( fc1) # forecast is from forecast package
fit1_acry   = accuracy(pred1) #   accuracy from forecast package see page number 10


fc2 = HoltWinters (train)   # forecast 2
pred2 = forecast( fc2 )
fit2_acry = accuracy(pred2 ) # fit1 accuracy 

# see how we are taking data from multiple outputs and then creating a saparate data frame

MAPE <- data.frame ( fit1_MAPE = fit1_acry[,'MAPE'], 
                    fit2_MAPE = fit2_acry[,'MAPE']
                    )
# I am taking best among them, here best is minimum of the MAPE

best <-  which.min(MAPE)

# see how I am using the paste function and get function. 

BestModel = get(paste('fc',best,sep="")) 

# we can convert the output into a list by saying : BestModel = list(get(paste('fc',best,sep=""))) 


forecastoutput <- data.frame(forecast(BestModel, h=8  )) # creating the forecasts for next 8 peroids

print(forecastoutput)

#plotting specific column from the output 

plot(forecastoutput$Point.Forecast ,type = "l" )

write.csv(forecastoutput,"outputmilk.csv")

# Now that you learned this , next step is to use this and 
# Apply this for multiple products ( using for loops etc)
# and see how the code gets transformed into. 
# you might ask me why I did not use the test data. I just wanted to keep it 
#simple you can try that. Again this is for the purpose of understanding the coding and scal










 
