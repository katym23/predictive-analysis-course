# Assignment 6 - Time Series Analysis

# load the required libraries
library(tidyverse)
library(forecast)

# load in the required data set
retail <- read.csv("retail_data.csv")
head(retail)

# convert to time series object

ts_retail <- ts(data = retail$Sales,
              start=c(2018,1),
              end=c(2023,12),
              frequency=12)

# view decomposition plot

plot(decompose(ts_retail))

# convert data to log form

retail_log <- log(ts_retail)

# train model
log_arima <-
  auto.arima(retail_log)

# predict the next 4 months
log_forecast_4 <-
  forecast(log_arima, h=4)
log_forecast_4

# predict the next 8 months
log_forecast_8 <-
  forecast(log_arima, h=8)
log_forecast_8

# predict the next year
log_forecast_12 <-
  forecast(log_arima, h=12)
log_forecast_12

# undo log transformation so true forecasted sales can be seen
print(exp(as.data.frame(log_forecast_4)))
print(exp(as.data.frame(log_forecast_8)))
print(exp(as.data.frame(log_forecast_12)))