library(DescTools)
library(readr)                                                                          # To read CSV files
library(moments)                                                                        # To use statistical functions
library(nortest)                                                                        # To use Anderson-Darling test
library(ggplot2)                                                                        # To use ggplot
library(tseries)                                                                        # To use Augmented Dickey-Fuller test
library(rugarch)                                                                        # Used in modelling
library(fGarch)                                                                         # Used in modelling
library(FinTS)                                                                          # Used in ARCH test
library(dynlm)                                                                          # To add lags in the model
library(vars)                                                                           # To use VAR
library(nlWaldTest)                                                                     # To use non-linear Wald test
library(lmtest)                                                                         # Used in BP test
library(broom)
library(car)
library(sandwich)
library(knitr)
library(forecast)
library(tsbox)
library(stats)
library(zoo)
library(vrtest)
library(FinTS)
library(strucchange)
library(tidyverse)
library(lubridate)
library(changepoint)                                                                    # Used for structural breaks detection
library(stats)

data   <- read_csv("train.csv")
head(data)

# Define the `stock_id` you want to extract
i <- 0  # Replace this number with the desired `stock_id`
# Extract the subset of the data frame for the specified `stock_id`
filtered_data <- subset(data, stock_id == i, select = c(time_id, target))
head(filtered_data)

filtered_data$time_id <- as.numeric(as.character(filtered_data$time_id))
filtered_data$target <- as.numeric(as.character(filtered_data$target))

plot(filtered_data$time_id,filtered_data$target,type = "l", col = "blue", lwd = 2)

# Descriptive statistics (doesn't work)
Summary1  <- summary(filtered_data)
Summary1
Std_Dev1  <- sd(filtered_data$target) 
Std_Dev1
Skewness <- skewness(filtered_data$target)
Skewness
Kurtosis <- kurtosis(filtered_data$target) 
Kurtosis

#Gaussianity test :(
ad_test      <- ad.test(filtered_data$target)
cat("Anderson-Darling test:",   ad_test$p.value,      "\n")
#rejecting the gaussianity assumption at any reasonable confidence level

qqnorm(filtered_data$target)                                                                               
qqline(filtered_data$target)


if (!requireNamespace("tseries", quietly = TRUE)) {
  install.packages("tseries")
}
library(tseries)

# Stationarity (unit root perspective)
data_clean <- na.omit(filtered_data$target)
length(filtered_data$target)
adf_test_result <- adf.test(filtered_data$target)
adf_test_result 
#p value lower than 1%, we reject the stationarity assumption 

# Non-null average test
zm_test <- t.test(filtered_data$target, mu = 0);                                                          
cat("Zero mean test:",zm_test$p.value,"\n")
#We reject the null mean hypothesis

# ACF and PACF
acf.X  <- acf( filtered_data$target, main = "ACF ", lag.max = 30)# ACF (autocorrolation function) plot of the log-returns
pacf.X <- pacf(filtered_data$target, main = "PACF", lag.max = 30)# Partial ACF plot of the log-returns

# ACF and PACF
acf.X  <- acf( filtered_data$target^2, main = "ACF", lag.max = 30)                                         
pacf.X <- pacf(filtered_data$target^2, main = "PACF", lag.max = 30)    

# ARCH effect test
archTest <- ArchTest(filtered_data$target, lags = 1, demean = TRUE)                                                                           
archTest  
#Accept H0 --> No ARCH effect (pvalue of 0.7835). Here we are in case of conditional homoscedasticity


### GARCH(1,1) ###
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0)),                         
                        variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),   
                        distribution.model = 'norm');
sgarch.fit = ugarchfit(data = as.array(filtered_data$target), spec = spec);                                
sgarch.fit

#########################################################
if (!requireNamespace("rugarch", quietly = TRUE)) {
  install.packages("rugarch")
}
library(rugarch)

# Adjusting the GARCH model to the data
fit <- ugarchfit(spec = spec, data = filtered_data$target)

#Extracting the predictions of the conditional volatility
volatility_pred <- sigma(fit)
#Modifications to have a time interval similar to our train data set
volatility_pred_df <- as.data.frame(volatility_pred)
head(volatility_pred_df)
num_rows <- nrow(volatility_pred)
num_rows
num_columns <- ncol(volatility_pred)
num_columns
volatility_pred_df <- data.frame(
  date = rownames(volatility_pred_df),
  volatility = as.numeric(volatility_pred_df$V1)
)
head(volatility_pred_df)
volatility_pred_df$date = filtered_data$time_id
head(volatility_pred_df)

#Ploting the predictions of the volatility
plot(volatility_pred_df, type = "l", col = "blue", lwd = 2,
     main = "Prediction of volatility with GARCH(1,1)",
     xlab = "Time", ylab = "Conditional Volatility")

#Then let's do the same for the other models

