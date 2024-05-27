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

#Reading the data
data   <- read_csv("stock_id_0.csv")
head(data)

#separating indexes 1 and indexes 2 because I still don't understand why we have 2 indexes
data_1 <- data[, c("time_id", "bid_price1", "bid_size1", "ask_price1", "ask_size1")]
data_1 <- data.frame(
  time_id = data$time_id,
  bid_price1 = sprintf("%.6f", data$bid_price1),
  bid_size1 = sprintf("%.6f", data$bid_size1),
  ask_price1 = sprintf("%.6f", data$ask_price1),
  ask_size1 = sprintf("%.6f", data$ask_size1)
)
head(data_1)

data_2 <- data[, c("time_id", "bid_price2", "bid_size2", "ask_price2", "ask_size2")]
data_2 <- data.frame(
  time_id = data$time_id,
  bid_price2 = sprintf("%.6f", data$bid_price2),
  bid_size2 = sprintf("%.6f", data$bid_size2),
  ask_price2 = sprintf("%.6f", data$ask_price2),
  ask_size2 = sprintf("%.6f", data$ask_size2)
)
head(data_2)

N <- nrow(data)
N

#Converting non numerical values into numerical values otherwise it won't work
data_1$bid_price1 <- as.numeric(as.character(data_1$bid_price1))
data_1$ask_price1 <- as.numeric(as.character(data_1$ask_price1))
data_1$bid_size1 <- as.numeric(as.character(data_1$bid_size1))
data_1$ask_size1 <- as.numeric(as.character(data_1$ask_size1))

#midprices and logreturns
data_1$mid_price1 <- (data_1$bid_price1*data_1$bid_size1 + data_1$ask_price1*data_1$ask_size1) / (data_1$bid_size1+data_1$ask_size1)
data_1$log_returns_1 <- c(NA, diff(log(data_1$mid_price)))
head(data_1,100)

#same
data_2$bid_price2 <- as.numeric(as.character(data_2$bid_price2))
data_2$ask_price2 <- as.numeric(as.character(data_2$ask_price2))
data_2$bid_size2 <- as.numeric(as.character(data_2$bid_size2))
data_2$ask_size2 <- as.numeric(as.character(data_2$ask_size2))

data_2$mid_price2 <- (data_2$bid_price2*data_2$bid_size2 + data_2$ask_price2*data_2$ask_size2) / (data_2$bid_size2+data_2$ask_size2)
data_2$log_returns_2 <- c(NA, diff(log(data_2$mid_price)))
head(data_2,100)

#ploting the midprices and the logreturns
plot(data_1$time_id,data_1$mid_price1)                                                 
plot(data_1$time_id,data_1$log_returns_1)  

plot(data_2$time_id,data_2$mid_price2)                                                 
plot(data_2$time_id,data_2$log_returns_2)

data_clean <- na.omit(data_1$log_returns_1)
# Descriptive statistics (doesn't work)
Summary1  <- summary(data_1)
Summary1
Std_Dev1  <- sd(data_clean) 
Std_Dev1
Skewness <- skewness(data_clean)
Skewness
Kurtosis <- kurtosis(data_clean) 
Kurtosis

#Gaussianity test :(
ad_test      <- ad.test(data_clean)
cat("Anderson-Darling test:",   ad_test$p.value,      "\n")
#rejecting the gaussianity assumption at any reasonable confidence level
                                                   
qqnorm(data_1$log_returns_1)                                                                               
qqline(data_1$log_returns_1)


# Stationarity (unit root perspective)
data_clean <- na.omit(data_1$log_returns_1)
adf_test_result <- adf.test(data_clean)
cat("Augmented Dickey-Fuller test:", adf_test$p.value,     "\n") 
#p value of 1%, we reject the stationarity assumption at 99% confidence level

# Non-null average test
zm_test <- t.test(data_1$log_returns_1, mu = 0);                                                          
cat("Zero mean test:",zm_test$p.value,"\n")
#We accept that we have a null mean at 99% confidence level

# ACF and PACF
acf.X  <- acf( data_clean, main = "ACF ", lag.max = 30)# ACF (autocorrolation function) plot of the log-returns
pacf.X <- pacf(data_clean, main = "PACF", lag.max = 30)# Partial ACF plot of the log-returns

# ACF and PACF
acf.X  <- acf( data_clean^2, main = "ACF", lag.max = 30)                                         
pacf.X <- pacf(data_clean^2, main = "PACF", lag.max = 30)    

# ARCH effect test
archTest <- ArchTest(data_clean, lags = 1, demean = TRUE)                                                                           
archTest  
#Accept H0 --> No ARCH effect (pvalue of 0.6378). Here we are in case of conditional homoscedasticity


#############################################
### Model fitting & residuals diagnostics ###
#############################################

### GARCH(1,1) ###
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0)),                         
                        variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),   
                        distribution.model = 'norm');
sgarch.fit = ugarchfit(data = as.array(data_clean), spec = spec);                                
sgarch.fit

resid <- residuals(sgarch.fit)                                                          # Extract the residuals
std_resid <-  resid / sigma(sgarch.fit)                                                 # Standardized 
mean(std_resid)                                                                         # Mean
var(std_resid)                                                                          # Variance 
skewness(std_resid)                                                                     # Skewness
kurtosis(std_resid)                                                                     # Kurtosis 
jarque.bera.test(std_resid) 
# reject H0 --> standardized residuals are not normally distributed 

# QQ-plot (to highlight the presence of fat tails)
qqnorm(std_resid)
qqline(std_resid)

Box.test(std_resid, type = "Ljung-Box")                                                 # Ljung-Box test
# reject H0 --> there is residual autocorrelation 

df <- data.frame(x = std_resid) 
ggplot(df, aes(x = x)) +                                                               
  geom_density(fill = "blue", alpha = 0.2) +                                            
  stat_function(fun = dnorm, args = list(mean = 0, 
                                         sd = 1), color = "red") +                 
  labs(title = "GARCH(1,1) std resid vs std normal", x = "", y = "Density", 
       color = "Distribution") +                                                       
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical") )  +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.20),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
        axis.line = element_line(color = "lightgrey"))                        # Set colors



### GARCH-M(1,1) ### (GARCH in Mean, to find the presence of a risk premium)
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0),archm=TRUE,archpow=1),     
                        variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                        distribution.model = 'norm')
sgarchM.fit = ugarchfit(data = as.array(data_clean), spec = spec)                                
sgarchM.fit

resid     <- residuals(sgarchM.fit)                                                     # Extract the residuals
std_resid <-  resid / sigma(sgarchM.fit)                                                # Standardized 
mean(std_resid)                                                                         # Mean
var(std_resid)                                                                          # Variance 
skewness(std_resid)                                                                     # Skewness
kurtosis(std_resid)                                                                     # Kurtosis 
jarque.bera.test(std_resid) 
# reject H0 --> standardized residuals are not normally distributed 

# QQ-plot (to highlight the presence of fat tails)
qqnorm(std_resid)                                                                              
qqline(std_resid) 

# Ljung-Box test 
Box.test(std_resid, type = "Ljung-Box")                                                 # Ljung-Box test
# accept H0 --> there is not residual autocorrelation 

df <- data.frame(x = std_resid) 
ggplot(df, aes(x = x)) +                                                               
  geom_density(fill = "blue", alpha = 0.2) +                                            
  stat_function(fun = dnorm, args = list(mean = 0, 
                                         sd = 1), color = "red") +                 
  labs(title = "GARCH-M(1,1) std resid vs std normal", x = "", y = "Density", 
       color = "Distribution") +                                                       
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical") )  +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.20),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
        axis.line = element_line(color = "lightgrey"))                        # Set colors



### eGARCH(1,1) ###
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0)),                          # eGARCH(1,1) specifications
                        variance.model = list(model = 'eGARCH', garchOrder = c(1,1)),
                        distribution.model = 'norm')
egarch.fit = ugarchfit(data = as.array(data_clean), spec = spec)                                 # eGARCH(1,1) fitting
egarch.fit

# Diagnostic 
resid     <- residuals(egarch.fit)                                                       # Extract the residuals
std_resid <-  resid / sigma(egarch.fit)                                                  # Standardized 
mean(std_resid);                                                                         # Mean
var(std_resid);                                                                          # Variance 
skewness(std_resid);                                                                     # Skewness
kurtosis(std_resid);                                                                     # Kurtosis 
jarque.bera.test(std_resid) 
# reject H0 --> standardized residuals are not normally distributed 

# QQ-plot (to highlight the presence of fat tails)
qqnorm(std_resid)                                                                              
qqline(std_resid) 

df <- data.frame(x = std_resid) 
ggplot(df, aes(x = x)) +                                                               
  geom_density(fill = "blue", alpha = 0.2) +                                            
  stat_function(fun = dnorm, args = list(mean = 0, 
                                         sd = 1), color = "red") +                 
  labs(title = "eGARCH(1,1) std resid vs std normal", x = "", y = "Density", 
       color = "Distribution") +                                                       
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical"))  +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.20),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
        axis.line = element_line(color = "lightgrey"))                        # Set colors

### gjrGARCH(1,1) ###
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0)),                          # gjrGARCH(1,1) specifications
                        variance.model = list(model = 'gjrGARCH', garchOrder = c(1,1)),
                        distribution.model = 'norm')
gjrgarch.fit = ugarchfit(data = as.array(X), spec = spec)                               # gjrGARCH(1,1) fitting
gjrgarch.fit

# Diagnostic 
resid <- residuals(gjrgarch.fit)                                                        # Extract the residuals
std_resid <-  resid / sigma(gjrgarch.fit)                                               # Standardized 
mean(std_resid)                                                                         # Mean
var(std_resid)                                                                          # Variance 
skewness(std_resid)                                                                     # Skewness
kurtosis(std_resid)                                                                     # Kurtosis 
jarque.bera.test(std_resid) 
# reject H0 --> standardized residuals are not normally distributed 

# QQ-plot (to highlight the presence of fat tails)
qqnorm(std_resid)                                                                              
qqline(std_resid) 

# Distribution plot vs standard normal (same procedure as above for the distribution plot)
df <- data.frame(x = std_resid) 
ggplot(df, aes(x = x)) +                                                               
  geom_density(fill = "blue", alpha = 0.2) +                                            
  stat_function(fun = dnorm, args = list(mean = 0, 
                                         sd = 1), color = "red") +                 
  labs(title = "gjrGARCH(1,1) std resid vs std normal", x = "", y = "Density", 
       color = "Distribution") +                                                       
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical")) +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.20),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
        axis.line = element_line(color = "lightgrey"))                        # Set colors


# Ljung-Box test 
Box.test(std_resid, type = "Ljung-Box")    