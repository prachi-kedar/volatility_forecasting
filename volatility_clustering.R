data   <- read.csv("train.csv")
head(data)

# Define the `stock_id` you want to extract
i <- 0  # Replace this number with the desired `stock_id`
# Extract the subset of the data frame for the specified `stock_id`
filtered_data <- subset(data, stock_id == i, select = c(time_id, target))
head(filtered_data)

filtered_data$time_id <- as.numeric(as.character(filtered_data$time_id))
filtered_data$target <- as.numeric(as.character(filtered_data$target))

plot(filtered_data$time_id,filtered_data$target,type = "l", col = "blue", lwd = 2)

# remove NAs
filtered_data <- na.omit(filtered_data)

# compute daily percentage changes
target_PC <- data.frame("Date" = filtered_data$time_id, 
                       "Value" = as.numeric(Delt(filtered_data$target) * 100))
target_PC <- na.omit(target_PC)
target_PC


plot(target_PC, 
     ylab = "Percent", 
     main = "Daily Percentage Changes",
     type=  "l", 
     col =  "steelblue", 
     lwd =  0.5)
abline(0, 0)

acf(target_PC$Value, main = "Wilshire 5000 Series")

library("fGarch")
# estimate GARCH(1,1) model of daily percentage changes
GARCH_Wilshire <- garchFit(data = target_PC$Value, trace = F)

# compute deviations of the percentage changes from their mean
dev_mean_target_PC <- target_PC$Value - GARCH_Wilshire@fit$coef[1]

# plot deviation of percentage changes from mean
plot(target_PC$Date, dev_mean_target_PC, 
     type = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "Estimated Bands of +- One Conditional Standard Deviation",
     cex.main=0.8,
     lwd = 0.2)

# add horizontal line at y = 0
abline(0, 0)

# add GARCH(1,1) confidence bands (one standard deviation) to the plot
lines(target_PC$Date, 
      GARCH_Wilshire@fit$coef[1] + GARCH_Wilshire@sigma.t, 
      col = "darkred", 
      lwd = 0.5)

lines(target_PC$Date, 
      GARCH_Wilshire@fit$coef[1] - GARCH_Wilshire@sigma.t, 
      col = "darkred", 
      lwd = 0.5)