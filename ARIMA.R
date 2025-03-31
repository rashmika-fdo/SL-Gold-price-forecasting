#Time Series Modeling and Forecasting of Gold Prices During Sri Lanka’s Economic Crisis

library(tidyverse)
library(scales)

#Importing gold daily dataset
gold <- read.csv("data.csv")

gold$Date <- as.Date(gold$Date, format = "%Y-%m-%d")
gold <- gold[order(gold$Date), ]

#Plotting gold dataset
ggplot(gold, aes(Date, Exchange.Rate)) +
  geom_line() +
  scale_x_date("Date") +
  scale_y_continuous(labels = label_comma()) +  # Format y-axis labels
  ylab("Gold Ounce Price (LKR)") +
  xlab("")


#Filtering gold data set from 2022Apr to 2024Nov
gold1 <- gold %>%
  filter(Date >= as.Date("2022-04-01") & Date <= as.Date("2024-11-30"))

gold <- gold1[,-1]
names(gold)[2] <- "Price"
head(gold)

str(gold)

#Plotting gold dataset from 2022Apr to 2024Nov
ggplot(gold, aes(Date, Price)) +
  geom_line() +
  scale_x_date("Date") +
  scale_y_continuous(labels = label_comma()) +  # Format y-axis labels
  ylab("Gold Ounce Price (LKR)") +
  xlab("")


#Creating a time series object for gold data
gold_ts <- ts(gold$Price, 
              start = c(2022, 91),  # 2022-04-01 is the 91st day of 2022
              frequency = 240)

plot(gold_ts, main = "Gold Prices (2022-04-01 to 2024-11-30)", ylab = "Gold Price", xlab = "Time")


#ACF and PACF plots for original data
acf(as.vector(gold_ts), main="ACF plot for gold data")
pacf(as.vector(gold_ts), main="PACF plot for gold data")


#Check for stationarity
library(tseries)
adf.test(gold_ts) #Non-stationary

kpss.test(gold_ts) #Non-stationary



#Differencing is done 
diff_gold <- diff(gold_ts)

adf.test(diff_gold) #Stationary
kpss.test(diff_gold) #Stationary

#ACF and PACF plots for differenced series
acf(as.vector(diff_gold), main="ACF plot for the differenced series")
pacf(as.vector(diff_gold), main="PACF plot for the differenced series")

plot(diff_gold)

###############################################################################

# Load necessary libraries
library(forecast) # For ARIMA modeling
library(MuMIn)    # For AICc computation

# Initialize variables to store model information
results <- data.frame(
  p = integer(),
  q = integer(), 
  AIC = numeric(), 
  BIC = numeric(), 
  AICc = numeric()
)

# Loop through all combinations of ARIMA(p,1,q) where p and q range from 0 to 5
for (p in 0:5) {
  for (q in 0:5) {
    # Try fitting the ARIMA model
    try({
      model <- arima(diff_gold, order = c(p, 1, q))
      
      # Compute AIC, BIC, and AICc
      model_aic <- AIC(model) # AIC is standard
      model_bic <- BIC(model) # Manually calculate BIC
      model_aicc <- AICc(model) # Requires MuMIn
      
      # Store results
      results <- rbind(results, data.frame(
        p = p, 
        q = q, 
        AIC = model_aic, 
        BIC = model_bic, 
        AICc = model_aicc
      ))
      
      # Print model summary (optional)
      print(paste("Model (", p, ",1,", q, "):", sep = ""))
      print(summary(model))
    }, silent = TRUE) # Skip any models that throw errors
  }
}

# View the results table
print(results)


# Find the best models based on AIC, BIC, or AICc
best_aic <- results[which.min(results$AIC), ]
best_bic <- results[which.min(results$BIC), ]
best_aicc <- results[which.min(results$AICc), ]


# Print the best models

cat("\nBest Model based on AIC:\n")
print(best_aic)

cat("\nBest Model based on BIC:\n")
print(best_bic)

cat("\nBest Model based on AICc:\n")
print(best_aicc)

# Fit the best ARIMA model (0,1,1)
best_model <- arima(gold_ts, order = c(0, 1, 1))


#checking the assumptions for the fitted model residuals. 
residuals_model <- residuals(best_model)


#Normal plot for the residuals
qqnorm(residuals_model)
qqline(residuals_model)


library("lmtest")

#Histogram for the residuals
hist(residuals_model)

mean(residuals_model)


#ACF and PACF plot for the residuals of the fitted model
acf(as.vector(residuals_model))
pacf(as.vector(residuals_model))


# Perform Ljung-Box test
# Testing for no autocorrelation up to lag 10
Box.test(residuals_model, lag = 10, type = "Ljung-Box")


#Checking ARCH effect
library(FinTS) 
arch_test <- ArchTest(residuals_model, lags = 5)
print(arch_test) 


#################################################################
#Forecasting the ARIMA model (0,1,1)
# Forecast the next 20 days

forecast_val <- forecast(best_model, h = 20)
forecast_val



library(ggplot2)
library(lubridate)


# Generate weekday dates for December 2024
start_date <- as.Date("2024-12-02")  # First weekday in December 2024
end_date <- as.Date("2024-12-31")    # Last day of December 2024

# Create a sequence of all dates in December
all_dates <- seq(start_date, end_date, by = "days")

# Filter only weekdays (exclude Saturdays and Sundays)
weekdays_only <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]

# Select the first 20 weekdays
forecast_dates <- weekdays_only[1:20]

#Confidence bounds
upper_original <- forecast_val$upper[,2]
lower_original <- forecast_val$lower[,2]

# Combine into a data frame for plotting
forecast_df <- data.frame(
  Date = forecast_dates,                 # Correct weekday dates
  Forecast = forecast_val$mean,          # Original scale forecast
  Lower = lower_original,                # Lower confidence interval
  Upper = upper_original                 # Upper confidence interval
)



#Filtering the 2024 gold data prices and storing in a dataframe gold_2024
gold <- read.csv("data.csv")
gold$Date <- as.Date(gold$Date, format = "%Y-%m-%d")
gold <- gold[order(gold$Date), ]
gold_24 <- gold %>%
  filter(Date >= as.Date("2024-01-01") & Date <= as.Date("2024-11-30"))

gold_2024 <- gold_24[,-1]
names(gold_2024)[2] <- "Price"


# Combining the original data to forecast data
combined_df <- rbind(
  data.frame(Date = gold_2024$Date, Price = gold_2024$Price, Type = "Actual"),
  data.frame(Date = forecast_df$Date, Price = forecast_df$Forecast, Type = "Forecast")
)


# Plotting the original & forecast with Confidence bounds.
ggplot(combined_df, aes(x = Date, y = Price, color = Type)) +
  geom_line(size = 1) +  # Line for both actual and forecast
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower, ymax = Upper), 
              fill = "lightblue", alpha = 0.5, inherit.aes = FALSE) +  # Confidence interval
  labs(title = "Gold Price with Forecast",
       x = "Date (2024)",
       y = "Gold Price (LKR)") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue"))


#Actual gold prices and the forecasted values.
print(combined_df)



######################################################################
#########################################################################
######################################################################

