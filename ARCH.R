library(tidyverse)
#Importing gold daily dataset
gold <- read.csv("data.csv")

gold$Date <- as.Date(gold$Date, format = "%Y-%m-%d")
gold <- gold[order(gold$Date), ]

#Filtering gold data set from 2022Apr to 2024Nov
gold1 <- gold %>%
  filter(Date >= as.Date("2022-04-01") & Date <= as.Date("2024-11-30"))

gold <- gold1[,-1]
names(gold)[2] <- "Price"

gold_ts <- ts(gold$Price, 
              start = c(2022, 91),  # 2022-04-01 is the 91st day of 2022
              frequency = 240)

#First differencing is done to make the original data stationary
diff_gold <- diff(gold_ts)

######################################################################
#ARCH(1) model
install.packages("fGarch")
library(fGarch)


arch.fit <- garchFit(~garch(1,0), data=diff_gold , trace=F)


summary(arch.fit)




###################################################################
#ARCH(2) model

arch_fit2 <- garchFit(~ garch(2, 0), data = diff_gold, trace = F)
summary(arch_fit2)

#Using rugarch package
library(rugarch)

arch2_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2,0)),
  mean.model = list(armaOrder = c(0, 0)),  
  distribution.model = "norm"  # Assuming normal distribution of errors
)

arch2_fit <- ugarchfit(spec=arch2_spec,data=diff_gold)
arch2_fit


# Forecast 21 steps ahead
forecast_arch2 <- ugarchforecast(arch2_fit,n.ahead=21)
forecast_arch2

plot(forecast_arch2)
plot(fitted(forecast_arch2))
plot(sigma(forecast_arch2))

sigma(forecast_arch2)
fitted(forecast_arch2)


# Generate predictions by forecasting future gold prices
sim <- ugarchsim(arch2_fit, n.sim = 21)

simulated_returns <- fitted(sim)

#LAst value of gold price
last <- tail(gold$Price,1)

#simulating to get forecasted gold prices
simulated_prices <- apply(simulated_returns, 2, function(x) {
  cumsum(x) + last  
})

  
simulated_prices

forecasted_prices <- as.vector(simulated_prices[, 1])
print(forecasted_prices)

view(forecasted_prices)


#Plotting the forecast

#Filtering the 2024 gold data prices and storing in a dataframe gold_2024
gold <- read.csv("data.csv")
gold$Date <- as.Date(gold$Date, format = "%Y-%m-%d")
gold <- gold[order(gold$Date), ]
gold_24 <- gold %>%
  filter(Date >= as.Date("2024-01-01") & Date <= as.Date("2024-11-30"))

gold_2024 <- gold_24[,-1]
names(gold_2024)[2] <- "Price"

#Plotting the forecast
library(ggplot2)
# Generate weekday dates for December 2024
start_date <- as.Date("2024-12-02")  # First weekday in December 2024
end_date <- as.Date("2024-12-31")    # Last day of December 2024

# Create a sequence of all dates in December
all_dates <- seq(start_date, end_date, by = "days")

# Filter only weekdays (exclude Saturdays and Sundays)
weekdays_only <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]

# Select the first 20 weekdays
forecast_dates <- weekdays_only[1:21]

#########################

forecast_df <- data.frame(Date = forecast_dates, Price = forecasted_prices)

gold_2024$Type <- "Actual"
forecast_df$Type <- "Forecast"

combined_df <- rbind(gold_2024, forecast_df)


# Plot using ggplot2
ggplot(combined_df, aes(x = Date, y = Price, color = Type)) +
  geom_line(size = 1) +  # Line plot
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  labs(
    title = "Gold Prices: Actual (Jan-Nov 2024) vs Forecasted (Dec 2024)",
    x = "Date",
    y = "Gold Price",
    color = "Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )


#################################################################














