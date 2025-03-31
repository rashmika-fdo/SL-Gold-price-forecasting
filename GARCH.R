library(tidyverse)
library(ggplot2)
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
str(diff_gold)
##################################################
#GARCH(1,1) model

library(rugarch)


garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "norm"  # Assuming normal distribution of errors
)

garch_fit <- ugarchfit(spec=garch_spec,data=diff_gold)
garch_fit






