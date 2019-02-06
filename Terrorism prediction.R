library(dplyr)
library(tidyr)
install.packages("tseries")
library(ggplot2)
library(zoo)
library(forecast)
library(tseries)
library(knitr)


df1 <- read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/US Terrorism.csv")
df1 <- subset(df1, select = c(iyear, country_txt))

# create time series
df <- df1 %>% count(iyear) %>%
  filter(iyear >=1994) 

df <- zoo(df$n, order.by = df$iyear)

# Predictions 2014-2015 and Evaluation metrics
ARIMA <- accuracy(f = predict(auto.arima(df[1:20]), 
                              n.ahead = 5)$pred, x = df[21:23])


# Selection of the most accurate function
index <- c("ARIMA" = min(abs(ARIMA)))
index <- names(index[index == min(index)])

# Fitted and Predicted values
ARIMA <- auto.arima(df, stationary = F, seasonal = F)$resid + as.vector(df)


pred_vls <- data.frame("ARIMA" = c(ARIMA, predict(auto.arima(df, stationary = F, 
                                                             seasonal = F), 
                                                  n.ahead = 5)$pred),
                       "Real data" = c(as.vector(df), NA, NA),
                       "Year" = c(1994:2021))

# Predicted future terrorist activity
pred_vls <- gather(pred_vls, method, value, - Year)
pred_vls <- filter(pred_vls, method == index | method == "Real.data")
pred_vls$method <- as.factor(pred_vls$method)
levels(pred_vls$method) <- c("ARIMA", "Real data")


#### Plot "Predicted terrorist activity in the world" ####
ggplot(pred_vls, 
       aes(x = Year, y = value, colour = method))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  ggtitle("Predicted terrorist activity in the United States")+
  labs(x = "Year",
       y = "Number of Attacks",
       colour = " ")+
  scale_x_continuous(breaks = c(1993:2025))+
  theme(legend.position = "right")

acf(ARIMA)
pacf(ARIMA)
