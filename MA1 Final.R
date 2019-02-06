library(tidyverse)

#Reads in the CSV for use.
oldf <- read.csv("C:/Users/Joseph/Documents/R/IDS 702/OldFaithful.csv", header = T)
waitModel <- lm(Interval ~ Duration , data = oldf)
summary(waitModel)
confint(waitModel, level = 0.95)
plot(Interval ~ Duration , data = oldf, xlab = "Duration", ylab = "Interval", main = "Gyser Duration at Old Faithful")
abline(waitModel)
abline(29.343441, 9.499061, col = "red")
abline(38.31297, 11.98288, col = "red")
#new intervals based on if the previous time was 4 minutes
newduration <- 4
newdata <- data.frame(Duration = newduration)
#predition data
predict.lm(waitModel, newdata, interval = "prediction")

resp <- read.csv("C:/Users/Joseph/Documents/R/IDS 702/Respiratory.csv", header = T)
#went back after looking at the residuals and played with log of each variable to get a better fitting line
resp$logRate <- log(resp$Rate)
#model after transformation
respModel = lm(Rate ~ Age , data = resp)
summary(respModel)
confint(respModel, level = 0.95)
plot(Rate~Age,data=resp, xlab = "Age in months", ylab = "Rate", main = "Respitory rate by age")
abline(respModel) #line of best fit/least squares from the prediction model
abline(46.0619711,-0.7534009, col = "red") # prediction lower error
abline(48.0423556,-0.6380259, col = "red") # predition upper error
#Transformed data for respitory rates at 1 month
newAge <- c(1)
newdata <- data.frame(Age = newAge)
#predition data
predict.lm(respModel, newdata, interval = "prediction")
#Transformed data for respitory rates at 18 months
newAge <- c(18)
newdata <- data.frame(Age = newAge)
#predition data
predict.lm(respModel, newdata, interval = "prediction")
#Transformed data for respitory rates at 29 month
newAge <- c(29)
newdata <- data.frame(Age = newAge)
#predition data
predict.lm(respModel, newdata, interval = "prediction")

elect <- read.csv("C:/Users/Joseph/Documents/R/IDS 702/Elections.csv", header = T)
#remove Palm Beach County for the initial model
elect <- elect[!(elect$County == "Palm Beach"),]
#went back after the initial runs to build a better model
elect$Bush2000 <- log(elect$Bush2000)
elect$Buchanan2000 <- log(elect$Buchanan2000)
#election model
ElectModel <- lm(Buchanan2000 ~ Bush2000 , data = elect)
plot(x = elect$Bush2000, y = elect$Buchanan2000, xlab = "Bush Votes", ylab = "Buchanan Votes", main = "2000 U.S. Election")
abline(ElectModel)
abline(-3.049512,0.659109,col = "red") #prediction lower error
abline(-1.633461,0.802815,col = "red") #prediction upper error
summary(ElectModel)
confint(ElectModel, level = 0.95)
#prediction for Buchanan's expected votes in Palm Beach
bushBP = log(152846)
newdata <- data.frame(Bush2000 = bushBP)
exp(predict.lm(ElectModel, newdata, interval = "prediction"))
BuchananBP <- exp(0.73096*log(152846)-2.34149)
BuchananBP