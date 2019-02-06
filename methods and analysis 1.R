### The following is the R Script for IDS 702: Modeling and Representation of Data (Fall 2018)

# initiate the library tidyverse that allows for better readibility and manipulation of data sets

library(tidyverse)

# This assignment requires single point linear regression over three different problem sets. 
# All problem sets are provided over Sakai for use in this assignment.

# The first problem set deals with Old Faithful 

### Old Faithful. In your answer, make sure you include the output from the regression
### model including the estimated intercept, slope, residual standard error, and R2. Also
### include the 95% confidence interval for the slope, and explain what the interval reveals
### about the relationship between duration and waiting time. Describe in a few sentences
### whether or not you think the regression assumptions are plausible based on residual
### plots (you don't need to include the plots). For the part of the question about prediction
### bands, construct a 95% prediction interval for the waiting time until the next eruption
### if the duration of the previous one was 4 minutes. This is the only prediction interval
### you need to report.

oldf <- read.csv("C:/Users/Joseph/Documents/R/IDS 702/OldFaithful.csv", header = T)

#summary information
head(oldf)
summary(oldf)

#initial plots
plot(x = oldf$Interval, y = oldf$Duration, xlab = "Interval", ylab = "Duration", main = "Gyser Duration at Old Faithful")
ggplot(data = oldf) + geom_point(mapping = aes(x = Interval, y = Duration))

#correlation data
cor(oldf$Interval, oldf$Duration)
regConT <- lm(Interval ~ Duration , data = oldf)
summary(regConT)
confint(regConT, level = 0.95)

#new intervals based on if the previous time was 4 minutes
newduration <- 4
newdata <- data.frame(Duration = newduration)

#predition data
predict.lm(regConT, newdata, interval = "prediction")
predict.lm(regConT, newdata, interval = "confidence")

#new plot of residuals
plot(y = regConT$residual, x=oldf$Interval, ylab = "Residuals", main = "Plot of residuals versus Time")
abline(0,0)  #adds a horizontal line at zero to help with visualization

#normal qualtile plot of residuals
qqnorm(regConT$residuals, ylab = "Residuals", main = "Normal quantile plot of residuals")


### The second problem set deals with Respitory rates for children

### Respiratory Rates for Children. In addition to the plot, include the output of
### the regression that predicts (possibly transformed) respiratory rates from (possibly
### transformed) age, as well as evidence that the model its the assumptions reasonably
### well. Demonstrate the usefulness of the model by providing 95% prediction intervals
### for the rate for three individual children: a 1 month old, an 18 months old, and a 29
### months old.

resp <- read.csv("C:/Users/Joseph/Documents/R/IDS 702/Respiratory.csv", header = T)

#summary information
head(resp)
summary(resp)

#went back after looking at the residuals and played with log of each variable to get a better fitting line
resp$logRate <- log(resp$Rate)

#initial plots
plot(x = resp$Age, y = resp$logRate, xlab = "Age in months", ylab = "Rate", main = "Respitory rate by age")
ggplot(data = resp) + geom_point(mapping = aes(x = Age, y = Rate))

#correlation data
cor(resp$Age, resp$logRate)
regConT = lm(Rate ~ Age , data = resp)
summary(regConT)
confint(regConT, level = 0.95)

#Transformed data for respitory rates at 1 month
newAge <- c(1)
newdata <- data.frame(Age = newAge)

#predition data
predict.lm(regConT, newdata, interval = "prediction")
predict.lm(regConT, newdata, interval = "confidence")

#new plot of residuals
plot(y = regConT$residual, x=resp$Age, ylab = "Residuals", main = "Plot of residuals versus Time")
abline(0,0)  #adds a horizontal line at zero to help with visualization

#normal qualtile plot of residuals
qqnorm(regConT$residuals, ylab = "Residuals", main = "Normal quantile plot of residuals")

#Transformed data for respitory rates at 18 months
newAge <- c(18)
newdata <- data.frame(Age = newAge)

#predition data
predict.lm(regConT, newdata, interval = "prediction")
predict.lm(regConT, newdata, interval = "confidence")

#new plot of residuals
plot(y = regConT$residual, x=resp$Age, ylab = "Residuals", main = "Plot of residuals versus Time")
abline(0,0)  #adds a horizontal line at zero to help with visualization

#normal qualtile plot of residuals
qqnorm(regConT$residuals, ylab = "Residuals", main = "Normal quantile plot of residuals")

#Transformed data for respitory rates at 29 month
newAge <- c(29)
newdata <- data.frame(Age = newAge)

#predition data
predict.lm(regConT, newdata, interval = "prediction")
predict.lm(regConT, newdata, interval = "confidence")

#new plot of residuals
plot(y = regConT$residual, x=resp$Age, ylab = "Residuals", main = "Plot of residuals versus Time")
abline(0,0)  #adds a horizontal line at zero to help with visualization

#normal qualtile plot of residuals
qqnorm(regConT$residuals, ylab = "Residuals", main = "Normal quantile plot of residuals")

### The last problem set deals with the 2000 Election

### The Dramatic U.S. Presidential Election of 2000. Include the output from
### the nal regression model that you used, as well as evidence that the model ts the
### assumptions reasonably well. Also include the 95% prediction interval based on your
### nal model.

elect <- read.csv("C:/Users/Joseph/Documents/R/IDS 702/Elections.csv", header = T)
elect <- elect[!(elect$County == "Palm Beach"),]

head(elect)
summary(elect)

#went back after the initial runs to build a better model
elect$Bush2000 <- log(elect$Bush2000)
elect$Buchanan2000 <- log(elect$Buchanan2000)

#initial plots
plot(x = elect$Bush2000, y = elect$Buchanan2000, xlab = "Bush Votes", ylab = "Buchanan Votes", main = "2000 U.S. Election")
# ggplot(data = elect) + geom_point(mapping = aes(x = Bush2000, y = Buchanan2000))

#correlation data
cor(elect$Buchanan2000, elect$Bush2000)
regConT <- lm(Buchanan2000 ~ Bush2000 , data = elect)
summary(regConT)
confint(regConT, level = 0.95)

BuchananBP <- exp(0.73096*log(152846)-2.34149)

BushPBVotes = 152846
newdata <- data.frame(Bush2000 = BushPBVotes)

#predition data
exp(predict.lm(regConT, newdata, interval = "prediction"))
#predict.lm(regConT, newdata, interval = "confidence")

#new plot of residuals
plot(y = regConT$residual, x=elect$Bush2000, ylab = "Residuals", main = "Plot of residuals versus Time")
abline(0,0)  #adds a horizontal line at zero to help with visualization

#normal qualtile plot of residuals
qqnorm(regConT$residuals, ylab = "Residuals", main = "Normal quantile plot of residuals")


#normal qualtile plot of residuals
qqnorm(regConT$residuals, ylab = "Residuals", main = "Normal quantile plot of residuals")

