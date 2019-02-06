###############################################################################
###############################################################################
##                                                                           ##
##    Duke University - Masters of Interdisciplinary Data Science            ##
##    IDS 702 - Modeling and Representation of Data                          ##
##    Instructor: Dr. Jerry Rieter                                           ##
##    Team Members: Joe Littell, Malvika Marathe                             ##
##                                                                           ##
##    Group Project #1 Looking at Effects of Job Training on Wages           ##
##                                                                           ##
###############################################################################
###############################################################################

library(arm)          #required for binned plots
library(pROC)         #required for ROC curves
library(tidyverse)    #required for data wrangling and fancy plots
library(lattice)      #required for paired plots

# Read in the data
wages <- read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/lalondedata.csv")

# initial look at the data
dim(wages)
head(wages)
summary(wages)

# EDA with plots

plot(wages)

###############################################################################
#                                                                             #
#                                   Code Book                                 #
#   Variable            Description                                           #
#   treat               1 if received job training                            #
#                       0 if did not receive job training                     #
#   age                 age in years                                          #
#   educ                years of education                                    #
#   black               1 if race is black, 0 otherwise                       #
#   hispan              1 if Hispanic ethnicity, 0 otherwise                  #
#   married             1 if married, 0 otherwise                             #
#   nodegree            1 if dropped out of high school, 0 otherwise          #
#   re74                real annual earnings in 1974                          #
#   re75                real annual earnings in 1975                          #
#   re78                real annual earnings in 1978                          #
#                                                                             #
###############################################################################

# Data Cleaning / Wrangling

#converting multiple variables for race into one single variable
wages$race[wages$black == 0 & wages$hispan == 0] <- 0  # People who are not black or hispanic
wages$race[wages$black == 1] <- 1                      # People who are black
wages$race[wages$hispan == 1] <- 2                     # People who are hispanic

# Removing older variables for races
wages$black <- NULL
wages$hispan <- NULL

# centering continious data
wages$ageC <- wages$age - mean(wages$age)
wages$ageC2 <- wages$ageC
wages$educC <- wages$educ - mean(wages$educ)

# Exploritory Data Analysis through Plots
plot(re78~re74, data = wages, ylab = "Wages for 1978", xlab = "Wages for 1974")
abline(1,1)
plot(re78~re75, data = wages, ylab = "Wages for 1978", xlab = "Wages for 1975")
abline(1,1)
boxplot(re78~treat, data = wages, ylab ="Wages for 1978", xlab = "Training")
plot(re78~ageC, data = wages, ylab = "Wages for 1978", xlab = "Age")
plot(re78~educC, data = wages, ylab = "Wages for 1978", xlab = "Education level in Years")
boxplot(re78~race, data = wages, ylab = "Wages for 1978", xlab = "Race (white 0, Black 1, Hisp 2)")
boxplot(re78~married, data = wages, ylab = "Wages for 1978", xlab = "Married 1")

# correlation matrix
mydata <- wages[,2:11]
corMatWages <- cor(mydata)
round(corMatWages, 3)

###############################################################################
###############################################################################
##                                                                           ##
##    Linear Regression Model                                                ##
##                                                                           ##
###############################################################################
###############################################################################

wageModel2 <- lm(re78 ~ re74 + re75 + as.factor(nodegree) + as.factor(treat) + ageC + ageC2 + educC + as.factor(race) + as.factor(married), data = wages)
summary(wageModel2) #R-Squared of 0.1488

confint(wageModel2)

# plot of the residual of Model versus the wage prior to 1974
plot(y= wageModel2$residual, x = wages$re74, xlab = "Wages in 1974", ylab = "Residual")
abline(0,0)

# plot of the residual of Model versus the wage prior to 1975
plot(y= wageModel2$residual, x = wages$re75, xlab = "Wages in 1974", ylab = "Residual")
abline(0,0)

# plot of the residual of the Model versus age in years
plot(y= wageModel2$residual, x = wages$ageC, xlab = "Age in Years", ylab = "Residual")
abline(0,0)

# plot of the residual of the Model versus education in years
plot(y= wageModel2$residual, x = wages$educ, xlab = "Education in Years", ylab = "Residual")
abline(0,0)

# plot of the residual of the Model versus race where 0 equals white, 1 equals black, and 2 equals hispanic
boxplot(wageModel2$residual~wages$race, xlab = "Race", ylab = "Residual")
abline(0,0)

# plot of the residual of the Model versus marraige status where 1 equals married
boxplot(wageModel2$residual~wages$married, xlab = "Married", ylab = "Residual")
abline(0,0)

# plot of the residual of the Model versus marraige status where 1 equals married
boxplot(wageModel2$redidual~wages$treat, xlab = "Received Treatment", ylab = "Residuals")
abline(0,0)

###############################################################################
###############################################################################
##                                                                           ##
##    Logistic Regression Model                                              ##
##                                                                           ##
###############################################################################
###############################################################################

# adjusting wages from 1974 and 1975 to reflect the highest wage earned in those years for 
# comparison 
wages$prewage <- 0

# variable to compare wages pre and post treatment/training
wages$diff <- 0

for(i in 1:nrow(wages)){
   wages$prewage[i] <- max(wages$re74[i], wages$re75[i])
   wages$diff[i] <- wages$re78[i] - wages$prewage[i]
}


# if the worker recieved less in 78 than previous years than the facter is zero   
wages$diff[wages$diff >= 0] <- 1
wages$diff[wages$diff < 0] <- 0
# wages$diff <- wages$re78
# wages$diff[wages$diff > 0] <- 1

summary(wages)

#initial data analysis with graphs
boxplot(wages$re74~wages$diff, xlab = "Wages in 1974", ylab = "Difference Predictor")
boxplot(wages$re75~wages$diff, xlab = "Wages in 1975", ylab = "Difference Predictor")
boxplot(wages$ageC~wages$diff, xlab = "age centered", ylab = "Difference Predictor")
boxplot(wages$educC~wages$diff,  xlab = "Education", ylab = "Difference Predictor")
boxplot(wages$race~wages$diff, xlab = "Race", ylab = "Difference Predictor")
boxplot(wages$married~wages$diff, xlab = "Married", ylab = "Difference Predictor")
boxplot(wages$treat~wages$diff, xlab = "treat", ylab = "Difference Predictor")
boxplot(wages$nodegree~wages$diff, xlab = "no degree", ylab = "Difference Predictor")

# correlation matrix
mydata <- wages[,c(2:11,14)]
corMatWages <- cor(mydata)
round(corMatWages, 3)

# Initial logistic model
logModel <- glm(diff ~ re74 + re75 + as.factor(treat) + as.factor(nodegree) +
                  ageC  + educ + as.factor(race) + as.factory(married),
                data = wages, family = binomial)

summary(logModel)

#create the predicted probabilities using the predict command
predprobs = predict(logModel, type = "response")

# create the raw reseiduals by subtracting the predicted probabilities from the Premature values
rawresids = wages$diff - predprobs

# Plot the residuals using binned plots in order to diagnose errors within the system
binnedplot(x = predprobs, y = rawresids, xlab = "Predicted Probabilities")

# Plot of the residuals against the individual variables to diagnosis for issues
binnedplot(x = wages$re74, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$re75, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$ageC, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$educ, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$race, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$married, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$treat, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$nodegree, y = rawresids, xlab = "Predicted Probabilities")

# Tables to diagnosis
tapply(rawresids, wages$re74, mean) 
tapply(rawresids, wages$re75, mean) 
tapply(rawresids, wages$race==0, mean) 
tapply(rawresids, wages$race==1, mean) 
tapply(rawresids, wages$mrace==2, mean) 
tapply(rawresids, wages$treat==0, mean) 
tapply(rawresids, wages$treat==1, mean) 
tapply(rawresids, wages$ageC, mean) 
tapply(rawresids, wages$educ, mean)
tapply(rawresids, wages$married==0, mean)
tapply(rawresids, wages$married==1, mean)   
tapply(rawresids, wages$nodegree==0, mean) 
tapply(rawresids, wages$nodegree==1, mean)

# Confusion Matrix
threshold = 0.720
table(wages$diff, logModel$fitted > threshold)

# ROC curve
roc(wages$diff, fitted(logModel), plot=T, legacy.axes=T, print.thres = "best")    # AOC 0.8097

# Confidence intervals
confint.default(logModel)
exp(confint.default(logModel))

#####################second model############################
wages$diff <- wages$re78
wages$diff[wages$diff > 0] <- 1

logModel2 <- glm(diff ~ re74 + re75 + as.factor(treat) + as.factor(nodegree) +
                  ageC  + educ + as.factor(race) + as.factor(married),
                data = wages, family = binomial)

summary(logModel2)

#create the predicted probabilities using the predict command
predprobs = predict(logModel2, type = "response")

# create the raw reseiduals by subtracting the predicted probabilities from the Premature values
rawresids = wages$diff - predprobs

# Plot the residuals using binned plots in order to diagnose errors within the system
binnedplot(x = predprobs, y = rawresids, xlab = "Predicted Probabilities")

# Plot of the residuals against the individual variables to diagnosis for issues
binnedplot(x = wages$re74, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$re75, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$ageC, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$educ, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$race, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$married, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$treat, y = rawresids, xlab = "Predicted Probabilities")
binnedplot(x = wages$nodegree, y = rawresids, xlab = "Predicted Probabilities")

# Confusion Matrix
threshold = 0.794
table(wages$diff, logModel2$fitted > threshold)

# ROC curve
roc(wages$diff, fitted(logModel2), plot=T, legacy.axes=T, print.thres = "best")    # AOC 0.8097

# Confidence intervals
confint.default(logModel2)
exp(confint.default(logModel2))

