#############################################################################################
#                                                                                           #
#                               MA 4 - Logistic Regression                                  #
#                                 Due October 11th, 2018                                    #
#                                     Joe Littell                                           #
#                                                                                           #
#############################################################################################

# Read in the nessecary libraries

library(arm)          #required for binned plots
library(pROC)         #required for ROC curves
library(tidyverse)    #required for fancy plots

# read in the data

smoking <- read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/babiesdata.csv", header = T)

# initial look at the data
dim(smoking)
head(smoking)
summary(smoking)

#############################################################################################
#                                                                                           #
#                     Data cleaning - rearranging of data (used in MA #3)                   #
#                                                                                           #
#############################################################################################

# collapsing all white races to one
smoking$mrace[smoking$mrace <= 5] <- 5
smoking$mrace[smoking$mrace == 10] <- 5

# collapsing all pervious pregnancies to 1-5 and 6+
smoking$parity[smoking$parity == 0] <- 0 # No previous Child
smoking$parity[smoking$parity == 1] <- 1 # One Prevous Child
smoking$parity[smoking$parity == 2] <- 2 # Two Prevous Child
smoking$parity[smoking$parity == 3] <- 3 # Three Prevous Child
smoking$parity[smoking$parity == 4] <- 4 # Four Prevous Child
smoking$parity[smoking$parity == 5] <- 5 # Five Prevous Child
smoking$parity[smoking$parity >= 6] <- 6 # Six+ Prevous Child to 6+

#removing father's data
smoking$dage <- NULL
smoking$ded <- NULL
smoking$drace <- NULL
smoking$dht <- NULL
smoking$dwt <- NULL
smoking$time <- NULL
smoking$number <- NULL
smoking$marital <- NULL
smoking$inc <- NULL

# Remove NA cases
column_names <- c('gestation', 'mrace', 'mage', 'med', 'mht', 'mpregwt', 'smoke', 'Premature')
smoking <- smoking[complete.cases(smoking[column_names]),]

#############################################################################################
#                                                                                           #
#                             Exploritoy Data Analysis                                      #
#                                                                                           #
#############################################################################################

#initial exploritory data analysis
boxplot(parity~Premature, data = smoking, ylab = "Premature", xlab = "parity")
boxplot(mrace~Premature, data = smoking, ylab = "Premature", xlab = "Mother's Race")
boxplot(mage~Premature, data = smoking, xlab = "Premature", ylab = "Mother's Age (Years)")
boxplot(med~Premature, data = smoking, xlab = "Premature", ylab = "Mother's Education")
boxplot(mht~Premature, data = smoking, xlab = "Premature", ylab = "Mother's Height (inches)")
boxplot(mpregwt~Premature, data = smoking, xlab = "Premature", ylab = "Mother's Pregnancy Weight (LBS)")
boxplot(smoke~Premature, data = smoking, xlab = "Premature", ylab = "Smoking")

#checking the tables
table(smoking$parity, smoking$Premature)
table(smoking$parity, smoking$Premature)/981

table(smoking$mrace, smoking$Premature)
table(smoking$mrace, smoking$Premature)/981

table(smoking$mage, smoking$Premature)
table(smoking$mage, smoking$Premature)/981

table(smoking$med, smoking$Premature)
table(smoking$med, smoking$Premature)/981

table(smoking$mht, smoking$Premature)
table(smoking$mht, smoking$Premature)/981

table(smoking$mpregwt, smoking$Premature)
table(smoking$mpregwt, smoking$Premature)/981

table(smoking$inc, smoking$Premature)
table(smoking$inc, smoking$Premature)/981

table(smoking$smoke, smoking$Premature)
table(smoking$smoke, smoking$Premature)/981

# T Apply
tapply(smoking$mrace, smoking$smoke, mean)

# check to see if the variables are correct before proceeding
summary(smoking)


#############################################################################################
#                                                                                           #
#                     Model Building using Logistic Regression                              #
#                                                                                           #
#############################################################################################

# mean centering our continuous variables
smoking$mean_mage <- smoking$mage - mean(smoking$mage)
smoking$mean_mht <- smoking$mht - mean(smoking$mht)
smoking$mean_mpregwt <- smoking$mpregwt - mean(smoking$mpregwt)

# Model building removing income because there is a substantial number of NAs
GestModel = glm(Premature ~ smoke + parity + mean_mage + mean_mht + 
                mean_mpregwt + med + mrace, data= smoking, family = binomial)
summary(GestModel)

#############################################################################################
#                                                                                           #
#                           Checking the Model for accuracy                                 #
#                                                                                           #
#############################################################################################

#create the predicted probabilities using the predict command
predprobs = predict(GestModel, type = "response")

# create the raw reseiduals by subtracting the predicted probabilities from the Premature values
rawresids = smoking$Premature - predprobs

# Plot the residuals using binned plots in order to diagnos errors within the system
binnedplot(x= predprobs, y= rawresids, xlab = "Predicted Probabilities")
binnedplot(x= smokingl$smoke, y= rawresids, xlab = "Predicted Probabilities")
binnedplot(x= smoking$mean_mage, y= rawresids, xlab = "Predicted Probabilities")
binnedplot(x= smoking$parity, y= rawresids, xlab = "Predicted Probabilities")
binnedplot(x= smoking$mean_mht, y= rawresids, xlab = "Predicted Probabilities")
binnedplot(x= smoking$mean_mpregwt, y= rawresids, xlab = "Predicted Probabilities")

# Confusion Matrix
threshold = 0.5
table(smoking$Premature, GestModel$fitted > threshold)

# Sensitivity
7 / (7+182)

# Specificity
786/ (786+6)

# ROC Curve
roc(smoking$Premature, fitted(logreg2), plot=T, legacy.axes=T)

# Confidence Interval
confint(GestModel)

#############################################################################################
#                                                                                           #
#                                Questions to tbe answered                                  #
#                                                                                           #
# 1. Do mothers who smoke tend to have higher chances of pre-term birth than mothers who    #
# do not smoke? What is a likely range for the difference in odds of pre-term birth         #
# for smokers and non-smokers?                                                              # 
# 2. Is there any evidence that the association between smoking and pre-term birth differ   #
# by mother's race? If so, characterize those differences.                                  #
# 3. Are there other interesting associations with the odds of pre-term birth that are      #
# worth mentioning?                                                                         #
#                                                                                           #
#############################################################################################

