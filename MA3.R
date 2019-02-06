# Methods and Analysis #3 Due 27 September 2018

# Load needed libraries to analyize the data
library(tidyverse)
library(skimr)
library(lattice)

# Load the data itself

# all data
smoking <-read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/smoking.csv")

skim(smoking)

#collapsing all white races to one
smoking$mrace[smoking$mrace== 0] <- 1  # white
smoking$mrace[smoking$mrace== 1] <- 1  # white
smoking$mrace[smoking$mrace== 2] <- 1  # white
smoking$mrace[smoking$mrace== 3] <- 1  # white
smoking$mrace[smoking$mrace== 4] <- 1  # white
smoking$mrace[smoking$mrace== 5] <- 1  # white
smoking$mrace[smoking$mrace== 6] <- 2  # Mexican
smoking$mrace[smoking$mrace== 7] <- 3  # Black
smoking$mrace[smoking$mrace== 8] <- 4  # Asian
smoking$mrace[smoking$mrace== 9] <- 5  # Mix
smoking$mrace[smoking$mrace== 99] <- NA

#collapsing all pervious pregnancies to 1-5 and 6+
smoking$parity[smoking$parity== 0] <- 0 # No previous Child
smoking$parity[smoking$parity== 1] <- 1 # 1 Prevous Child
smoking$parity[smoking$parity== 2] <- 2 # 1 Prevous Child
smoking$parity[smoking$parity== 3] <- 3 # 1 Prevous Child
smoking$parity[smoking$parity== 4] <- 4 # 1 Prevous Child
smoking$parity[smoking$parity== 5] <- 5 # 5 Prevous Child
smoking$parity[smoking$parity== 6] <- 6 # 6 Prevous Child
smoking$parity[smoking$parity== 7] <- 6 # 6 Prevous Child
smoking$parity[smoking$parity== 8] <- 6 # 6 Prevous Child
smoking$parity[smoking$parity== 9] <- 6 # 6 Prevous Child
smoking$parity[smoking$parity== 10] <- 6 # 6 Prevous Child
smoking$parity[smoking$parity== 11] <- 6 # 6 Prevous Child

# explority plots to see if there is anything noticable prior to transformation or
# regression modeling
plot(bwt.oz ~ inc + mage + med + mht + mpregwt + 
       mrace + parity + smoke, data = smoking, ask = T)

#looking for interactions
xyplot(bwt.oz ~ smoke | mrace, data = smoking)
xyplot(bwt.oz ~ smoke | inc, data = smoking)
xyplot(bwt.oz ~ smoke | mage, data = smoking)
xyplot(bwt.oz ~ smoke | mht, data = smoking)
xyplot(bwt.oz ~ smoke | mpregwt, data = smoking)
xyplot(bwt.oz ~ smoke | parity, data = smoking)

# nothing out of the ordinary or noticable came up so moving on to regression
birthModel <- lm(bwt.oz ~ as.factor(smoke) + as.factor(mrace) + mht + mpregwt +
                   as.factor(med) * inc + mage * as.factor(parity), data = smoking)

# looking at the model 
summary(birthModel) # R-squared is 0.1645 however smoking p value is 5.92 E-16

raceBirthModel <- lm(bwt.oz ~ as.factor(smoke) * as.factor(mrace) + mht + mpregwt +
                       as.factor(med) * inc + mage * as.factor(parity), data = smoking)

summary(raceBirthModel)

anova(birthModel, raceBirthModel)


#plot residuals to make adjustments

# smoking
plot(y= birthModel$residual, x = smoking$smoke, xlab = "Smoking = 1", ylab = "Residual")
abline(0,0)
# income
plot(y= birthModel$residual, x = smoking$inc, xlab = "Income", ylab = "Residual")
abline(0,0)
# mother's age
plot(y= birthModel$residual, x = smoking$mage, xlab = "Mother's Age", ylab = "Residual")
abline(0,0)
#mother's education
plot(y= birthModel$residual, x = smoking$med, xlab = "Mother's Education", ylab = "Residual")
abline(0,0)
# mother's height
plot(y= birthModel$residual, x = smoking$mht, xlab = "Mother's Height", ylab = "Residual")
abline(0,0)
# mother's Pregnancy weight
plot(y= birthModel$residual, x = smoking$mpregwt, xlab = "Mother's Preg WT", ylab = "Residual")
abline(0,0)
# mother's race
plot(y= birthModel$residual, x = smoking$mrace, xlab = "Mother's race", ylab = "Residual")
abline(0,0)
# mother's parity (number of previous births)
plot(y= birthModel$residual, x = smoking$parity, xlab = "Number of previous births", ylab = "Residual")
abline(0,0)

confint(birthModel, level = 0.95)
confint(raceBirthModel, level = 0.95)

