###############################################################################
###############################################################################
##                                                                           ##
##   Duke University - Masters of Interdisciplinary Data Science             ##
##   IDS 702 - Modeling and Data Represenation                               ##
##   Instructor: Dr. Jerry Reiter                                            ##
##   Joe Littell                                                             ##
##                                                                           ##
##   Methods and Analysis # 5                                                ##
##   Missing Data                                                            ##
##                                                                           ##
###############################################################################
###############################################################################

library(mice)
library(tidyverse)

completetrees <- read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/treeage.txt")
trees <- completetrees

dim(trees)
summary(trees)

# Remove data 
missingindex <- sample(1:20, 6)     # Choose 6 numbers at random between 1-20
trees$age[missingindex] = NA        # Index at those six numbers and change the corresponding age to NA
  
summary(trees)                      # ensure that data was removed
md.pattern(trees)                   # graphical represenation of how much is missing 

# convert data into only those that have completed cases
cctrees <- cc(trees)
cctrees

# Plot the completed data set of Age vs. diameter to look for patterns in EDA
plot(cctrees$age, x=cctrees$diameter, xlab = "Diameter", ylab = "Age", main = "Complete cases: Age versus Diameter")

# utilize the MICE function to create 50 models for filling the Missing Data 
treesMI50 <- mice(trees, m=50, defaultMethod = c("norm"))

# Create two models from the 50 to diagnose the Missing Data Models
d1 <- mice::complete(treesMI50, 1)
d1
d2 <- mice::complete(treesMI50, 35)
d2

# plot replicated date from MICE over observed/completed data
stripplot(treesMI50, col = c("grey",mdc(5)),pch = c(1,20))
stripplot(treesMI50, age~.imp, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(treesMI50, age~diameter, col=c("grey",mdc(2)),pch=c(1,20))

# create one large data set using two iterations from the initial data 
treesppcheck <- rbind(trees, trees)

# ensure that the second set has all Age data terned to NA's
treesppcheck[21:40, 3] = NA

# use MICE function to fill in missing data with 50 seperate models
trees2MI5ppcheck = mice(treesppcheck, m=50, defaultMethod = c("norm"))

# use two of th models for diagnosis
d1ppcheck = mice::complete(trees2MI5ppcheck, 1)
d1ppcheck
d2ppcheck = mice::complete(trees2MI5ppcheck, 35)
d2ppcheck

par(mfcol=c(2,1))
# Histogram comparing top and bottom of the data set for completed vs replicated data for Age
hist(d1ppcheck$age[1:20], xlab = "Age", main = "Age completed data")
hist(d1ppcheck$age[21:40], xlab = "Age", main = "Age replicated data")

# Histogram comparing top and bottom of the data set for completed vs replicated data for Age
hist(d2ppcheck$age[1:20], xlab = "Age", main = "Age completed data")
hist(d2ppcheck$age[21:40], xlab = "Age", main = "Age replicated data")

# Plot to compare replicated data and completed data agaisnt diameter
plot(d2ppcheck$age[1:20]~d2ppcheck$diameter[1:20], ylab = "Age", xlab = "Diameter", main = "Age vs Diameter completed data")
plot(d2ppcheck$age[21:40]~d2ppcheck$diameter[21:40], ylab = "Age", xlab = "Diameter", main = "Age vs Diameter replicated data")

# Create a linear regression using all of the 50 models for completeing the missing data
treereg = with(data=treesMI50, lm(age~diameter))
treereg2 = pool(treereg)
summary(treereg2)

###############################################################################
##                                                                           ##
##   Question # 2                                                            ##
##   IDS 702 - Modeling and Data Represenation                               ##
##                                                                           ##
###############################################################################

nhanes <- read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/nhanes.csv")
nhanes[nhanes=="."] = NA
nhanes_num = nhanes[,c("bmxbmi","age", "ridageyr","riagendr","ridreth2","dmdeduc","indfminc")]

nhanes_num[, ] <- sapply(nhanes_num[,], as.numeric)

# look at a summary of data
summary(nhanes_num)

#graphically represent missing data
md.pattern(nhanes_num)

# convert NA Factors to NA values 
nhanes_num$indfminc[nhanes_num$indfminc >= 14] <- NA
nhanes_num$dmdeduc[nhanes_num$dmdeduc >= 7]    <- NA

# Ensure that the data was converted/removed
summary(nhanes_num)

# Adjust the data so only completed cases are shown/used
ccnhanes <- cc(nhanes_num)
ccnhanes

# plot Completed Cases for BMI vs. Years
plot(ccnhanes$bmxbmi~ccnhanes$ridageyr, 
     xlab = "BMI", 
     ylab = "Age", 
     main = "Complete cases: BMI versus Age")

# plot Complted Cases for BMI vs. Gender
boxplot(ccnhanes$bmxbmi~ccnhanes$riagendr, 
     xlab = "BMI", 
     ylab = "Age", 
     main = "Complete cases: BMI versus Age")

# Use MICE function to fill in missing data through 10 models
BMIMI50 <- mice(nhanes_num, m = 10, defaultMethod = c("norm"))

d1 <- mice::complete(BMIMI50, 1)
d1

d7 <- mice::complete(BMIMI50, 7)
d7

# Stripped Plots to compare the replicated data vs completed data of the same type.
stripplot(BMIMI50, col = c("grey",mdc(5)),pch = c(1,20))
stripplot(BMIMI50, bmxbmi~.imp, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(BMIMI50, bmxbmi~ridageyr, col=c("grey",mdc(2)),pch=c(1,20))
stripplot(BMIMI50, indfminc~.imp, col=c("grey",mdc(2)),pch=c(1,20))

# create a data frame from the same dataset twice.
bmippcheck <- rbind(nhanes_num, nhanes_num)

# check the dimentions to ensure the correct number of NAs
dim(nhanes_num)

# Setting the lower half of the BMI dataframe to NA to compare replicated data vs normal
bmippcheck[10123:20244, 1] = NA
bmippcheck

# Using MICE to replicate missing data through the use of 10 models 
BMIMI5ppcheck = mice(bmippcheck, m = 10, defaultMethod = c("norm"))

d1ppcheck = mice::complete(BMIMI5ppcheck, 1)
d1ppcheck
d2ppcheck = mice::complete(BMIMI5ppcheck, 5)
d2ppcheck

# Histogram comparing top and bottom of the data set for completed vs replicated data for BMI
par(mfcol=c(2,1))
hist(d1ppcheck$bmxbmi[1:10122], xlab = "BMI", main = "BMI completed data")
hist(d1ppcheck$bmxbmi[10123:20244], xlab = "BMI", main = "BMI replicated data")

# Histogram comparing top and bottom of the data set for completed vs replicated data for BMI
hist(d2ppcheck$bmxbmi[1:10122], xlab = "BMI", main = "BMI completed data")
hist(d2ppcheck$bmxbmi[10123:20244], xlab = "BMI", main = "BMI replicated data")

# Histogram comparing top and bottom of the data set for completed vs replicated data for BMI vs 
plot(d2ppcheck$bmxbmi[1:10122]~d2ppcheck$diameter[1:10122], ylab = "BMI", xlab = "Diameter", main = "Age vs Diameter completed data")
plot(d2ppcheck$bmxbmi[10123:20244]~d2ppcheck$diameter[10123:20244], ylab = "BMI", xlab = "Diameter", main = "Age vs Diameter replicated data")

# Linear Regression Model using MICE data
BMIreg = with(data=BMIMI50, lm(bmxbmi~ridageyr +               # Age
                                 as.factor(riagendr) +         # Gender
                                 as.factor(ridreth2) +         # Race
                                 as.factor(dmdeduc) +          # education
                                 as.factor(indfminc)))         # income

# Regession using a pool of all MICE models as one.
BMIreg2 = pool(BMIreg)

# Summary of Linear Regession Model 
summary(BMIreg2)

confint(BMIreg2)
