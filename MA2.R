# Methods and Analysis #2 Due Thursday September 20th, 2018

library(lattice)

# Brain Weight Question (A, B, C) (Book Problem #12)

#read in the file for use
mammal <- read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/Ex0912.csv", header = T)

# See the dimensions of the dataframe in use
dim(mammal)

# a quick Summary of all of the data in the data frame
summary(mammal)

# a quick plot of all of the infomration/variables against brain size
plot(mammal$Brain ~ mammal$Body + mammal$Litter + mammal$Gestation, data = mammal, ask=T)

# a construction of the model without log transformation. This was for my own edification
preMam <- lm(mammal$Brain ~ mammal$Body + mammal$Litter+ mammal$Gestation, data = mammal)
summary(preMam) #r-square of 0.81

# Log transformation of all variables, per the question
mammal$LogBody <- log(mammal$Body)
mammal$LogLitter <- log(mammal$Litter)
mammal$LogBrain <- log(mammal$Brain)
mammal$LogGestation <- log(mammal$Gestation)

#construction and plot of the model for Brain Size on body, litter, and Gestation, all post log transformation
logMam <- lm(mammal$LogBrain ~ mammal$LogBody + mammal$LogLitter + mammal$LogGestation, data = mammal)
summary(logMam)  #R-Squared of 0.9537
# Coefficients:
#Estimate Std. Error t value
#(Intercept)       0.85482    0.66167   1.292
#mammal$Body       0.57507    0.03259  17.647
#mammal$Litter    -0.31007    0.11593  -2.675
#mammal$Gestation  0.41794    0.14078   2.969
#Pr(>|t|)    
#(Intercept)       0.19962    
#mammal$Body       < 2e-16 ***
#  mammal$Litter     0.00885 ** 
#  mammal$Gestation  0.00381 ** 
#  ---
#  Signif. codes:  
#  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.4748 on 92 degrees of freedom
#Multiple R-squared:  0.9537,	Adjusted R-squared:  0.9522
plot(mammal$LogBrain ~ mammal$LogBody + mammal$LogLitter + mammal$LogGestation, data = mammal, ask=T)

# Residual Plots of all the variables to determine if the model is a tighly fitted one or not
plot(y= logMam$residual, x = mammal$LogLitter, xlab = "Litter", ylab = "Residual")
abline(0,0)

plot(y= logMam$residual, x = mammal$LogBrain, xlab = "Brain", ylab = "Residual")
abline(0,0)

plot(y= logMam$residual, x = mammal$LogGestation, xlab = "Gestation", ylab = "Residual")
abline(0,0)

# Brain Weight Question (D, E, F) (Book Problem #12)

litterMam <- lm(mammal$LogBrain ~ mammal$LogBody + mammal$Litter + mammal$LogGestation, data = mammal)
summary(litterMam)
#Coefficients:
#  Estimate Std. Error t value
#(Intercept)      -0.2499816  0.4877205  -0.513
#mammal$Body       0.5594194  0.0329721  16.966
#mammal$Litter    -0.0001442  0.0001180  -1.222
#mammal$Gestation  0.6234242  0.1143816   5.450
#Pr(>|t|)    
#(Intercept)         0.609    
#mammal$Body       < 2e-16 ***
#  mammal$Litter       0.225    
#mammal$Gestation 4.18e-07 ***
#  ---
#  Signif. codes:  
#  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.4889 on 92 degrees of freedom
#Multiple R-squared:  0.9509,	Adjusted R-squared:  0.9493 

plot(mammal$LogBrain ~ mammal$LogBody + mammal$Litter + mammal$LogGestation, data = mammal, ask=T)

#plotting the residuals to see if it is a tighter fit than the other model

plot(y= litterMam$residual, x = mammal$Litter, xlab = "Litter", ylab = "Residual")
abline(0,0)

plot(y= litterMam$residual, x = mammal$LogBrain, xlab = "Brain", ylab = "Residual")
abline(0,0)

plot(y= litterMam$residual, x = mammal$LogGestation, xlab = "Gestation", ylab = "Residual")
abline(0,0)

# Ketucky Derby Question (Book Problem #20)

derby <- read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/Ex0920.csv", header = T)

summary(derby)
plot(y = derby$Speed, x = derby$Year, xlab = "Year", ylab = "Speed" )

#transformations after looking at the intial plot
derby$cSpeed <- derby$Speed - mean(derby$Speed)
derby$cYear <- derby$Year - mean(derby$Year)
derby$Year2 <- (derby$Year)^2
derby$cYear2 <- derby$Year2 - mean(derby$Year2)

#after transformation looking
xyplot(cSpeed ~ Year + Year2 | Condition, data = derby)

#DerbyModelNoTrans <- lm(Speed ~ Year + conGood + conSlow)
#summary(DerbyModelNoTrans)

#creating the model with fast being the baseline
DerbyModel <- lm(cSpeed ~ Year + Year2 + as.factor(Condition), data = derby)
summary(DerbyModel)

DerbyModel2 <- lm(Speed ~ Year + Year2 + as.factor(Condition), data = derby)
summary(DerbyModel2)

#checking the residuals
plot(DerbyModel$resid, x=derby$cSpeed, ylab = "Residuals")
abline(0,0)
boxplot(DerbyModel$resid ~ derby$Condition, ylab = "Residuals")

xyplot(DerbyModel$residuals ~ cSpeed | Condition, data = derby)

# Old Faithful Question (Book Probelm #15)

oldf <- read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/Ex1015.csv", header = T)

# check the initial data for exploritory analysis
summary(oldf)
plot(oldf$Interval ~ oldf$Duration + oldf$Date, data = oldf, ask=T)

# create a model of wait times per day
otheroldfModel <- lm(Interval ~ Duration, data = oldf)
oldFModel <- lm(Interval ~ Duration + as.factor(Date), data = oldf)
summary(oldFModel)

# Checking the models through plots
xyplot(oldFModel$residuals ~ Interval | Date, data = oldf)

anova(otheroldfModel, oldFModel)

# Wages and Race (Book Problem #29)

wagerace <- read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/Ex1029.csv", header = T)

summary(wagerace)

boxplot(Wage ~ Black, data = wagerace, xlab = "race", ylab = "Beginning Salary", main = "Beginning salaries by Race")

plot(Wage ~ Black + Education + Experience + Region + SMSA, data = wagerace, ask = T)

#model with no transformation
wagemodel <- lm(Wage ~ Black + Education + Experience + as.factor(Region) + as.factor(SMSA), data = wagerace)
summary(wagemodel) # R-Squared of .2108

xyplot(Wage ~ Black | Region, data = wagerace )
xyplot(Wage ~ Black | SMSA, data = wagerace)

#model with log transformation of wage
LogWage <- log(wagerace$Wage)
wagemodel2 <- lm(LogWage ~ Black + Education + Experience + as.factor(Region) + SMSA, data = wagerace)
summary(wagemodel2) # R-Squared of .2827

Education2 <- (wagerace$Education)^2
Experience2 <- (wagerace$Experience)^2

wagemodel3 <- lm(LogWage ~ as.factor(Black) * as.factor(Region) + Education + Education2 + Experience + Experience2 + as.factor(SMSA), data = wagerace)
summary(wagemodel3)
