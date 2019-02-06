## script from intro regression lectures

#a useful package.  type "install.packages("tidyverse") if you have not installed it yet.
install.packages("tidyverse")
library(tidyverse)

#read in the calories data-- data on calories consumed at lunch by 20 toddlers
#goal is to see if there is a relationship between time at table and calories consumed.

caloriesdata <- read.csv("C:/Users/Joseph/Documents/R/IDS 702/calories.txt", header = T)

#this command allows you to see the first 5 rows of the dataset
head(caloriesdata)

#this command provides a quick summary of the distributions of the variables
summary(caloriesdata)

#plot the data (commands without tidyverse) 
plot(x = caloriesdata$Time, y = caloriesdata$Calories, xlab = "Time", ylab = "Calories", main = "Calories versus time")

#same plot using ggplot (note: ggplot is more useful for multivariate datasets)

ggplot(data = caloriesdata) + geom_point(mapping = aes(x = Time, y = Calories))

#get correlation
cor(caloriesdata$Time, caloriesdata$Calories)

#fit a linear regression model
regConT = lm(Calories ~ Time, data = caloriesdata)

#view the results of the regression
summary(regConT)

#get 95% confidence intervals for the coefficients
confint(regConT)

###prediction of calories for new toddlers at 30, 35, and 40 minutes

#first make the dataset of new toddlers and their times
#make sure the predictor in the dataset has the same name as the predictor used in the regression model 

newtimes = c(30, 35, 40)
newdata = data.frame(Time = newtimes)

#now we can make the predictions. this is prediction for individual toddlers
predict.lm(regConT, newdata, interval = "prediction")

#if you want to make a prediction for the AVERAGE calorie amount at these times (not an individual), use
predict.lm(regConT, newdata, interval = "confidence")

### checking the fit of the model with residual plots

#residuals versus predictors (old plotting function)
plot(y = regConT$residual, x=caloriesdata$Time, ylab = "Residuals", main = "Plot of residuals versus Time")
abline(0,0)  #adds a horizontal line at zero to help with visualization

#note: you also can make residual plots using ggplot. I find the plot command a little easier.

#normal quantile plot or histograms of residuals can help assess normal distribution assumption
qqnorm(regConT$residuals, ylab = "Residuals", main = "Normal quantile plot of residuals")


##########  Analysis of AIDS and GDP Data

#goal is to see association between GDP and HIV rates 

aidsgdp = read.csv("aidsngdp.txt", header = T)

#get the dimension of the data: useful to see how many rows and columns
dim(aidsgdp)

#look at first 5 rows
head(aidsgdp)

#get summary statistics for all the variables in the data
summary(aidsgdp)

#rename variables to make typing a little easier!
names(aidsgdp)[4] = "gdp"
names(aidsgdp)[6] = "hiv"

#Exploratory data analysis
plot(y= aidsgdp$hiv, x=aidsgdp$gdp,  xlab = "Gross Domestic Product (per capita)", ylab = "HIV/AIDS Cases per 1000 People")

#no way a linear model will fit these data!  let's see what happens just for kicks...
aidsgdpreg1 = lm(hiv~gdp, data = aidsgdp)
summary(aidsgdpreg1)

plot(y=aidsgdpreg1$residual, x=aidsgdp$gdp, xlab = "GDP", main = "Residual plot for model without transformations")
#this caused an error because there is a missing value in gdp that was dropped from the regression, and hence 
#the residuals, but was not dropped from the gdp. 

#here is how you can make sure the same rows used in the regression are available for plotting:  add an x=T to the lm command.
#this tells R to store the explanatory variables (X) used in the regression, which include only cases with observed data.

aidsgdpreg1 = lm(hiv~gdp, data = aidsgdp, x=T)

#let's see what this gives us
aidsgdpreg1$x

#refit the model: results are exactly the same. adding x=T doesn't change the fitting, just stores the X variables in the regression object
summary(aidsgdpreg1)

#now we can plot residuals using the X values that were used in the regression.
plot(y=aidsgdpreg1$residual, x=aidsgdpreg1$x[,2], xlab = "GDP", ylab = "Residual", main = "Residual plot for model without transformations")
abline(0,0)

#try tranforming with log(y)

aidsgdp$loghiv = log(aidsgdp$hiv)
plot(y= aidsgdp$loghiv, x=aidsgdp$gdp,  xlab = "Gross Domestic Product (per capita)", ylab = "Log HIV/AIDS Cases per 1000 People")

aidsgdpreglogy = lm(loghiv~gdp, data = aidsgdp, x=T)
plot(y=aidsgdpreglogy$residual, x=aidsgdpreglogy$x[,2], xlab = "GDP", ylab = "Residual", main = "Residual plot for model with log(y) transformation")
abline(0,0)

#still no good.  how about logy and log x?

aidsgdp$loggdp = log(aidsgdp$gdp)
plot(y= aidsgdp$loghiv, x=aidsgdp$loggdp,  xlab = "Log Gross Domestic Product (per capita)", ylab = "Log HIV/AIDS Cases per 1000 People")

aidsgdpreglogyx = lm(loghiv~loggdp, data = aidsgdp, x=T)
plot(y=aidsgdpreglogyx$residual, x=aidsgdpreglogyx$x[,2], xlab = "Log GDP", ylab = "Residual", main = "Residual plot for model with log(y) and log(x) transformations")
abline(0,0)

#this looks good!  now we can interpret results

summary(aidsgdpreglogyx)

#confidence intervals on the log scale  -- not terribly interpretable 
confint(aidsgdpreglogyx)

#how much do we expect median of HIV rate to change for a 10% increase in GDP?  We get .925 from
exp(-.8186*log(1.1))

#need confidence interval for exp(beta*log(1.1))
exp(confint(aidsgdpreglogyx)*log(1.1))

#we are 95% confident that the median of the number of HIV cases per 1000 will decrease by a 
#factor of .92 (95% CI: .89, .95) if GDP increases by 10%