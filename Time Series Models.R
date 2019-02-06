####  Time series modeling of FTSE 100 returns

ftse100 = read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/ftse2018.csv", header = T)

#notice that the data go from latest to earliest date
#let's invert the order of the rows to make the time series increasing in date

ftse100 = ftse100[nrow(ftse100):1,]

#make a time series object of the closing prices
tsClose = ts(ftse100$Close)
ts.plot(tsClose)

#quite ugly!  hard to tell if this is stationary, and maybe not...
#let's look at autocorrelations

acf(ftse100$Close)
pacf(ftse100$Close)

#Looks like the autocorrelations are strong, but that lag 1 accounts for most of the autocorrelation

#Often it is better to work with returns for stock data
ftse100$return = (ftse100$Close - ftse100$Open)/ftse100$Open
tsReturn = ts(ftse100$return)
ts.plot(tsReturn)

acf(ftse100$return)
pacf(ftse100$return)

#These returns basically have no time series structure! 
#let's fit an AR(1) model to the returns anyways

ftsereturnmodel = arima(ftse100$return, order = c(1,0,0))
ftsereturnmodel

#suppose you want to make a prediction of the returns six days ahead

predict(ftsereturnmodel, n.ahead = 6)

#often people use log returns for stocks, where they take log(Close/Open) 

#### Time series regression using AR models 

cancersun = read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/Ex1512.csv", header = T)

#change names to get rid of capital letters
names(cancersun) = c("year", "melanoma", "sunspot")

plot(x=cancersun$sunspot, y = cancersun$melanoma, xlab = "Sunspots", ylab = "Melanoma Incidence Rate")

#let's try a linear regression of melanoma on sunspots 
regmelanoma = lm(melanoma ~ sunspot, data = cancersun)

summary(regmelanoma)

plot(regmelanoma$residual, x = cancersun$sunspot, xlab = "Sunspots", ylab = "Residual")
abline(0,0)

#it looks nice... nothing obvious going on.  but these data are ordered in time
#let's look at residuals versus year

plot(regmelanoma$residual, x = cancersun$year, xlab = "Year", ylab = "Residual")
abline(0,0)

#huge trend!  let's compute the autocorrelations to verify

lagcors = acf(regmelanoma$resid)
lagcors

#The lag-1 autocorrelation is large: 0.86.  There is evidence of serial correlation.
#Failing to account for this will result in inaccurate models

#the partial autocorrelation shows you the autocorrelations after removing effects of earlier lags
partlagcors  = pacf(regmelanoma$resid)
partlagcors

#seems like only the lag 1 correlation matters.  But the residuals are not stationary!  

#Let's fit a model that includes year as a predictor, to try to take care of the non-stationarity

###let's control for year and see what happens...

regmelanoma2 = lm(melanoma ~ sunspot + year, data = cancersun)

summary(regmelanoma2)

plot(regmelanoma2$residual, x = cancersun$sunspot, xlab = "Sunspots", ylab = "Residual")
abline(0,0)

plot(regmelanoma2$residual, x = cancersun$year, xlab = "Year", ylab = "Residual")
abline(0,0)

#obivously a lot better! 
#let's use visualization at time series plot of residuals

tsresidregmelanoma2 = ts(regmelanoma2$residual)
ts.plot(tsresidregmelanoma2)

#seems a lot more reasonable to assume stationarity
#let's look for autocorrelation in residuals...

acf(regmelanoma2$resid)
pacf(regmelanoma2$resid)

#.38 lag 1 correlation in residuals.... still a little high. that could distort SE.
#let's fit an AR(1) model, or possibly an AR(2) model since there might be lag 2 effects

#AR(1) model
tsregmelanoma1 = arima(cancersun$melanoma, order = c(1, 0, 0), xreg = cbind(cancersun$sunspot, cancersun$year))
tsregmelanoma1

#diagnostics
plot(y = tsregmelanoma1$residual, x= cancersun$sunspot, ylab = "Residual", xlab = "Sunspots")
abline(0,0)
plot(y = tsregmelanoma1$residual, x= cancersun$year, ylab = "Residual", xlab = "Year")
abline(0,0)

#residuals show a little bit of a pattern -- more negative than positive values

#let's noodle with the model to see if we can get a better fit
#maybe we can get a better model bylet's try predicting from sunspots the previous year

prevsunspot = c(NA, cancersun$sunspot[1:36])

#check to make sure we got it right
cbind(prevsunspot, cancersun$sunspot)

#add to the data
cancersun$prevsunspot = prevsunspot

plot(x=cancersun$prevsunspot[2:37], y = cancersun$melanoma[2:37], xlab = "Sunspots Previous Yr", ylab = "Melanoma Incidence Rate")

###let's control for year and see what happens...

# try with previous year

regmelanoma3 = lm(melanoma ~ prevsunspot + year, data = cancersun[2:37,])

summary(regmelanoma3)

plot(regmelanoma3$residual, x = cancersun$prevsunspot[2:37], xlab = "Sunspots Previous Yr", ylab = "Residual")
abline(0,0)

plot(regmelanoma3$residual, x = cancersun$year[2:37], xlab = "Year", ylab = "Residual")
abline(0,0)

#much better!  the better fitting model seems to have fixed up the problems!  

tsresidregmelanoma3 = ts(regmelanoma3$residual)
ts.plot(tsresidregmelanoma3)

#still reasonable to assume stationarity
#let's look for autocorrelation in residuals...

acf(regmelanoma2$resid)
pacf(regmelanoma2$resid)

#hardly any....  the better model seems to have reduced the autocorrelation

#let's fit an AR(1) model

tsregmelanoma3 = arima(cancersun$melanoma, order = c(1, 0, 0), xreg = cbind(cancersun$prevsunspot, cancersun$year))
tsregmelanoma3

#diagnostics
plot(y = tsregmelanoma3$residual, x= cancersun$prevsunspot, ylab = "Residual", xlab = "Sunspots - Previous Year")
abline(0,0)
plot(y = tsregmelanoma3$residual, x= cancersun$year, ylab = "Residual", xlab = "Year")
abline(0,0)

#look at a time series plot of the residuals.  
tsresidtsregmelanoma3 = ts(tsregmelanoma3$residual)
ts.plot(tsresidtsregmelanoma3)

#seems reasonable to assume stationarity
#let's look for autocorrelation in residuals... we have to exclude the first observation since it has a missing residual

acf(tsregmelanoma3$resid[2:37])
pacf(tsregmelanoma3$resid[2:37])

#no longer any serious autocorrelation!  We seem to have done a good job with the modeling.

#if you want to fit an AR(2) model, which is not necessary here, the command would be as follows
#tsregmelanoma4 = arima(cancersun$melanoma, order = c(2, 0, 0), xreg = cbind(cancersun$prevsunspot, cancersun$year))
#tsregmelanoma4

#Note: I tried a model with sunspots from 2 years ago, and it didn't seem to be a better fit.
#commands for making data for sunspots from previous 2 years
prev2sunspot = c(NA, NA, cancersun$sunspot[1:35])
cancersun$prev2sunspot = prev2sunspot
plot(x=cancersun$prev2sunspot[3:37], y = cancersun$melanoma[3:37], xlab = "Sunspots Previous 2 Yr", ylab = "Melanoma Incidence Rate")