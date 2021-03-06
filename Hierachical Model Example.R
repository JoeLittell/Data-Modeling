#R for mercury in bass data, using multi-level models
#install the package "lme4" to use the lmer command.

library(lme4)
library(lattice)

bass = read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/mercurydata.txt", header = T)
dim(bass)
summary(bass)

#### Exploratory data analysis to start thinking about models

#let's see how many stations and the counts in each (top row is label, second row is count)
table(bass$station.2)

#Station is a factor, so let's make it one in the data
bass$station.2 = factor(bass.station.2)

#let's make river a factor as well.
bass$river = factor(bass$river)

#let's look at mercury vs. weight and vs. length
pairs(mercury~ weight + length, data = bass)

#perhaps some fanning out, but not obvious fanning -- we might consider a log transformation of mercury
#similar relationships for mercury vs. length and mercury vs. weight
#check correlations among the predictors to look for colinearity

cor(bass$weight, bass$length)

#quite correlated!  let's just use one of the predictors.  length looks a stronger predictor.

#look at mercury versus length by river -- not much differences across rivers
xyplot(mercury ~ length | river, data = bass)

#mercury and length by station. maybe some different slopes, but sample sizes are too small to tell
xyplot(mercury ~ length | station.2, data = bass)

#we might try linear model with mercury and length

#create mean centered length
bass$length.c= bass$length - mean(bass$length)

#try a model using interactions with station
mercreg = lm(mercury ~ length.c * station.2, data = bass)
summary(mercreg)

#diagnostics
plot(y = mercreg$resid, x=bass$length, xlab = "Length", ylab = "Residual")
abline(0,0)
boxplot(mercreg$resid~bass$station.2, xlab = "Station", ylab = "Residual")

#pretty good fit... let's go with it. you could try logs as well.

#see if interaction effects are useful
mercregnoint = lm(mercury ~ length.c + station.2, data = bass)
anova(mercregnoint, mercreg)

#they seem to be useful overall, but many are based on very small sample sizes.
#it would be good to borrow strength across the stations to get better estimates
#that is what a hierarchical model does!

#### let's do a hierarchical regression (random effects regression) with lmer

#this call just uses a random intercept
mercreglmerint = lmer(mercury ~ length.c + (1 | station.2), data = bass) 
summary(mercreglmerint)

#look at the intercepts (and the common slope) for each station
coef(mercreglmerint)

#these equal the fixed effects plus the random effect
fixef(mercreglmerint)
ranef(mercreglmerint)


#this lmer call uses a random intercept and a random slope

mercreglmerintslope = lmer(mercury ~ length.c + ( 1 + length.c  | station.2), data = bass) 
summary(mercreglmerintslope)

#the intercepts and slope for each station
coef(mercreglmerintslope)

#the predicted values of mercury for each bass
preds = fitted(mercreglmerintslope)

#plot residuals versus length

plot(y = residuals(mercreglmerintslope), x = bass$length, xlab= "Length", ylab = "Residuals")

