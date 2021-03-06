####  multinomial logistic regression with Sesame street data

#let's predict how often the kids watch sesame street, with focus on if encouragement pushes them towards more viewing

sesame = read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/sesame.csv", header = T)

#load in the nnet library to do multinomial regression
library(nnet)
library(arm)

#viewcat is the outcome variable.  actually it is ordered, but we will treat it as unordered for this analysis.

table(sesame$viewcat)

###let's look at associations of viewcat with various predictors.  Ignore all the post-test variables

##let's start looking at plots with continuous predictors

boxplot(prenumb ~ viewcat, data = sesame, xlab = "Viewing Category", ylab = "Pretest Numbers")
boxplot(prelet ~ viewcat, data = sesame, xlab = "Viewing Category", ylab = "Pretest Letters")
boxplot(preform ~ viewcat, data = sesame, xlab = "Viewing Category", ylab = "Pretest Forms")
boxplot(preclasf ~ viewcat, data = sesame, xlab = "Viewing Category", ylab = "Pretest Classification")
boxplot(prerelat ~ viewcat, data = sesame, xlab = "Viewing Category", ylab = "Pretest Relationships")
boxplot(prebody ~ viewcat, data = sesame, xlab = "Viewing Category", ylab = "Pretest Body Parts")
boxplot(age ~ viewcat, data = sesame, xlab = "Viewing Category", ylab = "Age")

##now some categorical predictors
# bivariate relationship with viewenc
enctable = table(sesame$viewcat, sesame$viewenc)
enctable
prop.table(enctable, 2)
#prop.table(enctable, 1) gives the row percentages

sitetable = table(sesame$viewcat, sesame$site)
sitetable
prop.table(sitetable, 2)

#center all the continuous predictors for the regression modeling
sesame$cprenumb = sesame$prenumb - mean(sesame$prenumb)
sesame$cprelet = sesame$prelet - mean(sesame$prelet)
sesame$cpreform = sesame$preform - mean(sesame$preform)
sesame$cpreclasf = sesame$preclasf - mean(sesame$preclasf)
sesame$cprerelat = sesame$prerelat - mean(sesame$prerelat)
sesame$cprebody = sesame$prebody - mean(sesame$prebody)
sesame$cage = sesame$age - mean(sesame$age)

#let's make a dummy variable with encouragement = 2 as the baseline, since we want to interpret the effect of encouragement
sesame$viewenc2 = rep(0, nrow(sesame)) 
sesame$viewenc2[sesame$viewenc == 1] = 1

#fit a multinomial regression.  note that viewcat = 1 is the reference level.
viewcatreg1 = multinom(viewcat ~ viewenc2 + as.factor(site) + cprenumb + cprelet + cpreform + cpreclasf + cprerelat + cprebody + cage, data = sesame)

summary(viewcatreg1)

#exponentiate to get to interpretations in terms of multiplicative factors for odds

exp(coef(viewcatreg1))

#get confidence intervals
confint(viewcatreg1)
exp(confint(viewcatreg1))

#test if site is a useful  predictor using a change in deviance test

viewcatreg1nosite = multinom(viewcat ~ viewenc2 + cprenumb + cprelet + cpreform + cpreclasf + cprerelat + cprebody  + cage, data = sesame)

anova(viewcatreg1, viewcatreg1nosite, test = "Chisq")

#p-value is small -- site looks like a useful predictor

#test if encouragement is a useful  predictor using a change in deviance test

viewcatreg1noenc = multinom(viewcat ~ as.factor(site) + cprenumb + cprelet + cpreform + cpreclasf + cprerelat + cprebody + cage, data = sesame)

anova(viewcatreg1, viewcatreg1noenc, test = "Chisq")

#p-value is small -- encouragement looks like a useful predictor

##predicted probabilities for cases in the model
predprobs = fitted(viewcatreg1) 

#look at first five rows just to see what results
predprobs[1:5,]

###interpreting results with predictions

#let's get predicted probabilities for someone at average values of continuous predictors and in site 1
newdata = sesame[1:2,]
newdata$site = 1
newdata$cprenumb = 0
newdata$cprelet = 0
newdata$cprerelat = 0
newdata$cpreform = 0
newdata$cprebody = 0
newdata$cpreclasf = 0
newdata$cage = 0
newdata$viewenc2[1] = 0
newdata$viewenc2[2] = 1

predict(viewcatreg1, newdata, type = "probs")

#           1         2         3         4
#1 0.27441129 0.2144506 0.3088348 0.2023033
#2 0.02655388 0.3858047 0.3838143 0.2038272

#now let's repeat for a kid in site 2

newdata$site = 2
predict(viewcatreg1, newdata, type = "probs")

#           1         2         3         4
#1 0.24999038 0.1353515 0.2447120 0.3699461
#2 0.02561089 0.2577973 0.3219774 0.3946144

####diagnostics comparing average raw residuals across bins based on predictor values

#for viewcat = 1:  create a raw residual using only the first column of the predicted probabilities
rawresid1 = (sesame$viewcat == 1) -  predprobs[,1]

#for viewcat = 2:  create a raw residual using only the second column of the predicted probabilities
rawresid2 = (sesame$viewcat == 2) -  predprobs[,2]

#for viewcat = 3:  create a raw residual using only the third column of the predicted probabilities
rawresid3 = (sesame$viewcat == 3) -  predprobs[,3]

#for viewcat = 4:  create a raw residual using only the fourth column of the predicted probabilities
rawresid4 = (sesame$viewcat == 4) -  predprobs[,4]

tapply(rawresid1, sesame$site, mean)
tapply(rawresid2, sesame$site, mean)
tapply(rawresid3, sesame$site, mean)
tapply(rawresid4, sesame$site, mean)

tapply(rawresid1, sesame$viewenc2, mean)
tapply(rawresid2, sesame$viewenc2, mean)
tapply(rawresid3, sesame$viewenc2, mean)
tapply(rawresid4, sesame$viewenc2, mean)

#looks good!

##can do binned plots for continuous variables
#make a 2 by 2 graphical display
par(mfcol = c(2,2))

binnedplot(sesame$cprenumb, rawresid1, xlab = "Prenumbers", ylab = "Raw residuals", main = "Binned plot: viewcat = 1")
binnedplot(sesame$cprenumb, rawresid2, xlab = "Prenumbers", ylab = "Raw residuals", main = "Binned plot: viewcat = 2")
binnedplot(sesame$cprenumb, rawresid3, xlab = "Prenumbers", ylab = "Raw residuals", main = "Binned plot: viewcat = 3")
binnedplot(sesame$cprenumb, rawresid4, xlab = "Prenumbers", ylab = "Raw residuals", main = "Binned plot: viewcat = 4")

#repeat for the other continuous predictors.....  all looks okay!

###if you want to change the reference level, e.g., using level 4 as reference, use the relevel command
sesame$viewcat2 = relevel(as.factor(sesame$viewcat), ref = "4")

#then refit the model
viewcatreg2 = multinom(viewcat2 ~ viewenc2 + as.factor(site) + cprenumb + cprelet + cpreform + cpreclasf + cprerelat + cprebody + cage, data = sesame)
summary(viewcatreg2)
