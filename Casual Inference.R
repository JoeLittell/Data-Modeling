### Script for analyis of the minimum wage data
# see the slides on Propensity scores for data description

#load in the data
minwagedata = read.csv("MinimumWageData.csv", header = T)

#check to make sure the data loaded correctly
dim(minwagedata)
minwagedata[1:2,]

summary(minwagedata)

#note that there are more in NJ than PA, and we labeled NJ = 1 and PA = 0
#we will want to switch the 0s and 1s later for matching

#let's look at covariates by NJ and by PA

#first PA
summary(minwagedata[minwagedata$NJ.PA == 0, 3:8])

#now NJ
summary(minwagedata[minwagedata$NJ.PA == 1, 3:8])

#notice that the distributions of prior employment are not well balanced
#other variables pretty close, but we might be able to do better by matching

#since there are more in PA than NJ, we make PA the treated and NJ the control
#we can do this pretty easily by making a new dummy variable

minwagedata$PA.NJ = 0
minwagedata$PA.NJ[minwagedata$NJ.PA == 0] = 1

#fit the logit regression for the propensity scores
pscorereg = glm(PA.NJ ~ EmploymentPre + WagePre + BurgerKing + KFC + Roys + Wendys, data = minwagedata)
pscorereg

#oops... we didn't need to include one of the dummy variables due to collinearity. drop Wendys

pscorereg = glm(PA.NJ ~ EmploymentPre + WagePre + BurgerKing + KFC + Roys, data = minwagedata)
pscorereg

#propensity scores are the predicted probabilities
pscores = predict(pscorereg, type = "response")

#look at distribution of propensity scores for treateds and controls

boxplot(pscores~minwagedata$PA.NJ, xlab = "State (1 = PA, 0 = NJ)", ylab = "Propensity score")

#you also could do this by comparing histograms.  Here is some code to do so
par(mfcol = c(2,1))
hist(pscores[minwagedata$PA.NJ == 1], xlab = "Propensity Score", main = "Propensity scores for PA after matching")
hist(pscores[minwagedata$PA.NJ == 0], xlab = "Propensity Score", main = "Propensity scores for NJ after matching")

#we can see clear differences in the distributions of propensity scores
#thus, a simple comparison of the outcomes would be confounded by differences in the background variables

#the propensity scores overlap lots, so we can feel good about prospects for matching
#we will use the package MatchIt to do propensity score matching
#install.packages("MatchIt")
library(MatchIt)

#main call-- embed the logistic regression inside the call
#start with a main effects only regression
matchesMW = matchit(PA.NJ ~ EmploymentPre + WagePre + BurgerKing + KFC + Roys, method = "nearest", distance = "logit", data = minwagedata)

#if you are curious, here are the row numbers of the matched controls using built in functions from MatchIt
matchesMW$match.matrix

#create the combined dataset using function from MatchIt
minwagematcheddata = match.data(matchesMW)

#let's look at the propensity scores after matching.  the "distance" variable gives scores. 
boxplot(minwagematcheddata$distance~minwagematcheddata$PA.NJ, xlab = "State (1 = PA, 0 = NJ)", ylab = "Propensity score")

#the distributions are more similar! now let's look at balance post-matching for each covariate.
#can see results conveniently using the summary command on the MatchIt object

summary(matchesMW)

#for descriptions of all output, see https://r.iq.harvard.edu/docs/matchit/2.4-20/Output_Values2.html 
#we will focus on the percent balance improvement table.  postive values are good, negative values are bad

#generally improved balance!  Not quite balanced for percentage of Roys restaurants

#let's repeat but this time include the outputs for interactions and squared terms

summary(matchesMW, interactions = T)

#looks like quantities involving Roys restaurants got worse after matching.
#let's see if we can find a better balance using interaction effects...

matchesMWint = matchit(PA.NJ ~ WagePre*(BurgerKing + KFC + Roys) + EmploymentPre*(BurgerKing + KFC + Roys), method = "nearest", distance = "logit", data = minwagedata, )

#we will start looking at the results with "interaction" option  in summary() turned off

summary(matchesMWint)

#this seems to be an improvement! we have closer balance, in general and especially for interactions

#notice that we don't see the balance in the squared terms
#we need to turn on the interaction option to get results for squared terms
#but this also produces silly variables (interactions of interactions) that we should ignore
#so, we turn it on just to look for EmploymentPrexEmplomentPre and WagePrexWagePre

summary(matchesMWint, interactions = T)

#let's see if we can find better balance using interaction effects and quadratic effects...

matchesMWintsq = matchit(PA.NJ ~ (WagePre + I(WagePre^2))*(BurgerKing + KFC + Roys) + (EmploymentPre+ I(EmploymentPre^2))*(BurgerKing + KFC + Roys), method = "nearest", distance = "logit", data = minwagedata)

#now we don't need to turn on the interactions option, since we will get all the output
summary(matchesMWintsq)

#that didn't really seem to help...  wages are off.  let's drop interactions for wages

matchesMWintsq1 = matchit(PA.NJ ~ (WagePre + I(WagePre^2))  + (EmploymentPre+ I(EmploymentPre^2))*(BurgerKing + KFC + Roys), method = "nearest", distance = "logit", data = minwagedata)

summary(matchesMWintsq1)

#that looks pretty good.  but let's turn on the interactions option to check the interaction effects for wages
#here, we are just looking for the two-way interactions with wages and wages^2

summary(matchesMWintsq1, interactions = T)

#let's try removing the quadratic for Wage^2

matchesMWintsq2 = matchit(PA.NJ ~ WagePre  + (EmploymentPre+ I(EmploymentPre^2))*(BurgerKing + KFC + Roys), method = "nearest", distance = "logit", data = minwagedata)
summary(matchesMWintsq2)
summary(matchesMWintsq2, interactions  = T)
#that is even a little better... let's go with this as our final model and matched set

#we can create the final matched set
minwagematcheddata = match.data(matchesMWintsq2)

#we can estimate the difference in the outcome variable
trteffct = mean(minwagematcheddata$EmploymentPost[minwagematcheddata$PA.NJ==1]) - mean(minwagematcheddata$EmploymentPost[minwagematcheddata$PA.NJ==0])

#a problem with this estimator is that there is still imbalance in the treated and 
#matched control covariate distributions!  So, we really shouldn't use it...

#but, if you wanted to do so, you can treat the data like two independent samples  

se = sqrt(var(minwagematcheddata$EmploymentPost[minwagematcheddata$PA.NJ==1])/73 + var(minwagematcheddata$EmploymentPost[minwagematcheddata$PA.NJ==0])/73)

#so confidence intervals would be
trteffct - 1.96*se
trteffct + 1.96*se

#given small imbalances remaining, we should do a regression analysis to 
#control for covariates when estimating the treatment effect.
#this involves regressing on the relevant covariates and adding a dummy variable
#for the treatment.  This is a regular regression analysis, so I leave it
#to you if you want to pursue it further!