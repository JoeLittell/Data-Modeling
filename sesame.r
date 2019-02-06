# Analysis of Sesame Street data, using postnum as the outcome

#load in lattice library for graphics and MASS library for case influence
library(lattice)
library(MASS)

sesame <-read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/sesame.csv")

#get a sense of the data
dim(sesame)
summary(sesame)

#let's start looking at plots with continuous predictors
plot(y = sesame$postnum, x = sesame$prenum, xlab = "Pretest Numbers", ylab = "Posttest numbers")
plot(y = sesame$postnum, x = sesame$age, xlab = "Age", ylab = "Posttest numbers")
plot(y = sesame$postnum, x = sesame$prelet, xlab = "Pretest Letters", ylab = "Posttest numbers")
plot(y = sesame$postnum, x = sesame$peabody, xlab = "Peabody Score", ylab = "Posttest numbers")
plot(y = sesame$postnum, x = sesame$prebody, xlab = "Pretest Body", ylab = "Posttest numbers")
plot(y = sesame$postnum, x = sesame$preform, xlab = "Pretest Form", ylab = "Posttest numbers")
plot(y = sesame$postnum, x = sesame$preclasf, xlab = "Pretest Classification", ylab = "Posttest numbers")
plot(y = sesame$postnum, x = sesame$prerelat, xlab = "Pretest Relational", ylab = "Posttest numbers")

#now with categorical predictors
boxplot(postnumb ~ viewenc, data = sesame, xlab = "Encouraged", ylab = "Posttest Numbers")
boxplot(postnumb ~ site, data = sesame, xlab = "Site", ylab = "Posttest Numbers")
boxplot(postnumb ~ sex, data = sesame, xlab = "Sex", ylab = "Posttest Numbers")
boxplot(postnumb ~ setting, data = sesame, xlab = "Setting", ylab = "Posttest Numbers")
boxplot(postnumb ~ viewcat, data = sesame, xlab = "Viewing Category", ylab = "Posttest Numbers")

#NOTE:  viewcat formally is an outcome variable and so should not be considered a predictor, if we are thinking about causal effect of encouragement
#so we won't include it in the analysis.

#let's also see if there are any interactions with site, which is an interesting variable from a policy perspective

xyplot(postnumb ~ prenumb | site, data = sesame)
xyplot(postnumb ~ age | site, data = sesame)
bwplot(postnumb ~ as.factor(viewenc) | site, data = sesame)

#nothing really remarkable.  Let's proceed without interactions.

#take a look at correlations among predictors for multicollinearity
round(cor(sesame), 3)

#there are a lot of high correlations among the predictors.  Maybe we don't need to include all of the pre-tests to get reasonable models.

#let's center all the continuous predictors
sesame$cprenumb = sesame$prenumb - mean(sesame$prenumb)
sesame$cprelet = sesame$prelet - mean(sesame$prelet)
sesame$cpreform = sesame$preform - mean(sesame$preform)
sesame$cpreclasf = sesame$preclasf - mean(sesame$preclasf)
sesame$cprerelat = sesame$prerelat - mean(sesame$prerelat)
sesame$cprebody = sesame$prebody - mean(sesame$prebody)
sesame$cpeabody = sesame$peabody - mean(sesame$peabody)
sesame$cage = sesame$age - mean(sesame$age)

#let's make a new dummy variable with encouragement = 1 and no encouragement = 0, since we want to interpret results as the effect of encouragement.
sesame$encouraged = rep(0, nrow(sesame))
sesame$encouraged[sesame$viewenc==1] = 1

#do a quick check to make sure the code did what we wanted it to do.
head(sesame)

## first pass model

reg1 = lm(postnumb ~ as.factor(site) + encouraged + as.factor(sex) + as.factor(setting) + cprenumb + cprelet + cprebody + cpreclasf + cpreform + cprerelat + cpeabody + cage, data = sesame)

summary(reg1)

#looks like effects of multicollinearity could be affecting some estimates.
#we could try to drop some of the predictors and see what happens to our story.  but let's look at the residuals first.

plot(y = reg1$resid, x = sesame$cprenum, xlab = "Pretest Numbers", ylab = "Residual")
abline(0,0)
plot(y = reg1$resid, x = sesame$cage, xlab = "Age", ylab = "Residual")
abline(0,0)
plot(y = reg1$resid, x = sesame$cprelet, xlab = "Pretest Letters", ylab = "Residual")
abline(0,0)
plot(y = reg1$resid, x = sesame$cpeabody, xlab = "Peabody Score", ylab = "Residual")
abline(0,0)
plot(y = reg1$resid, x = sesame$cprebody, xlab = "Pretest Body", ylab = "Residual")
abline(0,0)
plot(y = reg1$resid, x = sesame$cpreform, xlab = "Pretest Form", ylab = "Residual")
abline(0,0)
plot(y = reg1$resid, x = sesame$cpreclasf, xlab = "Pretest Classification", ylab = "Residual")
abline(0,0)
plot(y = reg1$resid, x = sesame$cprerelat, xlab = "Pretest Relational", ylab = "Residual")
abline(0,0)

#now with categorical predictors
boxplot(reg1$resid ~ sesame$encouraged, xlab = "Encouraged", ylab = "Residual")
boxplot(reg1$resid ~ sesame$site,  xlab = "Site", ylab = "Residual")
boxplot(reg1$resid ~ sesame$sex, xlab = "Sex", ylab = "Residual")
boxplot(reg1$resid ~ sesame$setting, xlab = "Setting", ylab = "Residual")

#looking pretty good. let's check case influence diagnostics.

lev = hatvalues(reg1)
cooks = cooks.distance(reg1)

plot(lev, ylab = "Leverage value")
plot(cooks, ylab = "Cooks Distance")

#no one appears too far out there on leverage. let's take a look at the person with the largest Cooks Distance
sesame[cooks > .075,]

#this it the 50th person in the dataset.  nothing jumps out about this person as odd.... look at leverage again. 
lev[50]

#not particularly big...  no real good reason to exclude this point from a scientific perspective.  let's not do anything about the person for now, since 
#we might reduce predictors anyways!

summary(reg1)

#let's do a nested F test to see if site is a useful predictor

reg1nosite = lm(postnumb ~ encouraged + as.factor(sex) + as.factor(setting) + cprenumb + cprelet + cprebody + cpreclasf + cpreform + cprerelat + cpeabody + cage, data = sesame)

anova(reg1nosite, reg1)

#it appears to be a useful predictor.

#let's try an interaction between site and encouragement, since that will address our question of whether encouragement helped disadvantaged students catch up.

reg2 = lm(postnumb ~ as.factor(site) * encouraged + as.factor(sex) + as.factor(setting) + cprenumb + cprelet + cprebody + cpreclasf + cpreform + cprerelat + cpeabody + cage, data = sesame)

anova(reg1, reg2)

#no significant evidence that effects of encouragement differed across sites (types of people)
#so, we can interpret the CIs of the model without interactions.

confint(reg1)

#encouragement to watch Sesame Street is associated with an increase of aronud 1.6 points (95% CI: -1.1 to 4.2) on the numbers post-test.  
#Because this is a wide confidence interval that ranges anywhere from a 1.1 point decrease to a 4.2 point increase, this study does not provide
#conclusive evidence that encouragement to watch Sesame Street made a difference.

#Here we might want to do an analysis of whether encouragement to watch Sesame Street actually made a difference in whether 
#the children actually watch Sesame Street!  That is another outcome variable and so deserves its own regression.

#as a sensitivity analysis, we can try dropping some of the pretest scores.
#For example, drop prerelat or preclassif since they are somewhat highly correlated with prenumb
#It does not appear to make much difference in our overall conclusions about encouragement.  Thus, we might as well keep all of them to aid interpretation.