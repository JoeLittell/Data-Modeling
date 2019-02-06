#ANALYSIS OF PRETERM BIRTH AND SMOKING
#the data can be found in the class datasets folder (same as used for the exam in spring 2017!)
#codebook also found in that folder.

babies = read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/babiesdata.csv", header = T)

summary(babies)

#note all the missing data, expressed as "NA"

#some values that don't exactly match the code book.  assume they are missing value codes, but really we would like 
#to ask the data collectors for clarification!

table(babies$mrace)
babies$mrace[babies$mrace == 10] = NA

table(babies$drace)
babies$drace[babies$drace == 10] = NA

#note also that "number" has a value at 9 which means "don't know".  We may want to make that missing too, although it is 
#possible that people who smoke many cigarettes do not report the value.  We also could leave it as a separate category if we ]
#treat number of cigarettes as a dummy variable.

table(babies$number)

#actually, only 4 people reported that they did not know, so let's treat it as missing since it won't matter much.

#given the large amount of missing data in dad's values for ht and wt, we won't use them
#otherwise we will drop the cases with missing values.  this implies that the reasons for 
#values being missing are completely random and not systematic.

#we will learn better methods for handling missing data in the unit on missing data

#let's create a dataset with no missing cases in the columns corresponding to all variables except dad's height and weight (columns 14 and 15 in the file)
babiescom = na.omit(babies[,-c(14, 15)])


### exploratory analyses  --  Premature is the outcome. 

#looks like smoking is a candidate for an important predictor
tapply(babiescom$Premature, babiescom$smoke, mean)

#mom and dad race seem to have similar patterns of Premature
tapply(babiescom$Premature, babiescom$mrace, mean)
tapply(babiescom$Premature, babiescom$drace, mean)

#in fact, mom and dad race are basically identical (if we lump all whites together)!
table(babiescom$mrace, babiescom$drace)
#we will only use mom race to avoid multi-collinearity

#similar issue with education, although not as obvious
tapply(babiescom$Premature, babiescom$med, mean)
tapply(babiescom$Premature, babiescom$ded, mean)
table(babiescom$med, babiescom$ded)

#probably we can use only of these, but we might start out with both...

# marital status  -- everyone is married!  let's not bother with this variable.
table(babiescom$marital)

#income -- not an obvious trend, but perhaps evidence that low income has higher risks. 
tapply(babiescom$Premature, babiescom$inc, mean)
#given the bounciness, we may want to include inc as a series of dummy variables.

#parity
table(babiescom$parity)
tapply(babiescom$Premature, babiescom$parity, mean)
#maybe just consider 0 versus all others in a dummy variable

###exploratory analysis for continuous variables

#load in the arm library

library(arm)

binnedplot(babiescom$mpregwt, babiescom$Premature, xlab = "Pregnancy Weight", ylab = "Premature", main = "Binned Prematurity and Pregnancy Weight") 
#suggestion of higher risk at lower weights.  Not obvious that one needs transformation

#mom height

binnedplot(babiescom$mht, babiescom$Premature, xlab = "Mother's Height", ylab = "Premature", main = "Binned Prematurity and Mother's Height") 
#suggestion of higher risk at lower heights. Not obvious that one needs transformation

#mom age
binnedplot(babiescom$mage, babiescom$Premature, xlab = "Age", ylab = "Premature", main = "Binned Prematurity and Mother's Age") 
#suggestion of higher risk at high ages and maybe low ages, but difficult to see. May need a transformation, perhaps quadratic.


###first attempt at a logistic regression

#lump all whites and mixed race into one category
#use only mom race and mom educ
#try dummy for parity: zero or greater than zero.

#mean center age
babiescom$magecent = babiescom$mage - mean(babiescom$mage)

#try quadratic trend in age
babiescom$magecent2 = babiescom$magecent^2

#make no HS degree the baseline. combine all categories for no HS degree, including unclear trade school ones. 

#mean center wt
babiescom$mwtcent = babiescom$mpregwt - mean(babiescom$mpregwt)

#if we need quadratic term later, could use this statement
#babiescom$mwtcent2 = babiescom$mwtcent^2

#mean center ht
babiescom$mhtcent = babiescom$mht - mean(babiescom$mht)

#if we need quadratic term later, could use this statement
#babies$mhtcent2 = babies$mht^2

#use dummy variables for income

#let's fit the logistic regression!
#we will make dummy variables in the regression call itself using the I( ... ) command.
#for example I(parity > 0) makes a dummy variable equal to one when parity > 0 and equation to 0 otherwise.

babiesreg1 = glm(Premature~ smoke + I(parity > 0) + I(mrace == 6) + I(mrace == 7) + I(mrace == 8) + magecent + magecent2 + I(med == 2) + I(med == 3) + I(med == 4) + I(med == 5) + mwtcent + mhtcent + as.factor(inc), family = binomial, data = babiescom)

##binned residual plots

# compute raw residuals
rawresid1 = babiescom$Premature - fitted(babiesreg1)

binnedplot(x=babiescom$mhtcent, y = rawresid1, xlab = "Mother's height centered", ylab = "Binned Residuals", main = "Binned residuals versus height")
#no obvious problems -- not sure why the function used so few groups.  maybe not a lot of diversity in height values

binnedplot(x=babiescom$mwtcent, y = rawresid1, xlab = "Mother's weight centered", ylab = "Binned Residuals", main = "Binned residuals versus weight")
#no obvious problems 

binnedplot(x=babiescom$magecent, y = rawresid1, xlab = "Mother's age centered", ylab = "Binned Residuals", main = "Binned residuals versus age")
#no obvious problems 

tapply(rawresid1, babiescom$smoke, mean) 
tapply(rawresid1, babiescom$parity>0, mean) 
tapply(rawresid1, babiescom$mrace==0, mean) 
tapply(rawresid1, babiescom$mrace==6, mean) 
tapply(rawresid1, babiescom$mrace==7, mean) 
tapply(rawresid1, babiescom$mrace==8, mean) 
tapply(rawresid1, babiescom$med==0, mean) 
tapply(rawresid1, babiescom$med==2, mean) 
tapply(rawresid1, babiescom$med==3, mean)
tapply(rawresid1, babiescom$med==4, mean)
tapply(rawresid1, babiescom$med==5, mean)   
tapply(rawresid1, babiescom$inc, mean) 

#no major problems with any of these, as all are pretty close to zero. 
#so far the model looks pretty good!

#ROC curve
roc(babiescom$Premature, fitted(babiesreg1), plot=T, legacy.axes=T)
#area under the curve of .66, so not a super strong fit but not terrible either!

#let's see if smoking is important - first create a new regression without smoking

babiesregnosmoke = glm(Premature~ I(parity > 0) + I(mrace == 6) + I(mrace == 7) + I(mrace == 8) + magecent + magecent2 + I(med == 2) + I(med == 3) + I(med == 4) + I(med == 5) + mwtcent + mhtcent + as.factor(inc), family = binomial, data = babiescom)

anova(babiesreg1, babiesregnosmoke, test = "Chisq")

#not quite significant, but certainly something to consider given the modest sample size

#how about income?
babiesregnoinc = glm(Premature~ smoke + I(parity > 0) + I(mrace == 6) + I(mrace == 7) + I(mrace == 8) + magecent + magecent2 + I(med == 2) + I(med == 3) + I(med == 4) + I(med == 5) + mwtcent + mhtcent, family = binomial, data = babiescom)

anova(babiesreg1, babiesregnoinc, test = "Chisq")

#income is not important in this data set!  Keep it anyways because people will ask if we controlled for income, so we can say yes!

#do we need the squared term?

babiesregnosq2 = glm(Premature~ smoke + I(parity > 0) + I(mrace == 6) + I(mrace == 7) + I(mrace == 8) + magecent  + I(med == 2) + I(med == 3) + I(med == 4) + I(med == 5) + mwtcent + mhtcent + as.factor(inc), family = binomial, data = babiescom)

anova(babiesreg1, babiesregnosq2, test = "Chisq")

#clearly not!  Let's get rid of them to simplify the model.

#ROC curve
roc(babiescom$Premature, fitted(babiesregnosq2), plot=T, legacy.axes=T)
#same area under the curve of .66, so more evidence that squared term did not matter


#since race seems to matter, let's see if an interaction of race with smoking is useful

babiesregnosq2int = glm(Premature~ smoke + I(parity > 0) + smoke*(I(mrace == 6) + I(mrace == 7) + I(mrace == 8)) + magecent  + I(med == 2) + I(med == 3) + I(med == 4) + I(med == 5) + mwtcent + mhtcent + as.factor(inc), family = binomial, data = babiescom)
anova(babiesregnosq2, babiesregnosq2int, test = "Chisq") 

#interactions not very useful -- don't bother with them

#let's call it and go with this final model.

confint.default(babiesregnosq2) 
exp(confint.default(babiesregnosq2))

#for interpretations, we can use the odds ratios from the exp(confint) results.  Or, we can compute predicted probablities, for example for smokers versus nonsmokers for different demographic groups. Predicting at the mean values of the continuous variables will make interpre