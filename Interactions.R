####example of data analysis with interactions

#ozone data problem 10.26 in Statistical Sleuth Version 2

#all the datasets from Sleuth 2 are in the R package Sleuth2
#install it using install.packages("Sleuth2")
#then type library(Sleuth2). the data file is named ex1026.
#let's rename the data file to ozone
install.packages("Sleuth2")
library(Sleuth2)
ozone = ex1026

#good idea to see what size data you are working with

dim(ozone)
#[1] 17  3

#it's small!  but that's useful for demonstrating interactions.

#now let's see the names of the variables
ozone[1,]
#  Inhibit UVB Surface
#1       0   0    DEEP

#now a quick look at the distributions of the variables
summary(ozone)

#there are zeros in Inhibit. IF we want to take logs, we will have to add .5 to the values.  This is a small number compared to 
#the non-zero values of Inhibit. 

#let's start plotting

plot(Inhibit~UVB, ylab = "Inhibit", xlab  = "UVB", data=ozone)
boxplot(Inhibit~Surface, data=ozone)

#take a look at trellis plots using the lattice library to see if we find any evidence of interaction effects
#load the lattice library if you have not done so in your R session yet
library(lattice)

#alternatively, you can use ggplot to make these trellis plots.  either is fine.

xyplot(Inhibit~UVB | Surface, data = ozone) 
#clearly interactions are suggested.

#let's create a mean-centered version of UVB
ozone$UVBrealcent = ozone$UVB - mean(ozone$UVB)

#make the dummy variable for Surface and its interaction with UVB
ozone$Surf = rep(0, 17)
ozone$Surf[ozone$Surface == "Surface"] = 1
ozone$UVBSurf = ozone$UVBrealcent * ozone$Surf

#here is the regression

regoz1a = lm(Inhibit~UVBrealcent + Surf + UVBSurf, data = ozone)

#here is the regression with short cuts for creating the dummy variable
#and interaction.  the use of a * tells R to make an interaction plus the main effects for both variables

regoz1 = lm(Inhibit~UVBrealcent*as.factor(Surface), data = ozone)

#Results are equivalent.  We'll use the second one for no other reason than we fit it last.

plot(y=regoz1$residual, x=ozone$UVBrealcent, xlab = "UVB Centered", ylab = "Residual")
abline(0,0)
boxplot(regoz1$residual~ ozone$Surface)

#after fitting regression, look for interaction effects in residual plots.
#if you see different patterns in residuals vs. some predictor based on values of another predictor, try an interaction effect in the regression model
#this will be more useful in the diamonds data analysis, since we have more than 2 predictors there.

xyplot(regoz1$residual~ ozone$UVBrealcent | ozone$Surface) 

#residual plots look good.  Let's go with this model.

summary(regoz1)

#Call:
#lm(formula = Inhibit ~ UVBrealcent * as.factor(Surface), data = ozone)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-16.8107  -3.1507  -0.4999   2.2825  20.3308 

#Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                             26.535      3.071   8.641 9.52e-07 ***
#UVBrealcent                           1238.976    222.963   5.557 9.28e-05 ***
#as.factor(Surface)Surface              -18.335      5.323  -3.444  0.00436 ** 
#UVBrealcent:as.factor(Surface)Surface -980.039    381.539  -2.569  0.02335 *  
#---
#Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1 
#
#Residual standard error: 8.521 on 13 degrees of freedom
#Multiple R-squared: 0.7289,     Adjusted R-squared: 0.6663 
#F-statistic: 11.65 on 3 and 13 DF,  p-value: 0.0005498 


#let's see what happens if we switch the baseline to Surface

ozone$Deep = rep(0, 17)
ozone$Deep[ozone$Surface == "Deep"] = 1
ozone$UVBDeep = ozone$UVBrealcent * ozone$Deep

#here is the regression using Surface as the baseline

regoz1b = lm(Inhibit~UVBrealcent + Deep + UVBDeep, data = ozone)

summary(regoz1b)

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)   
#(Intercept)    8.199      4.348   1.886  0.08189 . 
#UVBrealcent  258.936    309.612   0.836  0.41808   
#Deep          18.335      5.323   3.444  0.00436 **
#UVBDeep      980.039    381.539   2.569  0.02335 * 
#---
#Signif. codes:  
#0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 8.521 on 13 degrees of freedom
#Multiple R-squared:  0.7289,    Adjusted R-squared:  0.6663 
#F-statistic: 11.65 on 3 and 13 DF,  p-value: 0.0005498

#notice the R^2 and residual SE are identical -- we have not changed the quality of the fit
#the interpretation changes now, since the baseline is surface,
#notice the slope for UVBrealcent for Surface is 258.936.  This is the same as the slope
#from the regression with Deep as the baseline, since we have slope_Surface = (1238.976 - 980.039)


#let's test if Surface is at all a useful predictor here.
regoz2 = lm(Inhibit~UVBrealcent, data = ozone)

#here is a command to do the nested F test.
anova(regoz2, regoz1)

#Analysis of Variance Table
#
#Model 1: Inhibit ~ UVBrealcent
#Model 2: Inhibit ~ UVBrealcent * as.factor(Surface)
#  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1     15 2797.8                                  
#2     13  943.8  2      1854 12.768 0.0008559 ***
#---
#Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1 

#Surface is clearly a useful predictor.

#how can we display the relationship?

plot(y = ozone$Inhibit, x = ozone$UVBrealcent, xlab = "Difference in UVB from mean (0.02)", ylab = "Percent Inhibition", main = "Relationship of Inhibition with UVB exposure")
abline(26.525, 1238.976, lty=1)
abline(26.535 - 18.335, 1238.976 - 980.039, lty = 2)
#note that the dashed line is for the Surface, and solid line is for DEEP.