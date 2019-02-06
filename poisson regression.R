#Poisson regression for elephant mating data (Case 22.01 in The Statisticsl Sleuth, 2nd edition)

elephants = read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/elephants.csv", header = T)

#make names lower case for ease of typing
names(elephants) = c("age", "matings")

plot(y=elephants$matings, x = elephants$age, xlab = "Age", ylab="Number of matings", main = "Matings versus Age for Elephants Data")

elephants$agec = elephants$age - mean(elephants$age)

elephreg = glm(matings~agec, data = elephants, family = "poisson")

summary(elephreg)

confint.default(elephreg)

#diagnostics
elephregresid = resid(elephreg, type = "pearson")

plot(y=elephregresid, x=elephants$agec, xlab = "Age", ylab = "Pearson Residuals")
abline(0,0)

#maybe, maybe, maybe a hint of quadratic trend.  let's try to fit quadratic term for age.

elephants$agec2 = elephants$agec^2

elephreg2 = glm(matings~agec + agec2, data = elephants, family = "poisson")

summary(elephreg2)

#test if quadratic effect is useful by change of deviance test.
anova(elephreg, elephreg2, test = "Chisq")

#quadratic term not a useful predictor.  drop it to simplify model.

#residual plot looks pretty good, so let's go with this model.

#interpretation of age coefficient: multiplicative change in expected count when increasing X by one year

exp(.069)
exp(confint.default(elephreg))