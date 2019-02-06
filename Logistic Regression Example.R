### Exploration of binned residual plots for logistic regression

library(arm)

# we use simulated datasets to see what binned residual plots can reveal about the fit
# of a logistic regression

#let's make an arbitrary predictor, say X, generated to be always positive
#use a uniform distribution on 1 to 10, taking n = 500 random draws.

n = 5000
X = runif(n, 1, 10)

#now let's make a logistic regression function

beta0 = -3
beta1 = 0.5
p = exp(beta0 + beta1*X)/(1 + exp(beta0 + beta1*X))

#take a look at values of p
summary(p)

#now generate values of Y from Bernoulli distributions with probabilities in p

Y = rbinom(n, 1, p)

#look at Y
mean(Y)

goodfit = glm(Y~X, family = "binomial")

summary(goodfit)

predprobsgoodfit = predict(goodfit, type = "response")
rawresidsgoodfit = Y - predprobsgoodfit

binnedplot(x= predprobsgoodfit, y= rawresidsgoodfit,  xlab = "Predicted Probability")

#you see no systematic patterns, suggesting the model describes the data reasonably well
#that's good, since we actually fit the right model!

#now let's see what happens if we needed a squared term but forgot to include it.

beta0 = -3
beta1 = 1
beta2 = -.1
p = exp(beta0 + beta1*X + beta2*X^2)/(1 + exp(beta0 + beta1*X + beta2*X^2))

#take a look at values of p
summary(p)

#now generate values of Y from Bernoulli distributions with probabilities in p

Y = rbinom(n, 1, p)

#look at Y
mean(Y)

badfit = glm(Y~X, family = "binomial")

summary(badfit)

predprobsbadfit = predict(badfit, type = "response")
rawresidsbadfit = Y - predprobsbadfit

binnedplot(x= predprobsbadfit, y= rawresidsbadfit,  xlab = "Predicted Probability")

#predictions are really bad for large values of X.  We have a lot of negative residuals,
#suggesting that the predicted probabilities are way too high.

#what happens when we add the quadratic term?

Xsq = X^2
goodfitquad = glm(Y ~ X + Xsq, family = "binomial")

summary(goodfitquad)

predprobsgoodfitquad = predict(goodfitquad, type = "response")
rawresidsgoodfitquad = Y - predprobsgoodfitquad

binnedplot(x= predprobsgoodfitquad, y= rawresidsgoodfitquad,  xlab = "Predicted Probability")


#one more example -- let's see what happens when we need to use log(X) as the predictor

beta0 = -3
beta1 = 2
p = exp(beta0 + beta1*log(X))/(1 + exp(beta0 + beta1*log(X)))

#take a look at values of p
summary(p)

#now generate values of Y from Bernoulli distributions with probabilities in p

Y = rbinom(n, 1, p)

#look at Y
mean(Y)

badfit = glm(Y~X, family = "binomial")

summary(badfit)

predprobsbadfit = predict(badfit, type = "response")
rawresidsbadfit = Y - predprobsbadfit

binnedplot(x= predprobsbadfit, y= rawresidsbadfit,  xlab = "Simulated X value")

#predictions are really bad for large values of X.  We have a lot of negative residuals,
#suggesting that the predicted probabilities are way too high.

#what happens when we use log(X)?

logX = log(X)
goodfitlog = glm(Y ~ logX, family = "binomial")

summary(goodfitlog)

predprobsgoodfitlog = predict(goodfitlog, type = "response")
rawresidsgoodfitlog = Y - predprobsgoodfitlog

binnedplot(x= predprobsgoodfitlog, y= rawresidsgoodfitlog,  xlab = "Simulated X value")

