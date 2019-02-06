#R commands for analysis of the NC rivers data, the Pygmalion data, and the smelling data

##### NC rivers data

#read in data
NCriverdata = read.csv("mercurydata.txt", header = T)

#let's do some exploratory data analysis -- how many observations in each river? 
table(NCriverdata$river)

#histograms by river.  first tell R to use a 2 by 1 graph
par(mfcol = c(2,1))
hist(NCriverdata$mercury[NCriverdata$river=="lumber"], xlab = "Mercury", main = "Mercury in Fish in Lumber River")
hist(NCriverdata$mercury[NCriverdata$river=="wacamaw"], xlab = "Mercury", main = "Mercury in Fish in Wacamaw River")

#these histograms have similar shapes.  They have a bit of right skew, without any serious outliers.
#given the reasonably large sample sizes, the central limit theorem should be applicable for these data.
#hence, we can proceed with inference by confidence intervals or significance tests, both using the t-distribution.

#let's get mean and SD of mercury levels in each river sample
mean(NCriverdata$mercury[NCriverdata$river=="lumber"])
sd(NCriverdata$mercury[NCriverdata$river=="lumber"])
mean(NCriverdata$mercury[NCriverdata$river=="wacamaw"])
sd(NCriverdata$mercury[NCriverdata$river=="wacamaw"])

#let's get the 95% confidence interval in each river
t.test(NCriverdata$mercury[NCriverdata$river=="lumber"])
t.test(NCriverdata$mercury[NCriverdata$river=="wacamaw"])

#see the slides on CIs for interpretation.

#close graphics window for next analysis
dev.off()


####Pygmalion Data

#input data manually, since only twelve data points
none = c(3, 2, 6, 10, 11, 5)
accel = c(20, 10, 19, 15, 9, 18)
scores = c(none, accel)
condition = c(rep(0, 6), rep(1, 6))

#plot of data -- no need for histograms given small sample size
plot(y = scores, x = condition, xlab = "Treatment group (0 = none, 1 = accelerated)", ylab = "Changes in IQ", main = "IQ changes in both groups")

#let's get mean and SD of each group
mean(scores[condition==0])
sd(scores[condition==0])
mean(scores[condition==1])
sd(scores[condition==1])

#now get the 99% confidence interval (and more)
t.test(scores ~ condition, conf.level = .99)

####odors data
odors = read.csv("odors.txt", header = T)

#since this is a matched pairs dataset, we need to take the differences
odors$diff = odors$unscented - odors$scented

#histogram of results
hist(odors$diff, xlab = "Seconds", main = "Time difference (unscented - scented)")

#looks pretty close to a normal distribution.  Let's look at a QQ plot for more evidence, since sample size is only 21.
qqnorm(odors$diff, main = "Differences distribution versus normal distribution")

#The CLT theorem should be applicable for these data, since the distribution of the data is already close to normal.

#let's get mean and SD of time difference
mean(odors$diff)
sd(odors$diff)

# now for 95% interval
t.test(odors$diff)