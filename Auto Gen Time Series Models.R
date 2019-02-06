###code to simulate AR1 time series at T = 500 time points

#set T = the number of time points
T = 500

#make a time vector going from 1 to T for storing the outcomes
time = seq(1:T)

##generate the y values from an AR(1) process

#set up the vector of T values -- initialize at 0 for convenience
y = rep(0, T)

#variance of errors
sigma = 1

#mean of outcome
mu = 10

#draw the first time point from a normal distribution
y[1] = mu + rnorm(1, 0, sigma)
y[2] = alpha*y[1] + rnorm(1, 0, sigma)

#this is the autocorrelation
alpha = .9
alpha2  = .8

#create the rest of the outcomes from the AR(1) model
for(j in 2:T) {
  y[j] = mu + alpha*(y[j - 1] - mu) + rnorm(1, 0, sigma) + alpha2*(y[j-2]- mu) + rnorm(1, 0, sigma)
}

#time series plot
plot(x = time, y = y, xlab = "Time", ylab = "Outcome")

#a nicer plot can be obtained by converting y to a time series object
ytimeseries = ts(y)

#look at the object we created -- just shows the data
ytimeseries

#now make a nice time series plot
plot.ts(ytimeseries)

#we can see obvious patterns, which are evident in the autocorrelations

#autocorrelations and partial autocorrelations
acf(y)
pacf(y)

#we can try other values of alpha to get a sense of how AR(1) time series might look

