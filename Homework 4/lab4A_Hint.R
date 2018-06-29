# econ147lab4A_Hint.R     script file for econ 147 Homework 4 R Exercises A calculations
#

#
# simulate time series data
#

# simulate MA(1) process with theta > 0

ma1.model.5 = list(ma=0.5)
mu = 0.05
set.seed(123)
ma1.sim.5 = mu + arima.sim(model=ma1.model.5, n=250,
                         innov=rnorm(n=250, mean=0, sd=0.1))
acf.ma1.model.5 = ARMAacf(ma=0.9, lag.max=10)

par(mfrow=c(3,1))
	ts.plot(ma1.sim.5, main="MA(1) Process: mu=0.05, theta=0.9",
	       xlab="time",ylab="y(t)")
	abline(h=0)
	plot(1:10, acf.ma1.model.5[2:11], type="h", col="blue", main="theoretical ACF")
	tmp=acf(ma1.sim.5, lag.max=10, main="Sample ACF")
par(mfrow=c(1,1))

# do the same for the the MA model with theta = 0.9 and -0.9

# simulate AR(1) process with phi > 0
ar1.model.5 = list(ar=0.99)
mu = 0.05
set.seed(123)
ar1.sim.5 = mu + arima.sim(model=ar1.model.5, n = 250,
                         innov=rnorm(n=250, mean=0, sd=0.1))
acf.ar1.model.5 = ARMAacf(ar=0.99, lag.max=10)

par(mfrow=c(3,1))
	ts.plot(ar1.sim.5,main="AR(1) Process: mu=0.05, phi=0.99",
	       xlab="time",ylab="y(t)")
	abline(h=0)
	plot(1:10, acf.ar1.model.5[2:11], type="h", col="blue", main="Theoretical ACF")
	tmp=acf(ar1.sim.5, lag.max=10, main="Sample ACF")
par(mfrow=c(1,1))

# do the same for the model with phi = 0.9




