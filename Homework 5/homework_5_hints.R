# Econ 147 HW5 Hints # 
# Make sure that you have installed the following packages
# install.packages("PerformanceAnalytics")
# install.packages("quantmod")
# install.packages("TSA")
# install.packages("car") 
# install.packages("tseries") 


library(PerformanceAnalytics)
library(quantmod)
library(TSA)
library(car) 
library(tseries) 

options(digits=4)

symbols = c("MSFT", "^GSPC") # microsoft and S&P500, there are thousands of other domestic and international symbols such as ^ftse (FTSE 100) and ^N225 (Nikkei 225).

getSymbols(symbols, from ="2000-01-03", to = "2014-02-21")
# check data
colnames(MSFT)
start(MSFT)
end(MSFT)

# extract adjusted closing prices
MSFT = MSFT[, "MSFT.Adjusted", drop=F]
GSPC = GSPC[, "GSPC.Adjusted", drop=F]

# calculate log-returns for GARCH analysis
MSFT.ret = CalculateReturns(MSFT, method="compound")
GSPC.ret = CalculateReturns(GSPC, method="compound")

# remove default column header which is treated as the first observation
MSFT.ret = MSFT.ret[-1,]
GSPC.ret = GSPC.ret[-1,]
# rename column
colnames(MSFT.ret) ="MSFT" 
colnames(GSPC.ret) ="GSPC"


# plot returns
plot(MSFT.ret) #getSymbols()has some built-in graphical features that apply to plot()
plot(GSPC.ret, main="SP500 daily returns")


#plot descriptive statistics: Microsoft 
par(mfrow=c(2,2))
plot(MSFT.ret, main="MSFT daily returns")
acf(MSFT.ret, main="ACF", lwd=2)
plot(density(MSFT.ret),type="l", main="Smoothed density", xlab="daily returns", ylab="density estimate", col="slateblue1")
qqnorm(MSFT.ret, col="slateblue1")
qqline(MSFT.ret)
par(mfrow=c(1,1))


#plot descriptive statistics: S&P 500
par(mfrow=c(2,2))
plot(GSPC.ret, main="SP500 daily returns")
acf(GSPC.ret, main="ACF", lwd=2)
plot(density(GSPC.ret),type="l", main="Smoothed density", xlab="daily returns", ylab="density estimate", col="slateblue1")
qqnorm(GSPC.ret, col="slateblue1")
qqline(GSPC.ret)
par(mfrow=c(1,1))

# Testing Normality 
?jarque.bera.test # Also review pp.49-51 of CER_StatisticalInference_Slides
jarque.bera.test(MSFT.ret)
jarque.bera.test(GSPC.ret)

############ conditional maximum likelihood estimation #################
# GARCH(1,1) on MSFT
?garch
garch_AP  <- garch(MSFT.ret, order = c(1,1))
n  = dim(MSFT.ret)[1]
n0 = n-1
summary(garch_AP)
attach(garch_AP)
# The estimation results are in garch_AP
# coef: contains the estimators of omega, alpha1 and beta1
# vcov: contains the variance estimators of the estimators of omega, alpha1 and beta1
# fitted.values: contains the fitted conditioinal standard deviation sigma_t
# residuals: contains the fitted residuals for e_t 

omega_est  = coef[1]                   # estimator of omega
sd_omega   = vcov[1,1]^.5              # standard deviation estimator of the estimator of omega
alpha1_est = coef[2]                   # estimator of alpha1
sd_alpha1  = vcov[2,2]^.5              # standard deviation estimator of the estimator of alpha1
beta1_est  = coef[3]                   # estimator of beta1
sd_beta1   = vcov[3,3]^.5              # standard deviation estimator of the estimator of beta1
sig        = fitted.values[2:n0,c(1)]  # fitted conditioinal standard deviation sigma_t
rsd        = residuals[2:n0]           # fitted residuals for e_t 
fit.r      = sig*rsd                   # fitted cc return for r_t
n1         = n0-1
date       = 1:n1
date_i     = seq(from = 1, to = n0, by = 250)
lab_x      = seq(from = 2000, to = 2014, by = 1)

# plot the real cc return and the fitted cc return
par(mfrow=c(2,1))
par(mar = c(3,4.5,1.5,2))
plot(date, MSFT.ret[3:n], type='l',col='blue',xaxt = 'n', xlab= "",ylab="", xlim=c(1,n1), ylim=c(-.15,.15))
axis(1, at=date_i, labels=lab_x) 
title('Real Data (MSFT)')
plot(date,fit.r, type='l',col='green',xaxt = 'n', xlab= "",ylab="", xlim=c(1,n1), ylim=c(-.15,.15))
axis(1, at=date_i, labels=lab_x) 
title('Fitted Data (MSFT)')


# GARCH(1,1) with GSPC
garch_AP  <- garch(GSPC.ret, order = c(1,1))
n  = dim(MSFT.ret)[1]
n0 = n-1
summary(garch_AP)
attach(garch_AP)
# The estimation results are in garch_AP
# coef: contains the estimators of omega, alpha1 and beta1
# vcov: contains the variance estimators of the estimators of omega, alpha1 and beta1
# fitted.values: contains the fitted conditioinal standard deviation sigma_t
# residuals: contains the fitted residuals for e_t 

omega_est  = coef[1]                   # estimator of omega
sd_omega   = vcov[1,1]^.5              # standard deviation estimator of the estimator of omega
alpha1_est = coef[2]                   # estimator of alpha1
sd_alpha1  = vcov[2,2]^.5              # standard deviation estimator of the estimator of alpha1
beta1_est  = coef[3]                   # estimator of beta1
sd_beta1   = vcov[3,3]^.5              # standard deviation estimator of the estimator of beta1
sig        = fitted.values[2:n0,c(1)]  # fitted conditioinal standard deviation sigma_t
rsd        = residuals[2:n0]           # fitted residuals for e_t 
fit.r      = sig*rsd                   # fitted cc return for r_t
n1         = n0-1
date       = 1:n1
date_i     = seq(from = 1, to = n0, by = 250)
lab_x      = seq(from = 2000, to = 2014, by = 1)

# plot the real cc return and the fitted cc return
par(mfrow=c(2,1))
par(mar = c(3,4.5,1.5,2))
plot(date, GSPC.ret[3:n], type='l',col='blue',xaxt = 'n', xlab= "",ylab="", xlim=c(1,n1), ylim=c(-.1,.1))
axis(1, at=date_i, labels=lab_x) 
title('Real Data (GSPC)')
plot(date,fit.r, type='l',col='green',xaxt = 'n', xlab= "",ylab="", xlim=c(1,n1), ylim=c(-.1,.1))
axis(1, at=date_i, labels=lab_x) 
title('Fitted Data (GSPC)')

