# econ147lab3.R			script file for econ 147 Homework 3 calculations

options(digits=4)
library(mvtnorm)

# bivariate normal distribution
mu.x = 0.01
sig.x = 0.25
mu.y = 0.05
sig.y = 0.15

# simulate from bivariate normal with a given rho 
rho.xy = -0.5
sig.xy = rho.xy*sig.x*sig.y
Sigma.xy = matrix(c(sig.x^2, sig.xy, sig.xy, sig.y^2), 2, 2, byrow=TRUE)

# use the rmvnorm() function to simulate from bivariate normal
?rmvnorm
n = 100
set.seed(123)
xy.vals = rmvnorm(n, mean=c(mu.x, mu.y), sigma=Sigma.xy) 
head(xy.vals)

# scatterplot
plot(xy.vals[,1], xy.vals[,2], pch=16, cex=2, col="blue", 
     xlab="x", ylab="y")
title("Bivariate normal: rho=?")
abline(h=mu.y, v=mu.x)
segments(x0=0, y0=min(xy.vals[,2]), x1=0, y1=0, col="red")
segments(x0=min(xy.vals[,1]), y0=0, x1=0, y1=0, col="red")

# compute area under bivariate standard normal distribution
# Finc P( -00 < X < 0 and -00 < Y < 0)
?pmvnorm
pmvnorm(lower=c(-Inf, -Inf), upper=c(0, 0), mean=c(mu.x, mu.y), sigma=Sigma.xy)


