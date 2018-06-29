dat1 = read.csv("XOM.csv", header=TRUE)
names(dat1)
attach(dat1)
n           = dim(dat1)[1]
n0          = n-1
date        = 1:n0
date_i      = seq(from = 1, to = n0+1, by = 12)
lab_x       = seq(from = 2009, to = 2018, by = 1)
A_Return    = Adj.Close[2:n]/Adj.Close[1:(n-1)] - 1

dat2 = read.csv("DAL.csv", header=TRUE)
names(dat2)
attach(dat2)
n=dim(dat2)[1]
B_Return = Adj.Close[2:n]/Adj.Close[1:(n-1)] - 1

dat3 = read.csv("TB3MS.csv", header=TRUE)
names(dat3)
attach(dat3)
T_Return = TB3MS/100

plot(date,A_Return, ylim=range(c(-0.3,0.4)), type='l', col='blue',xaxt = 'n',xlab= "",ylab='Monthly Simple Returns',xlim=c(1,n))
par(new = TRUE) 
axis(1, at=date_i, labels=lab_x)
par(new = TRUE) 
plot(date,B_Return, ylim=range(c(-0.3,0.4)), axes = FALSE, xlab = "", ylab = "", type='l', col='red',xlim=c(1,n))
legend(x="topright", legend=c("XOM","DAL"), col=c("blue","red"), lwd=2)


mu_A     = mean(A_Return)

sigma_A  = var(A_Return)

SD_A     = var(A_Return)^.5

mu_B     = mean(B_Return)

sigma_B  = var(B_Return)

SD_B     = var(B_Return)^.5

sigma_AB = cov(A_Return, B_Return)

Cor_AB   = cov(A_Return, B_Return)/(var(A_Return)*var(B_Return))^.5

mu_T     = mean(T_Return) 

sigma_T  = var(T_Return)

SD_T     = var(T_Return)^.5

Cor_TB   = cov(T_Return, B_Return)/(var(T_Return)*var(B_Return))^.5

# Portfolio Frontier of Two Risky Assets

Port_Fron = function(mu_p, mu_A, mu_B, sigma_A, sigma_B, sigma_AB)
{

P_F = ((mu_p - mu_B)/(mu_A - mu_B))^2*sigma_A + ((mu_p - mu_A)/(mu_A - mu_B))^2*sigma_B - 2*(mu_p - mu_A)*(mu_p - mu_B)/((mu_A - mu_B))^2*sigma_AB


P_F^.5

}


x_minA    = (sigma_B-sigma_AB)/(sigma_A+sigma_B-2*sigma_AB)
mu_min    = x_minA*mu_A + (1-x_minA)*mu_B
sd_min    = Port_Fron(mu_min, mu_A, mu_B, sigma_A, sigma_B, sigma_AB)

mu_p1     = seq(from = 0, to = mu_min, by =0.0001)

sd_p1     = Port_Fron(mu_p1, mu_A, mu_B, sigma_A, sigma_B, sigma_AB)

mu_p2     = seq(from = mu_min, to = 0.04, by =0.0001)

sd_p2     = Port_Fron(mu_p2, mu_A, mu_B, sigma_A, sigma_B, sigma_AB)

#SD_A      = Port_Fron(mu_A, mu_A, mu_B, sigma_A, sigma_B, sigma_AB)

#SD_B      = Port_Fron(mu_B, mu_A, mu_B, sigma_A, sigma_B, sigma_AB)

# Plot the portfolio frontier

plot(sd_p1, mu_p1, type = "l", lwd = 3, col = "red", xlab = expression(sigma[p]),ylab = expression(mu[p]), xlim=range(c(0,0.2)), ylim=range(c(0,0.04)))
par(new=TRUE)
plot(sd_p2, mu_p2, type = "l", lwd = 3, col = "green", xlab = "", ylab = "", xlim=range(c(0,0.2)), ylim=range(c(0,0.04)))
par(new = TRUE)
p <- c(SD_A, mu_A)
points(t(p), pch=16, col = "red")
text(t(p), "XOM", lwd = 3, col = "red", adj = c(1.1,1.2))
par(new = TRUE)
p <- c(SD_B,mu_B)
points(t(p), pch=16, lwd = 3, col = "green")
text(t(p), "DAL", col = "green", adj = c(1.1,-0.8))

# Plot the portfolio frontier with the minimum variance portfolio

plot(sd_p1, mu_p1, type = "l", lwd = 3, col = "red", xlab = expression(sigma[p]),ylab = expression(mu[p]), xlim=range(c(0,0.2)), ylim=range(c(0,0.04)))
par(new=TRUE)
plot(sd_p2, mu_p2, type = "l", lwd = 3, col = "green", xlab = "", ylab = "", xlim=range(c(0,0.2)), ylim=range(c(0,0.04)))
par(new = TRUE)
p <- c(SD_A, mu_A)
points(t(p), pch=16, col = "red")
text(t(p), "XOM", lwd = 3, col = "red", adj = c(1.1,1.2))
par(new = TRUE)
p <- c(SD_B,mu_B)
points(t(p), pch=16, lwd = 3, col = "green")
text(t(p), "DAL", col = "green", adj = c(1.1,-0.8))
par(new = TRUE)
p <- c(sd_min,mu_min)
points(t(p), pch=16, lwd = 3, col = "black")
text(t(p), "Min_V Portfolio", col = "black", adj = c(-0.1,0.0))


# Portfolio Frontier of Risky and Riskless Assets

sig_x    = seq(from = 0, to = 0.20, by = 0.001)

line_A   = mu_T + ((mu_A - mu_T)/sigma_A^.5)*sig_x

line_B   = mu_T + ((mu_B - mu_T)/sigma_B^.5)*sig_x

plot(sig_x,line_A, ylim=range(c(0,0.04)),lwd = 3, xlab = expression(sigma[p]), ylab = expression(mu[p]), type='l', col='red')
par(new = TRUE)
p <- c(SD_T, mu_T)
points(t(p), pch=16, lwd = 3,col = "blue")
text(t(p), "T-Bill", col = "blue", adj = c(-0.1,1.2))
par(new = TRUE)
p <- c(SD_A, mu_A)
points(t(p), pch=16, lwd = 3, col = "red")
text(t(p), "XMO", col = "red", adj = c(-0.1,1.2))

 
plot(sig_x,line_B, ylim=range(c(0,0.04)), lwd = 3, xlab = expression(sigma[p]), ylab = expression(mu[p]), type='l',  col='green')
par(new = TRUE)
p <- c(SD_B, mu_B)
points(t(p), pch=16, lwd = 3,col = "green")
text(t(p), "DAL", col = "green", adj = c(1.1,-0.5))
par(new = TRUE)
p <- c(SD_T, mu_T)
points(t(p), pch=16, lwd = 3,col = "blue")
text(t(p), "T-Bill", col = "blue", adj = c(-0.1,1.2))


plot(sig_x,line_A, ylim=range(c(0,0.04)), lwd = 3,xlab = expression(sigma[p]), ylab = expression(mu[p]), type='l', col='red')
par(new = TRUE)
p <- c(SD_T, mu_T)
points(t(p), pch=16, lwd = 3,col = "blue")
text(t(p), "T-Bill", col = "blue", adj = c(-0.1,1.2))
par(new = TRUE)
p <- c(SD_A, mu_A)
points(t(p), pch=16, lwd = 3,col = "red")
text(t(p), "XOM", col = "red", adj = c(-0.1,1.2))
par(new = TRUE)
plot(sig_x,line_B, lwd = 3,ylim=range(c(0,0.04)), xlab = "", ylab = "", type='l', col='green')
par(new = TRUE)
p <- c(SD_B, mu_B)
points(t(p), pch=16, lwd = 3,col = "green")
text(t(p), "DAL", col = "green", adj = c(1.1,-0.5))


# Portfolio Frontier of Two Risky Assets and One Riskless Assets

plot(sig_x,line_A, xlim=range(c(0,0.2)), lwd = 3, ylim=range(c(0,0.04)),xlab = expression(sigma[p]), ylab = expression(mu[p]), type='l', col='red')
par(new = TRUE)
p <- c(SD_T, mu_T)
points(t(p), pch=16, lwd = 3, col = "blue")
text(t(p), "T-Bill", col = "blue", adj = c(-0.1,1.2))
par(new = TRUE)
p <- c(SD_A, mu_A)
points(t(p), pch=16, lwd = 3, col = "red")
text(t(p), "XMO", col = "red", adj = c(-0.4,0.8))
par(new = TRUE)
plot(sig_x, line_B, lwd = 3, xlim=range(c(0,0.2)), ylim=range(c(0,0.04)), xlab = "", ylab = "", type='l', col='red')
par(new = TRUE)
p <- c(SD_B, mu_B)
points(t(p), pch=16, lwd = 3, col = "green")
text(t(p), "DAL", col = "green", adj = c(1.1,-0.5))
par(new = TRUE)
plot(sd_p1, mu_p1, type = "l", lwd = 3, col = "black", xlab = expression(sigma[p]),ylab = expression(mu[p]), xlim=range(c(0,0.2)), ylim=range(c(0,0.04)))
par(new=TRUE)
plot(sd_p2, mu_p2, type = "l", lwd = 3, col = "black", xlab = "", ylab = "", xlim=range(c(0,0.2)), ylim=range(c(0,0.04)))
 

 
A1     = (mu_A - mu_T)*sigma_B
A2     = (mu_B - mu_T)*sigma_AB
A3     = (mu_B - mu_T)*sigma_A
A4     = (mu_A - mu_T + mu_B - mu_T)*sigma_AB

xtan_A = ( A1 - A2 )/( A1 + A3 - A4 )
mu_tan = xtan_A*mu_A + (1-xtan_A)*mu_B
SD_tan = Port_Fron(mu_tan, mu_A, mu_B, sigma_A, sigma_B, sigma_AB)
sigma_tan = SD_tan^2
line_T = mu_T + ((mu_tan - mu_T)/SD_tan)*sig_x


plot(sig_x,line_A, lwd = 3, xlim=range(c(-0.05,0.2)), ylim=range(c(0,0.04)), xlab = "", ylab = "", type='l', col='red')
par(new = TRUE)
p <- c(SD_T, mu_T)
points(t(p), pch=16, lwd = 3, col = "blue")
text(t(p), "T-Bill", col = "blue", adj = c(-0.1,0.8))
par(new = TRUE)
p <- c(SD_A, mu_A)
points(t(p), pch=16, lwd = 3, col = "red")
text(t(p), "XOM", col = "red", adj = c(-0.3,0.7))
par(new = TRUE)
plot(sig_x, line_B, lwd = 3, xlim=range(c(-0.05,0.2)), ylim=range(c(0,0.04)), xlab = "", ylab = "", type='l', col='red')
par(new = TRUE)
p <- c(SD_B, mu_B)
points(t(p), pch=16, lwd = 3, col = "red")
text(t(p), "DAL", col = "red", adj = c(-0.2,0.8))
par(new = TRUE)
plot(sd_p1, mu_p1, lwd = 3, type = "l", col = "black", xlab = expression(sigma[p]),ylab = expression(mu[p]), xlim=range(c(-0.05,0.2)), ylim=range(c(0,0.04)))
par(new=TRUE)
plot(sd_p2, mu_p2, lwd = 3, type = "l", col = "black", xlab = "", ylab = "", xlim=range(c(-0.05,0.2)), ylim=range(c(0,0.04)))
par(new = TRUE)
plot(sig_x,line_T, lwd = 3, xlim=range(c(-0.05,0.2)), ylim=range(c(0,0.04)), xlab = "", ylab = "", type='l', col='green')
par(new = TRUE)
p <- c(SD_tan, mu_tan)
points(t(p), pch=16, lwd = 3, col = "green")
text(t(p), "Tangency-Portfolio", col = "green", adj = c(1.05,-0.2))


plot(sd_p1, mu_p1, lwd = 3, type = "l", col = "black", xlab = expression(sigma[p]),ylab = expression(mu[p]), xlim=range(c(-0.05,0.2)), ylim=range(c(0,0.04)))
par(new=TRUE)
plot(sd_p2, mu_p2, lwd = 3, type = "l", col = "black", xlab = "", ylab = "", xlim=range(c(-0.05,0.2)), ylim=range(c(0,0.04)))
par(new = TRUE)
p <- c(SD_T, mu_T)
points(t(p), pch=16, lwd = 3, col = "blue")
text(t(p), "T-Bill", col = "blue", adj = c(-0.1,0.8))
par(new = TRUE)
p <- c(SD_A, mu_A)
points(t(p), pch=16, lwd = 3, col = "red")
text(t(p), "CVX", col = "red", adj = c(-0.3,0.7))
par(new = TRUE)
p <- c(SD_B, mu_B)
points(t(p), pch=16, lwd = 3, col = "red")
text(t(p), "UAL", col = "red", adj = c(-0.2,0.8))
par(new = TRUE)
plot(sig_x,line_T, lwd = 3, xlim=range(c(-0.05,0.2)), ylim=range(c(0,0.04)), xlab = "", ylab = "", type='l', col='green')
par(new = TRUE)
p <- c(SD_tan, mu_tan)
points(t(p), pch=16, lwd = 3, col = "green")
text(t(p), "Tangency-Portfolio", col = "green", adj = c(1.05,-0.2))

# Problem 1

x_tan = (sigma_A/sigma_tan)^.5

# Problem 2

x_tan = ( mu_A - mu_T )/( mu_tan - mu_T )

# Out of Sample

dat1 = read.csv("XOM.csv", header=TRUE)
names(dat1)
attach(dat1)
n          = dim(dat1)[1] 
A_Return   = Adj.Close[2:n]/Adj.Close[1:(n-1)] - 1

dat2 = read.csv("DAL.csv", header=TRUE)
names(dat2)
attach(dat2)
n=dim(dat2)[1]
B_Return = Adj.Close[2:n]/Adj.Close[1:(n-1)] - 1

dat3 = read.csv("TB3MS.csv", header=TRUE)
names(dat3)
attach(dat3)
T_Return = TB3MS/100

W0       = 10000
n        = dim(dat)[1]
r1       = A_Return
r2       = B_Return
r3       = T_Return
S        = n-1
T        = 59
N_Exp    = S - T
L1       = rep(0,N_Exp)
L2       = rep(0,N_Exp)
L_T      = rep(0,N_Exp)
L_A      = rep(0,N_Exp)

for (s in 1:N_Exp)
{

s1     = s+T-1

Dat    = A_Return[s:s1]

mu_A   = mean(Dat)

V_A    = (mean(Dat^2) - (mean(Dat))^2)*T/(T-1)

SD_A   = V_A^.5

Dat    = B_Return[s:s1]

mu_B   = mean(Dat)

V_B    = (mean(Dat^2) - (mean(Dat))^2)*T/(T-1)

SD_B   = V_B^.5

V_AB   = cov(A_Return[s:s1], B_Return[s:s1])

Dat    = T_Return[s:s1]

mu_T   = mean(Dat)

V_T    = (mean(Dat^2) - (mean(Dat))^2)*T/(T-1)

SD_T   = V_T^.5

A1     = (mu_A - mu_T)*V_B
A2     = (mu_B - mu_T)*V_AB
A3     = (mu_B - mu_T)*V_A
A4     = (mu_A - mu_T + mu_B - mu_T)*V_AB
xtan_A = ( A1 - A2 )/( A1 + A3 - A4 )
mu_tan = xtan_A*mu_A  + (1 - xtan_A)*mu_B
V_tan  = xtan_A^2*V_A + (1 - xtan_A)^2*V_B + 2*xtan_A*(1 - xtan_A)*V_AB
SD_tan = V_tan^.5


L_T[s] = W0*(xtan_A*A_Return[s+T]+ (1-xtan_A)*B_Return[s+T])

x_tan1 = SD_A/SD_tan
x_T1   = 1 - x_tan1
x_A1   = x_tan1*xtan_A
x_B1   = x_tan1*(1-xtan_A)
L1[s]  = W0*(x_T1*T_Return[s+T] + x_A1*A_Return[s+T]+ x_B1*B_Return[s+T])

x_tan2 = (mu_A - mu_T)/(mu_tan - mu_T)
x_T2   = 1 - x_tan2
x_A2   = x_tan2*xtan_A
x_B2   = x_tan2*(1-xtan_A)
L2[s]  = W0*(x_T2*T_Return[s+T] + x_A2*A_Return[s+T]+ x_B2*B_Return[s+T])

L_A[s] = W0*A_Return[s+T]

}


date      = 1:N_Exp
date_i    = seq(from = 1, to = 60, by = 12)
lab_x     = seq(from = 2014, to = 2018, by = 1)
 


# Plot the Profits of the Portfolios

plot(date, L1, type ="l", col = "blue", xaxt = 'n', ylab="", xlim=range(c(1,N_Exp)),ylim=range(c(-1500,2000)))
axis(1, at=date_i, labels=lab_x)
par(new=TRUE)
plot(date, L_A, type ="l", col = "red", xaxt = 'n', ylab="", xlim=range(c(1,N_Exp)),ylim=range(c(-1500,2000)))
legend(x="topright", legend=c("L1 (SD=382.73)", "LA (SD=420.46)"), lty=1, lwd=2, col=c("blue", "red"))
abline(h=mean(L1), col = "blue")
abline(h=mean(L_A),col = "red")
var(L1)^.5
var(L_A)^.5

plot(date, L2, type ="l", col = "green", xaxt = 'n', ylab="", xlim=range(c(1,N_Exp)),ylim=range(c(-1500,2000)))
axis(1, at=date_i, labels=lab_x)
par(new=TRUE)
plot(date, L_A, type ="l", col = "red", xaxt = 'n', ylab="", xlim=range(c(1,N_Exp)),ylim=range(c(-1500,2000)))
legend(x="topright", legend=c("L2 (SD=138.60)", "LA (SD=420.46)"), lty=1, lwd=2, col=c("green", "red"))
abline(h=mean(L2), col = "green")
abline(h=mean(L_A),col = "red")
var(L2)^.5
var(L_A)^.5

 
 




