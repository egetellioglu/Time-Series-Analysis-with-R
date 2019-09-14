
rm(list = ls())

setwd("C:\\Users\\user\\Documents\\Time-Series-Analysis-with-R")
getwd()

# Exercise 1

# Simulation of two time series for Q1 & Q2

MyMatrNr=6811992      
set.seed(MyMatrNr)  

# The generated data are unified for me

# Time series X1 following an AR(4)
n1=1200                       # number of observations for X1
X1=arima.sim(n1, model=list(ar=c(0.4, -0.3, 0.0, 0.5)))*5

# Display the time series X1. Based on this gure (please do not use the acf-graphic). Try
# to explain that those observations depend on each other.

par(mfrow=c(1,1))
plot.ts(X1)
title("Time Series X1")

# Fit AR(p) models to X

BIC1=(0:15)*0
p.all=(0:15)
for(p in 0:15)
{X.fit=arima(X1,order=c(p,0,0))
BIC1 [p+1]=-2*X.fit$loglik+log(n1)*(p)}
BIC1
plot(p.all, BIC1,type="l",col=1,xlab="p",ylab="BIC")
title ("BIC against p")
p.opt=which.min(BIC1)-1
abline(v=p.opt,lty=3,lwd=1, col=2)
p.opt

# Calculate the AR(p) model for X again and state the fitted model

X.fit.opt=arima(X1,order=c(p.opt,0,0))
summary(X.fit.opt)
X.fit.opt

# Exercise 2

# Find some daily nancial time series within an observation period of about 5 years.
# Display the observations and the return
# series for this time series.

xx=read.csv("AAPL.csv", header=TRUE)
X=xx[,7]
n1=length(X)
X=xx[n1:1,7]	#### reverse the data
Apple.returns=diff(log(X))

# Plot the Apple series
Year1=(1:n1)/n1*(1/12+4)+2001
# Plot the returns
n2=length(Apple.returns)
Year2=(1:n2)/n2*(1/12+4)+2001
par(mfrow=c(2,1), mai=c(0.9, 0.8, 0.7, 0.5))
plot(Year1, X, type="l", xlab="Year", main="Time series of Apple from 2001 to 2005")
plot(Year2, Apple.returns, type="l", col=2, xlab="Year", ylab="Apple returns from 2001 to 2005", main="The returns of Apple from 2001 to 2005")

# Display the  acf of the returns and the sqared returns
acf(Apple.returns)
acf(Apple.returns**2)

# Fit the Garch(1,1) model

install.packages("fGarch")
library(fGarch)
Apple.garch11=garchFit(~garch(1,1), Apple.returns, trace=FALSE)

# Apple returns and estiamted conditional SD
sigma.tgarch11=Apple.garch11@sigma.t
par(mfrow=c(2,1), mai=c(0.9, 0.8, 0.7, 0.5))
matplot(Year2, Apple.returns, type="l", xlab="Year", ylab="Returns")
title("The return series of the Apple time series")
matplot(Year2, sigma.tgarch11, type="l", xlab="Year", ylab="sigma.t")
title("Estimated conditonal standard deviations by Garch(1,1)")


# Fit the Garch(1,2) model
Apple.garch12=garchFit(~garch(1,2), Apple.returns, trace=FALSE)
# Applereturns and estiamted conditional SD
sigma.tgarch12=Apple.garch12@sigma.t
matplot(Year2, Apple.returns, type="l", xlab="Year", ylab="Returns")
title("The return series of the Apple time series")
matplot(Year2, sigma.tgarch12, type="l", xlab="Year", ylab="sigma.t")
title("Estimated conditonal standard deviations by Garch(1,2)")

# Fit the Garch(2,1) Model
Apple.garch21=garchFit(~garch(2,1), Apple.returns, Trace=FALSE)
# Apple returns and estiamted conditional SD
sigma.tgarch21=Apple.garch21@sigma.t
par(mfrow=c(2,1), mai=c(0.9, 0.8, 0.7, 0.5))
matplot(Year2, Apple.returns, type="l", xlab="Year", ylab="Returns")
title("The returns series of the Apple time series")
matplot(Year2, sigma.tgarch21, type="l", xlab="Year", ylab="sigma.t")
title("Estimated conditional standard deivations by GARCH(2,1)")

# Fit the Garch (2,2) Model
Apple.garch22=garchFit(~garch(2,2), Apple.returns, Trace=FALSE)
# Apple returns and estiamted conditional SD
sigma.tgarch22=Apple.garch22@sigma.t
par(mfrow=c(2,1), mai=c(0.9, 0.8, 0.7, 0.5))
matplot(Year2, Apple.returns, type="l", xlab="Year", ylab="Returns")
title("The returns series of the Apple time series")
matplot(Year2, sigma.tgarch22, type="l", xlab="Year", ylab="sigma.t")
title("Estimated conditional standard deivations by GARCH(2,2)")


summary(Apple.garch11)		#### Wirte the BICs here
summary(Apple.garch12)
summary(Apple.garch21)
summary(Apple.garch22)

# Garch(1,1) is the selected and the model is h ^_t=6.5735 * 10^(-6)+0,089856Y_(t-1)^2+0,89713h_(t-1)  

# Display the cond std. Does it change over time clearly?

par(mfrow=c(3,1), mai=c(0.9, 0.8, 0.7, 0.5))
matplot(Year2, Apple.returns, type="l", xlab="Year", ylab="Returns")
title("The return series of the Apple time series")
matplot(Year2, sigma.tgarch11, type="l", xlab="Year", ylab="sigma.t")
title("Estimated conditional standard deviations by Garch(1,1)")
matplot(Year2, sigma.tgarch11**2, xlab="Year", type="l", ylab="conditional variance")
title("Estimated  conditional variance by Garch(1,1)")

#  VaR with Garch(1,1)
matplot(Year2, cbind(Apple.returns, 1.645*sigma.tgarch11, 2.326*sigma.tgarch11), type="l", ylab="loss & VaR")
title("Apple loss(black) with 5%- 1& VaR values (red/green)")
# Fit the aparch(1,1) models with delta=2
Apple.aparch11=garchFit(~aparch(1,1), Apple.returns, include.deta=FALSE, delta=2, cond.dis="std",trace=FALSE)
# Apple returns and estimated conditional SD
sigma.taparch11=Apple.aparch11@sigma.t
par(mfrow=c(2,1), mai=c(0.9, 0.8, 0.7, 0.5))
matplot(Year2, Apple.returns, type="l", xlab="Year")
title("The returns series of the Apple time series")
matplot(Year2, sigma.taparch11, type="l", xlab="Year")
title("Estimated conditional standard deviations by APARCH(1,1)")

summary(Apple.aparch11)
# BIC is -5.045636 and thus smaller than GARCH(1,1)
# BIC.Garch11 = -5.039452
# Fit the standard deviation with APARCH(1,1)
par(mfrow=c(3,1), mai=c(0.5, 0.8, 0.3, 0.5))
matplot(Year2, Apple.returns, type="l", xlab="Year")
title("The return series of the Apple time series")
abline(v=2003, col=3, lwd=2)
abline(v=2003.1, col=3, lwd=2)
abline(v=2001.12, col=2, lwd=2)
abline(v=2001.22, col=2, lwd=2)
matplot(Year2, sigma.tgarch11, type="l", xlab="Year", ylab="sigma.t")
title("Estimated conditonal standard deviation by Garch(1,1)")
abline(v=2003, col=3, lwd=2)
abline(v=2003.1, col=3, lwd=2)
abline(v=2001.12, col=2, lwd=2)
abline(v=2001.22, col=2, lwd=2)
matplot(Year2, sigma.taparch11, type="l", xlab="Year", ylab="sigma.t")
title("Estimated conditonal standard deviation by Aparch(1,1)")
abline(v=2003, col=3, lwd=2)
abline(v=2003.1, col=3, lwd=2)
abline(v=2001.12, col=2, lwd=2)
abline(v=2001.22, col=2, lwd=2)

# Calculate the 5%???? and 1%-VaR again

sigma.taparch11=Apple.aparch11@sigma.t
df=9.2560513  ##########shape from summary of Aparch(1,1)-t
q005.t=qt(0.95, df)/sqrt(df/(df-2)) ####### 
q001.t=qt(0.99, df)/sqrt(df/(df-2)) #######
matplot(Year2, cbind(Apple.returns, q005.t*sigma.taparch11, q001.t*sigma.taparch11), type="lll", ylab="loss & VaR")
title("Apple loss (black) with 5%- & 1% VaR (red/green) for APARCh(1,1)-t")







