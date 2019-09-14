#### FINANCIAL TIME SERTIES ANALYSIS ####

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


# Exercise 3 

# Find some daily nancial time series within the period between 
# January 2004 and May 2018 with a clear eect of the nancial crisis in 2008 and 2009.

par(mfrow=c(2,1))
Z=read.csv("BCS.csv")
Price=Z[,5]
nd=length(Price)

Year=(1:nd)/nd*(14+5/12)+2004
plot(Year, Price, type="l")
title("Daily time series of Barclays stock prices (2004 - May 2018)")
abline(v=2008, col=2)
abline(v=2010, col=2)



# Plot the returns of Barclays stock prices (2004 - May 2018)

BCS.Returns=diff(log(Price)) ###### calculate the log returns
n2=length(BCS.Returns)
Year2=(1:n2)/n2*(5/12+14)+2004
plot(Year2, BCS.Returns, type="l", col=1, xlab="Year", main="Barclays returns")
abline(v=2011, col=2)



# Calculate and compare the (unconditional) standard deviations of the returns
# for the four sub-periods 2004 to 2007, 2008 to 2009, 2010 to 2012 and 2013 to
# May 2018, respectively

BCS1=Price[2:756]       ###### data from 2004 to 2007
BCS2=Price[757:1260]    ###### data from 2008 to 2009
BCS3=Price[1261:2266]   ###### data from 2010 to 2012
BCS4=Price[2267:3627]   ###### data from 2013 to May 2018
n11=length(BCS1)
n22=length(BCS2)
n33=length(BCS3)
n44=length(BCS4)
return1=diff(log(BCS1))
return2=diff(log(BCS2))
return3=diff(log(BCS3))
return4=diff(log(BCS4))
sd1=sqrt(sum((return1-mean(return1))^2)/(n11-1))       ##### number n-1
sd2=sqrt(sum((return2-mean(return2))^2)/(n22-1))
sd3=sqrt(sum((return3-mean(return3))^2)/(n33-1))
sd4=sqrt(sum((return4-mean(return4))^2)/(n44-1))

sd1
sd2
sd3
sd4


# Exercise 4

# High-frequency data, realized volatility and realized correlations
# Data les under the names \W4451-UHFA1.txt", \W4451-UHFA2.txt", \W4451-UHFA3.txt";
# \W4451-EHFA1.txt", \W4451-EHFA2.txt", \W4451-EHFA3.txt" and \W4451-EHFS1.txt",
# \W4451-EHFS2.txt" and \W4451-EHFS3.txt" are provided on Moodle. Where the rst three
# data les consist of the ultra-high-frequency stock prices of the Allianz AG in two weeks in
# Jaunuary 2006, January 2009 and January 2012, respectively, i.e. in three periods before,
# during and after the 2008 nancial crisis. The next three data les consist of the 1-minute-
# prices treated from the rst three les. Corresponding 1-minute-prices of the Siemens AG are
# given in the last three data les. Each of these les consists of three columns with a title line:
# Day, Time and Price, where the variable \Time" in the rst three les is given in decimal
# format and the variable \Time" in the last six les is given in the format \hour:minute". A code
# \W4451-2018-Proj-Q4.txt" is also provided.

num=6811992  ####### your own Matrikelnummer

X1=read.table("W4451-EHFA1.txt",header=TRUE)
X2=read.table("W4451-EHFA2.txt",header=TRUE)
X3=read.table("W4451-EHFA3.txt",header=TRUE)
d1=as.numeric(substring(num,5,5))+1
d2=as.numeric(substring(num,6,6))+1
d3=as.numeric(substring(num,7,7))+1
XEHFA1=X1[X1$Day == (sort(unique(X1$Day))[d1]), ]
XEHFA2=X2[X2$Day == (sort(unique(X2$Day))[d2]), ]
XEHFA3=X3[X3$Day == (sort(unique(X3$Day))[d3]), ]

Y1=read.table("W4451-EHFS1.txt",header=TRUE)
Y2=read.table("W4451-EHFS2.txt",header=TRUE)
Y3=read.table("W4451-EHFS3.txt",header=TRUE)
XEHFS1=Y1[Y1$Day == (sort(unique(Y1$Day))[d1]), ]
XEHFS2=Y2[Y2$Day == (sort(unique(Y2$Day))[d2]), ]
XEHFS3=Y3[Y3$Day == (sort(unique(Y3$Day))[d3]), ]

Z1=read.table("W4451-UHFA1.txt",header=TRUE)
Z2=read.table("W4451-UHFA2.txt",header=TRUE)
Z3=read.table("W4451-UHFA3.txt",header=TRUE)
XUHFA1=Z1[Z1$Day == (sort(unique(Z1$Day))[d1]), ]
XUHFA2=Z2[Z2$Day == (sort(unique(Z2$Day))[d2]), ]
XUHFA3=Z3[Z3$Day == (sort(unique(Z3$Day))[d3]), ]



# #Display the three ultra-high-frequency price series of Allianz AG against the corresponding
# observation time points

X1=read.table("W4451-UHFA1.txt", header=TRUE)
X2=read.table("W4451-UHFA2.txt", header=TRUE)
X3=read.table("W4451-UHFA3.txt", header=TRUE)
p1=X1[,3]
p2=X2[,3]
p3=X3[,3]
par(mfrow=c(3,1),mai=c(0.5,0.6,0.5,0.4))
plot.ts(p1, type="l", col=2, ylab="Price")
title("Ultra-high frequency stock price of Allianz AG, 12 Jan.2006")
plot.ts(p2, type="l", col=2, ylab="Price")
title("Ultra-high frequency stock price of Allianz AG, 13 Jan.2009")
plot.ts(p3, type="l", col=2, ylab="Price")
title("Ultra-high frequency stock price of Allianz AG, 12 Jan.2012")

p1.log=log(p1)
p2.log=log(p2)
p3.log=log(p3)
returns.p1=diff(p1.log)
returns.p2=diff(p2.log)
returns.p3=diff(p3.log)
par(mfrow=c(3,1),mai=c(0.5,0.6,0.5,0.4))
plot.ts(returns.p1, type="l", col=2, ylab="Price")
title("Return Series of Allianz AG stock price, 12 Jan.2006")
plot.ts(returns.p2, type="l", col=2, ylab="Price")
title("Return Series of Allianz AG stock price, 13 Jan.2009")
plot.ts(returns.p3, type="l", col=2, ylab="Price")
title("Return Series of Allianz AG stock price, 12 Jan.2012")

# Calculate and state the realized volatility (RV) on the three days.

xpa1=XEHFA1[,3]
xpa2=XEHFA2[,3]
xpa3=XEHFA3[,3]
returns.xpa1=diff(log(xpa1))
returns.xpa2=diff(log(xpa2))
returns.xpa3=diff(log(xpa3))
rv.xpa1=sum(returns.xpa1**2)
rv.xpa2=sum(returns.xpa2**2)
rv.xpa3=sum(returns.xpa3**2)
rv.xpa1
rv.xpa2
rv.xpa3

xps1=XEHFS1[,3]
xps2=XEHFS2[,3]
xps3=XEHFS3[,3]
returns.xps1=diff(log(xps1))
returns.xps2=diff(log(xps2))
returns.xps3=diff(log(xps3))
rv.xps1=sum(returns.xps1**2)
rv.xps2=sum(returns.xps2**2)
rv.xps3=sum(returns.xps3**2)
rv.xps1
rv.xps2
rv.xps3

xupa1=XUHFA1[,3]
xupa2=XUHFA2[,3]
xupa3=XUHFA3[,3]
returns.xupa1=diff(log(xupa1))
returns.xupa2=diff(log(xupa2))
returns.xupa3=diff(log(xupa3))
rv.xupa1=sum(returns.xupa1**2)
rv.xupa2=sum(returns.xupa2**2)
rv.xupa3=sum(returns.xupa3**2)
rv.xupa1
rv.xupa2
rv.xupa3

# Calculate the equidistant returns on the three days for both companies.

returns.xpa1
returns.xpa2
returns.xpa3
returns.xps1
returns.xps2
returns.xps3
par(mfrow=c(3,1),mai=c(0.5,0.6,0.5,0.4))
plot(returns.xpa1, returns.xps1, xlab="Equidistant Returns of Allianz AG", ylab="Equidistant Returns of Siemens")
plot(returns.xpa2, returns.xps2, xlab="Equidistant Returns of Allianz AG", ylab="Equidistant Returns of Siemens")
plot(returns.xpa3, returns.xps3, xlab="Equidistant Returns of Allianz AG", ylab="Equidistant Returns of Siemens")
 
# Calculate and state the realized correlations (RCor) on those days.
 
rcov1=sum(returns.xpa1*returns.xps1)
rcov2=sum(returns.xpa2*returns.xps2)
rcov3=sum(returns.xpa3*returns.xps3)
rva1=sum(returns.xpa1**2)
rvs1=sum(returns.xps1**2)
rva2=sum(returns.xpa2**2)
rvs2=sum(returns.xps2**2)
rva3=sum(returns.xpa3**2)
rvs3=sum(returns.xps3**2)
rcor1=rcov1/(sqrt(rva1)*sqrt(rvs1))
rcor2=rcov2/(sqrt(rva2)*sqrt(rvs2))
rcor3=rcov3/(sqrt(rva3)*sqrt(rvs3))
rcor1
rcor2
rcor3



