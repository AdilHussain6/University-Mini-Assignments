#Time Series Practical Code

load("pond.RData")
plot(X,xaxt="n",yaxt="n",xlim=c(1966,2016),xlab="Year",ylab="Water level (feet)",main="Monthly water levels for a pond in Hampshire")
axis(1,seq(1966,2016,5))
axis(2,las=2)
summary(X)
var(X)
sd(X)
acf(X,main="Correlogram for X")

#Yearly plot every 10 years
par(mfrow=c(1,2))
plot(ts(X[1:12]),xaxt="n",yaxt="n",main="Monthly water levels in 1966",xlab="Month",ylab="Water level (feet)")
axis(1,seq(1,12,1))
axis(2,las=2)
plot(ts(X[121:132]),xaxt="n",yaxt="n",main="Monthly water levels in 1976",xlab="Month",ylab="Water level (feet)")
axis(1,seq(1,12,1))
axis(2,las=2)
plot(ts(X[241:252]),xaxt="n",yaxt="n",main="Monthly water levels in 1986",xlab="Month",ylab="Water level (feet)")
axis(1,seq(1,12,1))
axis(2,las=2)
plot(ts(X[361:372]),xaxt="n",yaxt="n",main="Monthly water levels in 1996",xlab="Month",ylab="Water level (feet)")
axis(1,seq(1,12,1))
axis(2,las=2)
plot(ts(X[481:492]),xaxt="n",yaxt="n",main="Monthly water levels in 2006",xlab="Month",ylab="Water level (feet)")
axis(1,seq(1,12,1))
axis(2,las=2)
plot(ts(X[589:600]),xaxt="n",yaxt="n",main="Monthly water levels in 2015",xlab="Month",ylab="Water level (feet)")
axis(1,seq(1,12,1))
axis(2,las=2)

#Fitting a linear trend
tt = 1:length(X)
fit1=lm(X~tt)
trend1=fitted(fit1) #trend is as a vector currently, needs conv to ts
trend1=ts(trend1,start=start(X),end=end(X),frequency=frequency(X))
#Superimpose on time series plot

lines(trend1,col="blue")

#Work out coeffs
round(fit1$coefficients[1],2)

#Find and plot residuals
resid1=ts(residuals(fit1),start=start(X),end=end(X),frequency=frequency(X))
plot(resid1,xaxt="n",yaxt="n",type='l',xlim=c(1966,2016),xlab="Year",ylab="Residuals",main="Residuals after removing linear trend")
axis(1,seq(1966,2016,5))
axis(2,las=2)

#Seasonal effect
n = length(X)
jan = as.numeric((1:n %% 12) == 1)
feb = as.numeric((1:n %% 12) == 2)
mar = as.numeric((1:n %% 12) == 3)
apr = as.numeric((1:n %% 12) == 4)
may = as.numeric((1:n %% 12) == 5)
jun = as.numeric((1:n %% 12) == 6)
jul = as.numeric((1:n %% 12) == 7)
aug = as.numeric((1:n %% 12) == 8)
sep = as.numeric((1:n %% 12) == 9)
oct = as.numeric((1:n %% 12) == 10)
nov = as.numeric((1:n %% 12) == 11)
dec = as.numeric((1:n %% 12) == 0)
fit4 = lm(resid1 ~ 0+jan+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec)
seasonal = ts(fitted(fit4),start=start(X), end=end(X),frequency=frequency(X))
fv = trend1+seasonal
Y = X-fv
plot(Y,xaxt="n",yaxt="n", type="l", xlab="Year", ylab="Residuals", main="Y",xlim=c(1966,2016))
axis(1,seq(1966,2016,5))
axis(2,las=2)
round(fit4$coefficients,2)

acf(Y,main="Correlogram of Y")
pacf(Y,main="PACF of Y")

#Fitting AR(p) models to the ts Y
arfit1=ar(Y, order=1,aic=F)
arfit2=ar(Y, order=2,aic=F)
arfit3=ar(Y, order=3,aic=F)


#Residuals of the AR(p) models
par(mfrow=c(3,2))
plot(arfit1$resid,main="AR(1)",ylab="Residuals",xlab="Year")
acf(arfit1$resid,na.action=na.omit,main="Correlogram for AR(1)")
plot(arfit2$resid,main="AR(2)",ylab="Residuals",xlab="Year")
acf(arfit2$resid,na.action=na.omit,main="Correlogram for AR(2)")
plot(arfit3$resid,main="AR(3)",ylab="Residuals",xlab="Year")
acf(arfit3$resid,na.action=na.omit,main="Correlogram for AR(3)")

#Our chosen Z
par(mfrow=c(1,1))
Z=arfit2$resid
plot(Z,xaxt="n",yaxt="n",main="Z",xlab="Year",ylab="Residuals")
axis(1,seq(1966,2016,5))
axis(2,las=2)
