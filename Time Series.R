AP<-seq(1:24)
AP
class(AP)
Ap.ts<-ts(AP,start=c(2014,1),end=c(2015,12),frequency=12)
head(Ap.ts)
Ap.ts
class(Ap.ts)

APS<-data(AirPassengers)

class(AirPassengers)
AirPassengers
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
summary(AirPassengers)
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))
cycle(AirPassengers)
plot(decompose(AirPassengers))
plot(decompose(AirPassengers)$seasonal)
plot(aggregate(AirPassengers),FUN=mean)
# only sesaonal-doble exp smoothing
#onlt Trend -exp smoothing
#all trend,seasonal,cyclic-ARIMA
boxplot(AirPassengers~cycle(AirPassengers),xlab="Month",ylab="Airpassengers",main="Avg",names=month.abb,col="green")

plot(diff(AirPassengers),ylab=("Differenced Airpassengers"))
plot(log10(AirPassengers),ylab=("Log Airpassengers"))
plot(exp(AirPassengers),ylab=("exp Airpassengers"))
plot(diff(log10(AirPassengers),ylab=("Diff Log Airpassengers")))

library(tseries)
adf.test(AirPassengers)
adf.test(diff(log10(AirPassengers)),alternative ="stationary",k=0)
#P<0.05
par(mfrow=c(1,2))
acf(ts(diff(log10(AirPassengers))),main="ACF")
pacf(ts(diff(log10(AirPassengers))),main="PACF")

install.packages("forecast")
library(forecast)
ARIMAfit<-auto.arima(log10(AirPassengers),approximation=FALSe,trace=FALSE)
summary(ARIMAfit)
#AIC,BIC =minimum, error value(MAPE) should be less

APforecast<-predict(fit,n.ahead=5*12)
fcast<-forecast.Arima(ARIMAfit,12*5,0.95)
plot(fcast)
