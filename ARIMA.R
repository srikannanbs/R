data1 <-read.csv("C:/Users/a550859/Documents/R/tradevolum.csv")
class(data1)
TradeVolume <- data.frame(data1)
class(TradeVolume)
frequency(TradeVolume)
summary(AirPassengers)
plot(TradeVolume)
abline(reg=lm(AirPassengers~time(AirPassengers)))
cycle(AirPassengers)
plot(decompose(AirPassengers))
plot(decompose(AirPassengers)$seasonal)
plot(aggregate(AirPassengers),FUN=mean)
# only sesaonal-doble exp smoothing
#onlt Trend -exp smoothing
#all trend,seasonal,cyclic,random-ARIMA
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