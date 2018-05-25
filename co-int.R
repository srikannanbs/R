data1 <- read.csv("C:/Users/a550859/Desktop/pma.csv", header= TRUE,sep=",")
data2<-data1
TradeVolume<- data.frame(data1)
summary(data2)

library(Hmisc)
dat=data2[7:9]
dat
yy=data2[7]
yy
cor(x,y=yy,use="everything",method="pearson")
cor.test(x,y=yy)
rcorr(as.matrix(data2))
library(MASS)
dim(TradeVolume)
summary(TradeVolume)
summary(TradeVolume$END_FAIR_VALUE_A)
par=(mfrowc(1,1))
plot(density(TradeVolume$END_FAIR_VALUE_A))
value=TradeVolume$END_FAIR_VALUE_A
cor(value, y = TradeVolume, use = "everything",method = c("spearman"))
density(na.omit(TradeVolume$DIFF))
plot(density((TradeVolume$DIFF)),main='Density',xlab='value',ylab='density')

library(caret)
library(Hmisc)
x <- data2$END_FAIR_VALUE_A
y <- data2$UNT_GOF_N
cor(x,y)
head(TradeVolume)
class(TradeVolume)
plot(cor(TradeVolume))

train_obs <- floor (0.4*nrow (data2))
print(train_obs)


set.seed(280)
train_ind<-sample(seq_len(nrow(data2)),size=train_obs)

train<-data2[train_ind,]
test<-data2[-train_ind,]

summary(train)

library(urca)
set.seed(123)
z<-rep(0,1000)
for (i in 2:1000) z[i] <- z[i-1]+rnorm(1)
p<-q<-r <- rep(0,1000)
p<-0.3*z+rnorm(1000)
q<-0.6*z+rnorm(1000)
r<-0.2*z+rnorm(1000)
jotest=ca.jo(data.frame(p,q,r),type="trace",K=2,ecdet="none",spec="longrun")
summary(jotest)
s=1.0000000*p -0.7552779*q -0.7699778*r

plot(s,type="1")

library(quantmod)
data1 <- read.csv("C:/Users/a550859/Desktop/pma.csv", header= TRUE,sep=",")
vol<- data.frame(data1)
getSymbols("NFS",vol)
Tra <-vol[4:8]
Tra
eWaAdj=unclass(Tra$END_FAIR_VALUE_A)
eWcAdj=unclass(Tra$BEGIN_FAIR_VALUE_A)
igeAdj=unclass(Tra$ACC_N)
jotest=ca.jo(data.frame(eWaAdj),type="trace",K=2,ecdet="none",spec="longrun")



library("quantmod")
library("tseries")
library("urca")

set.seed(123)

## Simulated cointegrated series

z <- rep(0, 10000)
for (i in 2:10000) z[i] <- z[i-1] + rnorm(1)

p <- q <- r <- rep(0, 10000)

p <- 0.3*z + rnorm(10000)
q <- 0.6*z + rnorm(10000)
r <- 0.8*z + rnorm(10000)

jotest=ca.jo(data.frame(p,q,r), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

s = 1.000*p + 1.791324*q - 1.717271*r
plot(s, type="l")

adf.test(s)

## EWA, EWC and IGE

getSymbols("EWA", from="2006-04-26", to="2012-04-09")
getSymbols("EWC", from="2006-04-26", to="2012-04-09")
getSymbols("IGE", from="2006-04-26", to="2012-04-09")

ewaAdj = unclass(EWA$EWA.Adjusted)
ewcAdj = unclass(EWC$EWC.Adjusted)
igeAdj = unclass(IGE$IGE.Adjusted)

jotest=ca.jo(data.frame(ewaAdj,ewcAdj,igeAdj), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

## SPY, IVV and VOO

getSymbols("SPY", from="2015-01-01", to="2015-12-31")
getSymbols("IVV", from="2015-01-01", to="2015-12-31")
getSymbols("VOO", from="2015-01-01", to="2015-12-31")

spyAdj = unclass(SPY$SPY.Adjusted)
ivvAdj = unclass(IVV$IVV.Adjusted)
vooAdj = unclass(VOO$VOO.Adjusted)

jotest=ca.jo(data.frame(spyAdj,ivvAdj,vooAdj), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)