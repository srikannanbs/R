library("MASS")
data1 <-read.csv("C:/Users/a550859/Documents/R/Workfolder/Jobdet.csv",header=TRUE,sep=",")
data1
TradeVolume <- data.frame(data1)
head(TradeVolume)
cor(TradeVolume)
plot(density(TradeVolume$DIFF))
model<-lm(DIFF ~.,data=TradeVolume)
summary(model)
pred<-predict(model)
pred
head(pred)
residual<-resid(model)
residual1<-data.frame(residual)
plot(TradeVolume$DIFF,residual,ylab="Residuals",xlab="waitingtime",main="DIFF")
abline(0,0)
plot(pred1,residual1,abline(0,0))
plot(TradeVolume$DIFF,col="red")
lines(pred1,col="blue")
finaldata<-cbind(TradeVolume,pred)
head(finaldata)
finaldata
write.csv(finaldata,"Pred_output.csv")




data1 <-read.csv("C:/Users/a550859/Documents/R/Workfolder/Jobdet.csv",header=TRUE,sep=",")
data1
dataset <- data.frame(data1)
library(randomForest)
set.seed(131)
dataset.rf <- randomForest(DIFF ~ .,data=dataset,mtry=3,importance=TRUE, na.action=na.omit)
print(dataset.rf)
## Show "importance" of variables: higher value mean more important:
round(importance(dataset.rf), 2)


library(randomForestSRC)
library(ggRandomForests)
data1 <-read.csv("C:/Users/a550859/Documents/R/Workfolder/Jobdet.csv",header=TRUE,sep=",")
data1
dataset <- data.frame(data1)
rf<-rfsrc(DIFF~.,data=dataset)
data(rf)
rf
gg_e <- gg_error(rf)
plot(gg_e)
plot(gg_rfsrc(rf),alpha=.5)+coord_cartesian(ylim=c(5,49))
