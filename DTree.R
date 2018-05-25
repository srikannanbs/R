library(rpart)
data1 <-read.csv("C:/Users/a550859/Documents/R/Workfolder/export.csv")
data1
TradeVolume <- data.frame(data1)
head(TradeVolume)
fit <- rpart(DIFF ~ ODATE + MEMNAME,method="anova",data=TradeVolume)
printcp(fit)
plotcp(fit)
summary(fit)
par(mfrow=c(1,2))
rsq.rpart(fit)
plot(fit,uniform=TRUE,main="Regression")
text(fit,use.n =TRUE,all=TRUE,cex=.8 )

post(fit,file="C:/Users/a550859/Documents/R/tree2.png",title="Regression Tree")

#Prune the tree
pfit<- prune(fit, cp=0.01000000)
pfit
plot(pfit, uniform=TRUE, main="Pruned Reg Tree")
text(pfit,use.n=TRUE,all=TRUE,cex=.8)
post(pfit,file="C:/Users/a550859/Documents/R/tree2.ps",title="Pruned Tree")

library(party)
fit2 <- ctree(MEMNAME ~ ODATE + DIFF, data=na.omit(TradeVolume))
plot(fit2, uniform=TRUE, main="cTree")

library(randomForest)
set.seed(61)
fit <- randomForest(MEMNAME ~ ODATE + DIFF, data=TradeVolume,ntree=200)

print(fit)

mtry <- tuneRF(TradeVolume,TradeVolume$BOOKKEEP, ntreeTry=200,stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)
set.seed(61)
fit <- randomForest(MEMNAME ~ ODATE + DIFF, data=TradeVolume,mtry=best.m,importance=TRUE,ntree=200)

importance(fit)
varImpPlot(fit)
library(caret)
library(Hmisc)
x <- TradeVolume$BOOKKEEP
y <- TradeVolume$ORDERS
cor(x,y)
head(TradeVolume)
class(TradeVolume)
plot(cor(TradeVolume))
# Plot partial dependence of each predictor
par(mfrow = c(3, 5), mar = c(2, 2, 2, 2), pty = "s");
for (i in 1:(ncol(TradeVolume) - 1))
{
  partialPlot(fit, TradeVolume, names(TradeVolume)[i], xlab = names(TradeVolume)[i],
              main = NULL);
}


#Calculate predictive probabilities of training dataset.
pred1=predict(fit,type = "prob")

asd <-cbind(TradeVolume,pred1)
write.table(asd,file="C:/Users/a550859/Documents/R/prob.csv",sep=",",row.names=F)



help("randomForest")
help(cor)
library(MASS)
dim(TradeVolume)
summary(TradeVolume)
summary(TradeVolume$BOOKKEEP)
par=(mfrowc(1,1))
plot(density(TradeVolume$DIFF))
cor(DIFF, y = TradeVolume, use = "everything",method = c("spearman"))
density(na.omit(TradeVolume$DIFF))
plot(density((TradeVolume$DIFF)),main='Density',xlab='value',ylab='density')
library(rpart)
par(mfrow=c(1,1))
plot(MEMNAME ~ ODATE + DIFF,TradeVolume)
fit1 <-lm(MEMNAME ~ ODATE + DIFF,data=TradeVolume)
fit1
summary(fit1)

par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(1,1))

abline(fit1,col="red")
plot(fit1)

library(randomForest)
library(randomForestSRC)
library(caret)
library(rpart)
data1 <-read.csv("C:/Users/a550859/Documents/R/Workfolder/export.csv")
volume <- data.frame(data1)
set.seed(71)
fit1 <-lm(MEMNAME ~ ODATE + DIFF,data=volume)
fit1
summary(fit1)

d=sort(sample(nrow(volume),nrow(volume)*.6))
train<-volume[d,]
test<-volume[-d,]
nrow(train)
nrow(test)

modfit <- train(DIFF~.,method="rf",data=train)
pred <- predict(modfit,train)
table(pred,train$DIFF)

library("MASS")
data1 <-read.csv("C:/Users/a550859/Documents/R/Workfolder/volume.csv",header=TRUE,sep=",")
data1
TradeVolume <- data.frame(data1)
head(TradeVolume)
cor(TradeVolume)
plot(density(TradeVolume$DIFF))
model<-lm(DIFF ~ POSITIONS,data=TradeVolume)
summary(model)
pred1<-predict(model)
pred1<-predict(model,TradeVolume,interval = "confidence")
pred1
head(pred1)
residual<-resid(model)
residual1<-data.frame(residual)
plot(TradeVolume$DIFF,residual,ylab="Residuals",xlab="waitingtime",main="DIFF")
abline(0,0)
plot(pred1,residual1,abline(0,0))
plot(TradeVolume$DIFF,col="red")
lines(pred1,col="blue")
finaldata<-cbind(TradeVolume,pred1)
head(finaldata)
finaldata
