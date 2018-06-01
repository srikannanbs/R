library(rpart)
data1 <-read.csv("C:/Users/a550859/Documents/R/tradevolume.csv")
TradeVolume <- data.frame(data1)
fit <- rpart(BOOKKEEP ~ ORDERS + EXECS + POSITIONS,method="anova",data=TradeVolume)
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
fit2 <- ctree(BOOKKEEP ~ ORDERS + EXECS + POSITIONS, data=na.omit(TradeVolume))
plot(fit2, uniform=TRUE, main="cTree")

library(randomForest)
set.seed(61)
fit <- randomForest(BOOKKEEP ~ ORDERS + EXECS + POSITIONS, data=TradeVolume,ntree=200)

print(fit)

mtry <- tuneRF(TradeVolume,TradeVolume$BOOKKEEP, ntreeTry=200,stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)
set.seed(61)
fit <- randomForest(BOOKKEEP ~ ORDERS + EXECS + POSITIONS, data=TradeVolume,mtry=best.m,importance=TRUE,ntree=200)

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
plot(density(TradeVolume$BOOKKEEP))
cor(BOOKKEEP, y = TradeVolume, use = "everything",method = c("spearman"))
density(na.omit(TradeVolume$BOOKKEEP))
plot(density((TradeVolume$BOOKKEEP)),main='Density',xlab='value',ylab='density')
library(rpart)
par(mfrow=c(1,1))
plot(BOOKKEEP~EXECS+ORDERS+POSITIONS,TradeVolume)
fit1 <-lm(BOOKKEEP~EXECS+ORDERS,data=TradeVolume)
fit1
summary(fit1)

par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(1,1))

abline(fit1,col="red")
plot(fit1)


