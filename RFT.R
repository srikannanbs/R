mydata <- read.csv("C:/Users/a550859/Documents/R/Trade_2017.csv")

#Explore data
nrow(mydata)
summary(mydata)


#Install and load packages required for random forest
library(party)
library(randomForest)
library(ROCR)
set.seed(71) 
rf <-randomForest(BOOKKEEP~ORDERS+EXECS,data=mydata, ntree=200) 
print(rf)

mtry <- tuneRF(mydata[-13],mydata$income, ntreeTry=200,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)