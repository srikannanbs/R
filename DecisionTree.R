str(iris)
set.seed(1234)
ind<-sample(2,nrow(iris),replace=T,prob=c(0.7,0.3))
iris.train<-iris[ind==1,]
iris.test<-iris[ind==2,]

library(party)

library(ggplot2)

#KMeans#
install.packages("rattle")
library(rattle)
data(wine,package='rattle')
head(wine)
str(wine)
wine.stand<-scale(wine[-1]) #To Standardize the variables
wine.stand

#Elbow chart
wssplot<-function(data,nc=15,seed=1234){
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data,centers=i)$withiniss)}
  plot(1:nc,wss,type="b",xlab="No of clusters",ylab="within group")
}
wssplot(wine.stand,nc=6)
kmeans.fit<-kmeans(wine.stand,4)
kmeans.fit
names(kmeans.fit)
attributes(kmeans.fit)
kmeans.fit$centers
kmeans.fit$cluster
kmeans.fit$size

library(cluster)
clusplot(wine.stand,kmeans.fit$cluster,main='2D cluster',color=TRUE,shade=TRUE,labels=2,lines=0)

library(rattle)
rattle()



library(arules)
library(arulesViz)
library(datasets)
data(Groceries)
itemFrequencyPlot(Groceries,topN=20,type="absolute")
rules<-apriori(Groceries,parameter=list(supp=0.001,conf=0.8))
summary(rules)
rules<-apriori(Groceries,parameter=list(supp=0.001,conf=0.8),
               appearance=list(default="lhs",rhs=""))

#lift>1 and conf>80 best