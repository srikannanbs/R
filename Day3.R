#Day3
#AP <- read.table("C:/MJ_Syn/Manisha_Notes/Training/BusinessAnalyticswithR/Script/Data/AP.csv", header=T, quote="\"")
AP<-seq(1:24)
AP
class(AP)
AP.ts<-ts(AP,start = c(2014,1),end=c(2015,12),frequency = 12)
head(AP.ts)
AP.ts
class(AP.ts)

TimeSeries_ARIMA

# Step 1: Plot  data as time series

data<-data(AirPassengers)

class(AirPassengers)
AirPassengers





#This tells you that the data series is in a time series format
start(AirPassengers)
end(AirPassengers)

frequency(AirPassengers)

summary(AirPassengers)

plot(AirPassengers)

abline(reg=lm(AirPassengers~time(AirPassengers)))






cycle(AirPassengers)








plot(decompose(AirPassengers))






plot(decompose(AirPassengers)$seasonal)






plot(aggregate(AirPassengers,FUN=mean))








boxplot(AirPassengers~cycle(AirPassengers),xlab = "Month", 
        ylab = "AirPassengers", main = "Average Passengers per Month",
        names = month.abb,col = "green")






# Step 2: Difference data to make data stationary on mean (remove trend)


plot(diff(AirPassengers),ylab=("Differenced AirPassengers"))





# Step 3: log transform data to make data stationary on variance

plot(log10(AirPassengers),main="Log transform data")





#Step 4: Difference log transform data to make data stationary on both mean and variance

plot(diff(log10(AirPassengers)),main= "Diferenced Log ")




#Augmented Dickey-Fuller Test
#install.packages("tseries")
library(tseries)
adf.test(AirPassengers)

adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

#Step 5: Plot ACF and PACF to identify potential AR and MA model

par(mfrow = c(1,2))

acf(ts(diff(log10(AirPassengers))),main="ACF")
pacf(ts(diff(log10(AirPassengers))),main="PACF")

#Step 6: Identification of best fit ARIMA model

#install.packages("forecast")
library(forecast)
ARIMAfit <- auto.arima(log10(AirPassengers), 
                       approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

#Step 6: Forecast number of Passengers using the best fit ARIMA model

APforecast <- predict(fit, n.ahead=5*12)

#ts.plot(AirPassengers,2.718^APforecast$pred, log = "y", lty = c(1,3)) 

AP.pred <- ts(exp(APforecast$pred), start = 1961,freq = 12)
AP.pred

fcast<-forecast.Arima(ARIMAfit,12*5,0.95)
fcast
plot(fcast)

#Step 7: Plot ACF and PACF for residuals of ARIMA model to ensure no more information is left for extraction
par(mfrow=c(1,2))
acf(ts(ARIMAfit$residuals),main="ACF Residual")
pacf(ts(ARIMAfit$residuals),main="PACF Residual")



hist(ARIMAfit$residuals)
plot(ARIMAfit$residuals)


#Augmented Dickey-Fuller Test
#install.packages("tseries")
library(tseries)
adf.test(AirPassengers)

adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

## Start(NA )
install.packages("fUnitRoots") 
library(fUnitRoots);
adfTest(AirPassengers);
adfTest(log(AirPassengers));
adfTest(diff(AirPassengers));

library("MASS")
##clustering with PCA
install.packages("rattle")
library(ggplot2)
library(rattle)
data1 <- read.csv("C:/Users/a550859/Desktop/set1.csv", header= TRUE,sep=",")
data2<-data.frame(data1)
plot(data2[1:10])
pc <- princomp(data2)
plot(pc)
summary(pc)
loadings(pc)

mar <- par()$mar
par(mar=mar+c(0,5,0,0))
barplot(sapply(data2, var), horiz=T, las=1, cex.names=0.8)
barplot(sapply(data2, var), horiz=T, las=1, cex.names=0.8, log='x')
par(mar=mar)

data3<-data.frame(scale(data2))
plot(sapply(data3,var))


pc<-princomp(data3)
plot(pc)
plot(pc,type='lines')
summary(pc)


pc <- prcomp(data3)
comp <- data.frame(pc$x[,1:9])
plot(comp, pch=16, col=rgb(0,0,0,0.5))

library(rgl)
library(plot3D)
library(plot3Drgl)
# Multi 3D plot
plot3D(comp$PC1, comp$PC2, comp$PC3)
plot3D(comp$PC1, comp$PC3, comp$PC4)


wss <- (nrow(comp)-1)*sum(apply(comp,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(comp,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

k <- kmeans(comp, 14, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)


# 3D plot
plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
plot3d(comp$PC1, comp$PC3, comp$PC4, col=k$clust)


# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))
clust
# First cluster
row.names(data3[k$clust==clust[1],])
# Second Cluster
row.names(data3[k$clust==clust[2],])
# Third Cluster
row.names(data3[k$clust==clust[3],])
# Fourth Cluster
row.names(data3[k$clust==clust[4],])
# Fifth Cluster
row.names(data3[k$clust==clust[5],])

boxplot(data3$Masked.Compay.ID ~ k$cluster,
        xlab='Cluster', ylab='Client',
        main='By Cluster')


library(urca)
library(vars)



'End
#Decision trees
# iris data
str(iris)
# split into training and test datasets
set.seed(1234)
ind <- sample(2, nrow(iris), replace=T, prob=c(0.7, 0.3))
iris.train <- iris[ind==1, ]
iris.test <- iris[ind==2, ]
# build a decision tree
library(party)
iris.formula <- Species ~ Sepal.Length + Sepal.Width +Petal.Length + Petal.Width
iris.ctree <- ctree(iris.formula, data=iris.train)
plot(iris.ctree)
# predict on test data
pred <- predict(iris.ctree, newdata = iris.test)
# check prediction result
table(pred, iris.test$Species)

##K Means Clustering

install.packages("rattle")
library(rattle)
data(wine)
head(wine)
str(wine)
wine.stand <- scale(wine[-1])  # To standarize the variables
?(scale)
# K-Means
k.means.fit <- kmeans(wine.stand, 3) # k = 3
names(k.means.fit)

attributes(k.means.fit)

# Centroids:
k.means.fit$centers
# Clusters:
k.means.fit$cluster
# Cluster size:
k.means.fit$size

#Elbow chart
wssplot <- function(data, nc=15, seed=1234){
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:nc){
set.seed(seed)
wss[i] <- sum(kmeans(data, centers=i)$withinss)}
plot(1:nc, wss, type="b", xlab="Number of Clusters",
ylab="Within groups sum of squares")}

wssplot(wine.stand, nc=6) 

library(cluster)

clusplot(wine.stand, k.means.fit$cluster,
        main='2D representation of the Cluster solution',
        color=TRUE, shade=TRUE,
        labels=2, lines=0)

#SQL connection

library("RODBC")
#define ODBC connection in control panel
ch<- odbcConnect("mydata")#open connection
gc()#garbage collection
product <- sqlFetch(ch, "DimProduct")#Open table
dat <- sqlQuery(ch, "select * from DimProduct") #write query
close(ch)#close connection


 Marketbasket.R
 Load the libraries
install.packages(c("arules", "arulesViz"))
library(arules)
library(arulesViz)
library(datasets)

# Load the data set
data(Groceries)
# Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute")

# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, 
conf = 0.8))


# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:5])
summary(rules)


rules<-sort(rules, by="confidence", decreasing=TRUE)

#rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))

#subset.matrix <- is.subset(rules, rules)
#subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
#redundant <- colSums(subset.matrix, na.rm=T) >= 1
#rules.pruned <- rules[!redundant]
#rules<-rules.pruned

rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
appearance = list(default="lhs",rhs="whole milk"),
control = list(verbose=F))

rules<-sort(rules, decreasing=TRUE,by="confidence")

inspect(rules[1:5])



rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
appearance = list(default="rhs",lhs="whole milk"),
control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

library(arulesViz)
plot(rules,method="graph",interactive=TRUE,shading=NA)


sel <- plot(rules, measure=c("support", "lift"), shading="confidence", interactive=TRUE)
subrules2 <- head(sort(rules, by="lift"), 10)
plot(subrules2, method="graph")















