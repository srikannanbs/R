#Day2
# Data Frame
n = c(2, 3, 5) 
s = c("aa", "bb", "cc") 
b = c(TRUE, FALSE, TRUE) 
df = data.frame(n, s, b)       # df is a data frame

#Create Data Frame
# Create the data frame.
emp.data <- data.frame(
  emp_id = c (1:5),
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25),
  start_date = as.Date(c("2012-01-01","2013-09-23","2014-11-15","2014-05-
                         11","2015-03-27")),
  stringsAsFactors=FALSE
  )
# Print the data frame.
print(emp.data)
#Structure of dataframe
str(emp.data)



#Summary of dataframe
print(summary(emp.data))

# Extract Specific columns.
result <- data.frame(emp.data$emp_name,emp.data$salary)
print(result)


# Extract first two rows.
result <- emp.data[1:2,]

print(result)

# Extract 3rd and 5th row with 2nd and 4th column.
result <- emp.data[c(3,5),c(2,4)]
print(result)


#Expand Data Frame
# Add the "dept" coulmn.
emp.data$dept <- c("IT","Operations","IT","HR","Finance")



v <- emp.data
print(v)

#Add rows
# Create the second data frame
emp.newdata <- data.frame(
  emp_id = c (6:8),
  emp_name = c("Rasmi","Pranab","Tusar"),
  salary = c(578.0,722.5,632.8),
  start_date = as.Date(c("2013-05-21","2013-07-30","2014-06-17")),
  dept = c("IT","Operations","Fianance"),
  stringsAsFactors=FALSE
)


# Bind the two data frames.
emp.finaldata <- rbind(emp.data,emp.newdata)
print(emp.finaldata)


#Joining Columns and Rows in a Data Frame
# Create vector objects.
city <- c("Tampa","Seattle","Hartford","Denver")
state <- c("FL","WA","CT","CO")
zipcode <- c(33602,98104,06161,80294)
# Combine above three vectors into one data frame.
addresses <- cbind(city,state,zipcode)
addresses
# Print a header.
cat("# # # # The First data frame\n")
# Print the data frame.
print(addresses)

# Create another data frame with similar columns
new.address <- data.frame(
  city = c("Lowry","Charlotte"),
  state = c("CO","FL"),
  zipcode = c("80230","33949"),
  stringsAsFactors=FALSE
)
# Print a header.
cat("# # # The Second data frame\n")


# Print the data frame.
print(new.address)
# Combine rows form both the data frames.
all.addresses <- rbind(addresses,new.address)

# Print the result.
print(all.addresses)

#Merging Data Frames
library(MASS)
merged.Pima <- merge(x=Pima.te, y=Pima.tr,
                     by.x=c("bp", "bmi"),
                     by.y=c("bp", "bmi")
)
head(merged.Pima)
nrow(merged.Pima)

library(MASS)
head(ships)
library(reshape)
#Melt the data
molten.ships <- melt(ships, id = c("type","year"))
head(molten.ships)

#Cast the Molten Data
recasted.ship <- cast(molten.ships, type+year~variable,sum)
head(recasted.ship)


###Example
mydata<-data.frame(
  id=c(1,1,2,2),
  time=c(1,2,1,2),
  x1=c(5,3,6,2),
  x2=c(6,5,1,4)
)

# example of melt function 
library(reshape)
mdata <- melt(mydata, id=c("id","time"))
mdata

# cast the melted data
# cast(data, formula, function) 
subjmeans <- cast(mdata, id~variable, mean)
subjmeans
timemeans <- cast(mdata, time~variable, mean)
timemeans


#RCommander

install.packages("Rcmdr",dependencies=TRUE)
library(Rcmdr)

#Linear Regression
library("MASS")
attach(Boston)
dim(Boston)
summary(Boston)
summary(Boston$medv)
head(Boston)
cor(Boston)
plot(density(Boston$medv))

plot(density(Boston$medv),main='Density of Median Housing Values',xlab='med. housing value',ylab='density')

#simple linear regression
plot(medv~lstat,Boston)

fit1=lm(medv~lstat,data=Boston)  

fit1

summary(fit1)

par(mfrow(1,1))

abline(fit1,col="red")

#multiple linear regression

fit2=lm(medv~lstat+age,data=Boston) 

summary(fit2)
#multiple linear regression(all variables)
fit3=lm(medv~.,Boston)  #multiple linear regression(all variables)

summary(fit3)
par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(1,1)) #Residual analysis for fit3

plot(fit3)


#Logistic
data(Titanic)
View(Titanic)
str(Titanic)


rawdata <- data.frame(Titanic)
head(rawdata)
str(rawdata)
#Check for missing value
sapply(rawdata,function(x) sum(is.na(x)))
sapply(rawdata, function(x) length(unique(x)))

#Taking care of the missing values
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

contrasts(rawdata$Sex)

#Breaking Data into Training and Test Sample
nrow(rawdata) #No Of observations in complete data
d = sort(sample(nrow(rawdata), nrow(rawdata)*.6))

#select training sample
train<-rawdata[d,]
test<-rawdata[-d,]
nrow(train) #No Of observations in training data
nrow(test)
#No Of observations in testing data

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)

head(test)


fitted.results <- predict(model,newdata=subset(test,select=c(1,2,3,5)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != as.integer(test$Survived))
misClasificError
print(paste('Accuracy',1-misClasificError))
as.integer(test$Survived)
library(caret)

fitted.results1 <- ifelse(fitted.results ==1,
                          c("Yes"), c("No"))

fitted.results1
confusionMatrix((test$Survived), fitted.results1)
# Missing Value treatment

z <- c(5,NA,12)
is.na(z)

mean(z)

mean(z,na.rm=T)


# Missing Value

data(airquality)

str(airquality)

fix(airquality)
summary(airquality)

mean(airquality$Ozone)

mean(airquality$Ozone,na.rm=TRUE)
summary(airquality,na.rm=TRUE)
fix(airquality)
airquality_new<-na.omit(airquality)
summary(airquality_new)
fix(airquality_new)

data <- airquality
data <- data[-c(5,6)]
data

pMiss <- function(x){sum(is.na(x))/length(x)*100}

apply(data,2,pMiss)

install.packages("mice")
library(mice)
md.pattern(data)

tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
tempData$imp$Ozone
completedData <- complete(tempData,1)

densityplot(tempData)

