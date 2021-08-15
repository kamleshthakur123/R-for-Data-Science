

library(DAAG)
str(roller)
plot(depression~weight,roller)
model <- lm(depression~weight,data = roller)

abline(model,col="blue",type="l")

par(mfrow=c(2,2))
plot(model)

### to decide whether it is normal or not
par(mfrow=c(1,1))
qreference(residuals(model),nrep = 8,nrows = 2)

### simulation 

### lets put some random number form std normal distribution
### this is just for academic pupose
## aS no model can be 100% perfect

x <- rnorm(100)
y <- 10+5*x
plot(y~x)
m1 <- lm(y~x)

summary(m1)
### Adding a line
abline(m1,col="red")
set.seed(7)
###if we introduce variablity term i.e error in y
### lets say rnorm(100,0,1) is random error
### error are normal distribute with mean is zero , whatsoever may be the sd.
### this is error is independent 
### y=b0+b1*x+error
### this will statisfy assumption of normality
### NOw summary will change


x <- rnorm(100)
y <- 10+5*x+rnorm(100)
plot(y~x)
m1 <- lm(y~x)
summary(m1)
abline(m1,col="red")

### lets keep mean zero but change sd 
### lets see the result
### there will be variablity high in graph
### R will fall down

x <- rnorm(100)
y <- 10+5*x+rnorm(100,0,3)
plot(y~x)
m1 <- lm(y~x)
summary(m1)
abline(m1,col="red")

par(mfrow=c(2,2))
plot(m1)

### now lets defy the rules of assumption
### lets try to make error dependent of x


x <- rnorm(100)
y <- 10+5*x+ .6*x*rnorm(100,0,3)
plot(y~x)
m1 <- lm(y~x)
summary(m1)
abline(m1,col="red")

par(mfrow=c(2,2))
plot(m1)

### see the four graphs how they have violated the 
### normality assumption.

set.seed(7)
x <- rnorm(100)
y <- 10+5*x*x
plot(y~x)
m1 <- lm(y~x)
summary(m1)
abline(m1,col="red")
par(mfrow= c(2,2))
plot(m1)

### in regression analysis is first step is 
### t0 plot scatter plot

str(roller)
plot(depression~weight,roller)
model <- lm(depression~weight,data = roller)

abline(model,col="blue",type="l")

par(mfrow=c(2,2))
plot(model)

### Anova

anova(model)

### some basic packages required
library(DAAG)
library(caret)
library(TeachingDemos)
library(MASS)
library(mlbench)
model2 <- lm(depression~weight,roller)

summary(model2)
names(model2)
data(roller)
fitted.values(model2)
#### Prediction of standard error of each row




se <- predict(model2,roller,se.fit = T)

se$se.fit
cbind(roller$weight,roller$depression,se$se.fit)

### in case of slope of weight we find ci like this
### find out the t value for 95% c.i
qt(.975,8)
##weight        2.6667     0.7002   3.808  0.00518 **
2.667-(2.306004*0.7002)     ### lower value of slope
2.667+(2.306004*0.7002)

predict(model2,roller,interval = "confidence")
predict(model2,roller,interval = "prediction")

### this can be calculated by standard error or tvalue
2.979669-(2.306004*3.61429)  ##pred value -(tvalue*se)
2.979669+(2.306004*3.61429)

### in case of prediction interval
### residual erro= 6.735
2.979669-(2.306004*sqrt((3.61429^2)+(6.735^2)))
2.979669+(2.306004*sqrt((3.61429^2)+(6.735^2)))  

### prediction interval is meant for individual value.
### here interest lies in the individual value.

### demos
put.points.demo(roller$weight,roller$depression,lsline=T)
summary(roller$weight)
### try to add point on and see the value of
## r and slope etc.



#######_____________________________________
## Resampling

data("BostonHousing")
str(BostonHousing)

plot(medv~lstat,BostonHousing)
### to improve somewhat nature of graph 
### let take log
plot(log(medv)~lstat,BostonHousing)
### from package caret use train function

m3 <- train(log(medv)~lstat,BostonHousing,
            method= "lm")
m3
### to understand bootstapping
head(BostonHousing[13:14])
### take 6 random sample and repeat it for 25 times for bootstrapping.
rnorm(1)
rnorm(1)   ## and so on to select the sample

names(m3)
m3$finalModel


#### resampling- cross validation
c <- trainControl(method = "cv",
                  number = 5)
m3 <- train(log(medv)~lstat,
            BostonHousing,
            method= "lm",
            trControl= c)
m3
names(m3)
m3$resample

### Data Partition
ind <- createDataPartition(BostonHousing$medv,p=.7,list = F)
traing <- BostonHousing[ind,]
testing <- BostonHousing[-ind,]

### some modification in cross validation 

c <- trainControl(method = "repeatedcv",
                  number = 10,
                  repeats = 5,
                  verboseIter = T)
m4 <- train(log(medv)~lstat,
          traing,
            method= "lm",
            trControl= c)
m4

m4$resample
m4$finalModel

p <-predict(m4,testing)
head(exp(p))      ### As we have taken log 

Actual  <- head(testing$medv)
plot(testing$medv,exp(p))


#### transformation

boxcox(medv ~ lstat, data = BostonHousing) -> bc
lambda <- bc$x[which.max(bc$y)]












































































