#### Logistric Regression
library(DAAG)
library(ggplot2)
library(dplyr)
library(psych)
library(caret)
install.packages("pROC")
library(pROC)
library(datasets.load)
install.packages("nnet")
library(nnet)
#### Read data
mydata <- read.csv("D:/My project file/binary.csv")
View(mydata)
mydata <- read.csv(file.choose(),header = T)
str(mydata)
mydata$admit<- as.factor(mydata$admit)
str(mydata)
mydata$rank <- as.factor(mydata$rank)
str(mydata)

### split data
set.seed(1234)
 ind<- sample(2,nrow(mydata),replace = T,prob = c(.8,.2))
training <- mydata[ind==1,]
testing <- mydata[ind==2,]

summary(training)

### logistic model
m <- glm(admit~.,data = training,family = "binomial")
summary(m)

m <- glm(admit~.-gre,data = training,family = "binomial")
summary(m)
par(mfrow=c(2,2))
termplot(m)   ### partial for gpa is logodds
#### predictions
p <- predict(m,training,type = "response")
head(p)
head(training$admit)

#### misclassification Error  (22.2%)
 
pred1 <- ifelse(p>0.5,1,0)
pred1
### confusion matrix
table(Predicte=pred1,Actual=training$admit)   
(208+29)/306
### This model is correct 77 percent , as no model is 100 % accurate

#### prediction for the test data

p2 <- predict(m,testing,type = "response")

pred2 <- ifelse(p2>.5,1,0)
pred2

#### confusion matrix
table(predicted=pred2,actual=testing$admit)
(57+5)/94

#### Baseline Rate
table(mydata$admit)
273/400
### this is our basline rate, 


table
### sensitivity : how many class 1 truly predicted ,
### devided by that column
26/(69+26)
### this model which is actually predicting model by 73 percent accuacy
### is predicting class 1 with accuracy of 27  , which is very poor



### specificity :how many class o truly  predicted,
### devided by that column
204/211

### this model which is predicting model by 73 percent accuracy
### is predictin class 0 with  96 percent accuracy
## this model is biased or doing great job in classifying class o than 1

### Next lecture

mydata <- read.csv("D:/My project file/binary.csv")
View(mydata)
mydata <- read.csv(file.choose(),header = T)
str(mydata)
mydata$admit<- as.factor(mydata$admit)
str(mydata)
mydata$rank <- as.factor(mydata$rank)
str(mydata)

### split data
set.seed(1234)
ind<- sample(2,nrow(mydata),replace = T,prob = c(.8,.2))
training <- mydata[ind==1,]
testing <- mydata[ind==2,]

### logistic model
m <- glm(admit~.,data = training,family = "binomial")
summary(m)

m <- glm(admit~.-gre,data = training,family = "binomial")
summary(m)

p1 <- predict(m,training,type = "response")
dev.off()
hist(p1)  ### more values are concentrated to the left of .3


predct1 <-ifelse(p1>.5,1,0)
table(predicted=predct1,Actual=training$admit)
(208+29)/(208+73+15+29)






### ROC
p1 <- predict(m,training,type = "response")

r <- multiclass.roc(training$admit,p1,percent=TRUE)
### as we are using binary
### so controls is students are not admitted
### cases are student admitted
roc <- r[["rocs"]]
roc
### to extract information from roc
r1 <- roc[[1]]
plot.roc(r1,col="red",lwd = 5,main= "Roc curve with train data")
plot.roc(r1,
         print.auc = T,
         auc.polygon = T,
         grid = c(.1,.2),
         grid.col = c("red","green"),
         max.auc.polygon = T,
         auc.polygon.col = "light blue",
         print.thres = T,
         main="Roc curve")
#### Best threshold
coords(r1,"best",ret="threshold",transpose="FALSE")
auc(r1)
### 2nd Roc curve
### gre is not included.
## there are roc curve is not same

m <- glm(admit~.,data = training,family = "binomial")
summary(m)
p1 <- predict(m,training,type = "response")


r <- multiclass.roc(training$admit,p1,percent=TRUE)
roc <- r[["rocs"]]
roc
r2 <- roc[[1]]
plot(r2,add=T,col="red")
### if we want more specificity or sensitivity 
### adjust probability according to that.

### Why logistic Regression?
gpa <- runif(500,3,3.6)
gpa <- append(gpa,runif(500,3.4,4))
outcome<- rep(0,500)
outcome <- append(outcome,rep(1,500))
data <- data.frame(gpa,outcome)
str(data)
data$outcome <- as.factor(data$outcome)
str(data)
data %>% ggplot(aes(x=gpa,y= outcome,fill=outcome))+
  geom_boxplot()+
  ggtitle("Box Plot: admitted vs rejected",
          "simulated data")

### multiple linear regression

gpa <- runif(500,3,3.6)
gpa <- append(gpa,runif(500,3.4,4))
outcome<- rep(0,500)
outcome <- append(outcome,rep(1,500))
data <- data.frame(gpa,outcome)
### why not use this to make prediction
### rather than classifciation
## this graph shows that if i have 3.64 , i will have 
### correspoinding probability to get selected .
### but what will happen if i have rank of 3 as ,  
### proabibility is negative here
## similar if rank is more than 4 ,proabili >1 ,that 
### is not acceptble

data %>% ggplot(aes(x=gpa,y=outcome))+
  geom_point()+
  stat_smooth(method = "lm",se=0,lwd=.5)
  
 ### to solve the above problem let make a logistic model
str(data)
logistic <- glm(outcome~gpa,data = data,family = "binomial")
pred3 <- predict(logistic,data,type = "response")
head(pred3)
data %>% ggplot(aes(x=gpa,y=pred3))+
  geom_point()
### now values do not go above and below 1,0
### we have nice S shape curve
### response variable is logit function 
### this transformation helps in getting predicton
### more interpretable
### in case of multiple linear regressin interpretation
### in terms does not work properly

###__________________________________________________________________
### Multinomial Logistic Regression

d1 <- read.csv(file.choose(),header = T)
str(d1)
d1$NSP <- as.factor(d1$NSP)
str(d1)

#### These should one reference in multinomial logistic regression
### we want to compare suspected vs normal
### also we want to compare pathological vs normal
d1$NSP <- relevel(d1$NSP,ref = "1")


### partition of data
set.seed(1234)
ind <- sample(2,nrow(d1),replace = T,prob = c(.8,.2))
train <- d1[ind==1,]
test <- d1[ind==2,]
### Model
library(nnet)

mod <- multinom(NSP~.,data=train)
summary(mod)

### by looking at model we are going del variable
## which have less impact on the response variable
mod <- multinom(NSP~AC+FM+UC+DL+DP,data=train)
summary(mod)
names(mod)
### 2-tailed test
z<- summary(mod)$coefficients/summary(m)$standard.errors
(1-pnorm(abs(z),0,1))*2


### prediction
p <- predict(mod,train,type="class")
head(p)
head(train$NSP)
library(caret)
confusionMatrix(p,train$NSP)
#No Information Rate : 0.7846  baseline  


### prediction
p <- predict(mod,test,type="class")
head(p)
head(test$NSP)
library(caret)
confusionMatrix(p,test$NSP)

### Roc curve for multinomial logistic regresion

p <- predict(mod,test,type="prob")
head(p)
head(test$NSP)

r <- multiclass.roc(test$NSP,p,percent=T)
rs <- r[["rocs"]]
r1 <- rs[[1]][[2]]
r2 <- rs[[2]][[2]]
r3 <-rs[[3]][[2]]
table(test$NSP)
plot.roc(r1,col = "blue")
plot.roc(r2,add = T,col = "red")
plot.roc(r3,add = T,col = "yellow")





































































