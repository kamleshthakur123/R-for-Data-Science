#### Decision Tree
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(pROC)
library(tree)

#### Classification Tree
str(spam7)    ### require  rpart package
mydata <- spam7
#### Split Data
set.seed(1234)
ind <- sample(2,nrow(mydata),replace = T,prob = c(.5,.5))
training <- mydata[ind==1,]
testing <- mydata[ind==2,]

#### Tree Model
tree <- rpart(yesno ~.,data = training)
rpart.plot(tree)
printcp(tree)
plotcp(tree)

tree <- rpart(yesno ~.,data = training,cp=.001)
rpart.plot(tree)
printcp(tree)
plotcp(tree)

### we can see from model that it has started overfitting
### there are few data points on the basis of whick
### it is making prediction that is not good
###  it is a case of overfitting
### we cant rely on rule  

### confusion matrix - train
p <- predict(tree,testing,type = "class")

library(caret)
confusionMatrix(p,testing$yesno)
confusionMatrix(p,testing$yesno, positive= "y")

### ROC
p1 <- predict(tree,testing,type = "prob")
head(p1)
head(training$yesno)
p1 <- p1[,2]
head(p1)
r <- multiclass.roc(testing$yesno,p1,percent=T)
roc <- r[["rocs"]]
r1 <- roc[[1]]

plot.roc(r1,
         print.auc = T,
         auc.polygon = T,
         grid = c(.1,.2),
         grid.col = c("green","red"),
         max.auc.polygon = T,
         auc.polygon.col = "lightblue",
         print.thres = T,
         main= "ROC curves")
### Regression Tree

data("BostonHousing")
str(BostonHousing)

mydata <- BostonHousing
### data partition
set.seed(1234)
ind <- sample(2,nrow(mydata),replace = T,prob = c(.5,.5))
Training <- mydata[ind==1,]
Testing <- mydata[ind==2,]

### Regression Tree
tree <- rpart(medv~.,Training)
tree
rpart.plot(tree)
printcp(tree)
plotcp(tree)
rpart.rules(tree)
print(tree)
summary(tree)

tree <- rpart(medv~.,Training,cp=.001)
tree
rpart.plot(tree)
printcp(tree)
plotcp(tree)
## we increase the cp value ,it becomes difficult to 
### predict true value on the basis of few data points 
### remaining

### predict
p <- predict(tree,Training)
sqrt(mean((Training$medv-p)^2))   ### RMSE
(cor(Training$medv,p))^2      ### R^2



























