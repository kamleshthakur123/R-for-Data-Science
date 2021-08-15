
#### Random forest , bootstrapping , extra gradient boosting
library(mlbench)
library(caret)
library(e1071)
library(lime)
#### Boston Housing file
data("BostonHousing")
mydata <- BostonHousing
set.seed(1234)
#### Data partition
set.seed(1234)
ind <- sample(2,nrow(mydata),replace = T,prob = c(.5,.5))
training <- mydata[ind==1,]
testing <- mydata[ind==2,]

### Regression & Bagging
set.seed(1234)
cvcontrol <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 2,
                          allowParallel = T)
set.seed(1234)
bag <- train(medv~.,data = training,
             method= "treebag",
             trControl=cvcontrol,
             importance=TRUE)
### importance is just to plot imp variables plot
plot(varImp(bag))
### plot,RMSE,R-Square
ba <- predict(bag,testing)
head(ba)
head(testing$medv)
plot(ba~testing$medv,main= "Predicted vs actual Medv-Test Data")
sqrt(mean((testing$medv-ba)^2))   ### RMSE
cor(ba,testing$medv)^2      ### R square
### Random Forest

set.seed(1234)
forest <- train(medv~.,data = training,
                method= "rf",
                trControl= cvcontrol,
                importance=T)
plot(varImp(forest))
plot(forest)
### prediction
a <- predict(forest,testing)
head(a)
head(testing$medv)
plot(a~testing$medv,
     main= "Predicted vs actual medv")
### calculate R2 and RMSE by own

### explaing predictions
explainer <- lime(testing[1:3,],forest,n_bins = 5)
explanation <- explain(x=testing[1:3,],
                       explainer = explainer,
                       n_features = 5)
plot_features(explanation)

### Extra Gradient Boosting
set.seed(1234)
boo <- train(medv~.,data = training,
             method="xgbTree",
             trControl=cvcontrol,
             tuneGrid= expand.grid(nrounds=500,
                                   max_depth=3,
                                   eta=.2,
                                   gamma=2.1,
                                   colsample_bytree=1,
                                   min_child_weight=1,
                                   subsample=1))
### gamma is for preventing overfitting
### try to increase gamma 
## max depth is how deep is ur tree
### eta is learinig rate which is default three
## try to work with first 3 parameters

plot(varImp(boo))

### plot ,RMSE, R sequare
p3 <- predict(boo,testing)
plot(p3~testing$medv,
     main= "p3 vs actual testing$medv")
### RMSE
sqrt(mean((p3-testing$medv)^2))
### R sequare
cor(p3,testing$medv)^2

####Classification

read.csv(file.choose(),header = T)
mydata <- read.csv(file.choose(),header = T)
mydata$NSP <- as.factor(mydata$NSP)
set.seed(1234)
ind <- sample(2,nrow(mydata),replace = T,prob = c(.5,.5))
training <- mydata[ind==1,]
testing <- mydata[ind==2,]
### Bagging

set.seed(1234)
cvcontrol <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 2,
                          allowParallel = T)
bag <- train(NSP~.,data = training,
             method="treebag",
             trControl=cvcontrol,
             importance= T)
plot(varImp(bag))
p <- predict(bag,testing)
confusionMatrix(p,testing$NSP)
### Random forest

forest <- train(NSP~.,data = training,
                method= "rf",
                trControl=cvcontrol,
                importance=T)
plot(varImp(forest))
p <- predict(forest,testing)
confusionMatrix(p,testing$NSP)

### Boosting
set.seed(1234)
boo <- train(NSP~.,data = training,
             method= "Xgbtree",
             trControl=cvcontrol,
             importance=T,
             tuneGrid=expand.grid(nrounds=500,
                                  max_depth=4,
                                  eta=.28,
                                  gamma=1.8,
                                  colsample_bytree=1,
                                  min_child_weight=1,
                                  subsample=1))
### plot graph and calculate the confusion matrix





























