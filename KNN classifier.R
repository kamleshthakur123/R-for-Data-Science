### KNN Classifier
### This method is used for the classificaton and regressin method
### if supervised learning
install.packages("caret")
install.packages("pROC")
install.packages("mlbench")
library(caret)
library(pROC)
library(mlbench)

#### Classification with KNN method
data <- read.csv(file.choose(),header = T)
str(data)

data$admit[data$admit==0] <- "No"
data$admit[data$admit==1] <- "Yes"
str(data)

data$admit <- as.factor(data$admit)
str(data)

### DAta partition
set.seed(1234)
ind <- sample(2,nrow(data),replace = T,prob = c(.7,.3))
Training <- data[ind==1,]
Testing <- data[ind==2,]


#### KNN method
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 5)
set.seed(2222)
fit <- train(admit~.,
             data = Training,
             method= "knn",
             tuneLength= 20,
             trControl=trControl,
             preProc= c("center","scale"))

#### Model Performance
plot(fit)
varImp(fit)
### predcition
pred <- predict(fit,newdata = Testing)
cbind(head(pred),head(Testing$admit))
confusionMatrix(pred,Testing$admit)

### some modification
trControl <- trainControl(method = "repeatedcv", #repeated cross-validation
                          number = 10,  # number of resampling iterations
                          repeats = 3,  # sets of folds to for repeated cross-validation
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)  # classProbs needed for ROC
set.seed(1234)
fit <- train(admit ~ ., 
             data = training,
             tuneGrid   = expand.grid(k = 1:50),
             method = "knn",
             tuneLength = 20,
             metric     = "ROC", 
             trControl = trControl,
             preProc = c("center", "scale"))  # necessary task

# Model performance
fit
plot(fit)
varImp(fit)

pred <- predict(fit, newdata = test )
confusionMatrix(pred, test$admit, positive = 'yes' )
#### Example 2 Boston Housing (Regression)

data("BostonHousing")
data <- BostonHousing
str(data)

### data partition

set.seed(1234)
ind <- sample(2,nrow(data),replace = T,prob = c(.7,.3))
Training <- data[ind==1,]
Testing <- data[ind==2,]

### KNN model
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)
fit <- train(medv ~., 
             data = Training,
             method= "knn",
             trControl=trControl,
             tunelength=20,
             preProc= c("center","scale"),
             tuneGrid = expand.grid(k=1:70))  

### Model Performance
plot(fit)
varImp(fit)

pred <- predict(fit,Testing)

### plot model to see the predciton vs actual value










































