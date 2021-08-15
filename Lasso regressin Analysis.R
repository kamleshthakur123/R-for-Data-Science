#### library Needed
library(caret)
library(glmnet)
library(mlbench)
library(psych)

data("BostonHousing")

str(BostonHousing)
pairs.panels(BostonHousing)
### scatter plot of every variables which are numeric

pairs.panels(BostonHousing[c(-4,-14)],cex=2)

### data partition
set.seed(222)
ind <- sample(2,nrow(BostonHousing),replace = T,
              prob = c(.7,.3))
Training <- BostonHousing[ind==1,]
Testing <- BostonHousing[ind==2,]

### custome control parameters
### verboselter is to see how the process is going on
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)
custom
### linear Model
set.seed(1234)
lm <- train(medv~.,Training,
            method= "lm",
            trControl = custom)
lm
### Result
lm$results

summary(lm)
par(mfrow=c(2,2))
plot(lm$finalModel)

#### Ridge Regression
### tries to shrink the coefficient 
###but keeps all the variable in the model
### glmnet package allows us to fit lass, ridge and elastic net model

set.seed(1234)
ridge <- train(medv~.,
               Training,
               method= "glmnet",
               tuneGrid= expand.grid(alpha=0,
                                     lambda=seq(.0001,1,length=5)),
               trControl=custom)

### lambda is the strength of penalty on the coeffi.
### as we increase lamba we are increasing the penality
### lamba will increase ,it will make coeffiecient to shrink

### plot
plot(ridge)

### it is clear from the plot for higher value of lambda 
### error increases
ridge
plot(ridge$finalModel,xvar = "lambda",label = T)
### when lambda is 8 or9 all the cofficeints are zero
### lambda does not make coefficient of those variables which'
### are not contributing in the model

### plot for important variable
plot(varImp(ridge,scale = F))

#### Lasso Regression
set.seed(1234)
lasso <- train(medv~.,Training,
               method= "glmnet",
               tuneGrid= expand.grid(alpha=1,
                                     lambda=seq(.0001,1,length=5)),
               trControl= custom)

### Plot 
par(mfrow=c(1,1))
plot(lasso)
plot(lasso$finalModel,xvar="lambda",
     label = T)
plot(varImp(lasso,scale = F))

#### Elastic net Regressin
set.seed(1234)
en <- train(medv~.,Training,
            method= 'glmnet',
            tuneGrid=expand.grid(alpha=seq(0,1,length=10),
                                 lambda=seq(.0001,1,length=5)),
            trControl=custom)
### plot Result
plot(en)
plot(en$finalModel,xvar = "lambda",label = T)
plot(varImp(en,scale = F))

#### compare Models
model_list <- list(linearmodel= lm, Ridge=ridge,
                   Lasso=lasso,elasticnet=en)
#### to compare the model
res <- resamples(model_list)
summary(res)
xyplot(res,metric="RMSE")   ### compares ridge and linear model

### Best model
en$bestTune
best <- en$finalModel
coef(best,en$bestTune$lambda)

### save final model
fm <- saveRDS(en,"final_model.rds")
### predictio

predict(fm,Training)
### also calculate Rmse





































