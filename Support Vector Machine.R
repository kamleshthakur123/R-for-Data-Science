#### Support Vector Machine
data("iris")
str(iris)
library(ggplot2)
names(iris)

qplot(Petal.Length,Petal.Width,data = iris,
      colour= Species)

### Support Vector Machine

### we look for optimal separating hyperplane
## that can be achieved by maximum margin distance

library(e1071)
mymodel <-svm(Species~.,data = iris)
summary(mymodel)

plot(mymodel,data = iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,
                  Sepal.Length=4))
#### Confusion Matrix and Misclassification Error
pred <- predict(mymodel,iris)
tab <-table(Predicted= pred, Actual= iris$Species)

1-sum(diag(tab))/sum(tab)

### Using linear Kernel

mymodel <-svm(Species~.,data = iris,
              kernel="linear")
summary(mymodel)

pred <- predict(mymodel,iris)
tab <-table(Predicted= pred, Actual= iris$Species)

1-sum(diag(tab))/sum(tab)

### misclassificaton has been increased to .33 percent

mymodel <-svm(Species~.,data = iris,
              kernel="polynomial")
summary(mymodel)

pred <- predict(mymodel,iris)
tab <-table(Predicted= pred, Actual= iris$Species)

1-sum(diag(tab))/sum(tab)

plot(mymodel,data = iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,
                  Sepal.Length=4))





mymodel <-svm(Species~.,data = iris,
              kernel="sigmoid")
summary(mymodel)

pred <- predict(mymodel,iris)
tab <-table(Predicted= pred, Actual= iris$Species)

1-sum(diag(tab))/sum(tab)

plot(mymodel,data = iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,
                  Sepal.Length=4))

### fine tune the model to get the less misclassification error
## this is also hyperparameter optimisation
set.seed(123)
tmodel <-tune(svm,Species~., data = iris,
     ranges = list(epsilon=seq(0,1,.1),
                   cost=2^(2:7)))
plot(tmodel)
summary(tmodel)
### best Model
mymodel <-tmodel$best.model
summary(mymodel)

plot(mymodel,data = iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,
                  Sepal.Length=4))



































