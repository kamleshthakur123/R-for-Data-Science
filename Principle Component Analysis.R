
#### Data iris
iris
str(iris)
summary(iris)

### Partition data
set.seed(111)
ind <- sample(2,nrow(iris),replace = T,prob = c(.7,.3))
train <- iris[ind==1,]
test <- iris[ind==2,]

#### scatter plots and correlation coefficeints

library(psych)
pairs.panels(train[,-5],)
### as it is clear from the graph there is 
### high colinearity between independent variables
#### this will lead to the multicolinearity
### hence predtiction will not be accurate.

### this can be handled by principal componet analysis.

pc <- prcomp(train[,-5],
             center = T,
             scale. = T)    ### sd for each variable

### All the four variables are normalised
names(pc)
pc$center
pc$scale
pc$x
print(pc)
summary(pc)

#### orthoganility of principal componet analysis
pairs.panels(pc$x,
             gap=0,
             bg=c("red","yellow","blue")[train$Species])
### it reduces the multicolinearity between different variables

#### Bi-Plot
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
g <-ggbiplot(pc,
             obs.scale = 1,
             var.scale = 1,
             groups = train$Species,
             elipise=TRUE,
             circle = TRUE,
             ellipse.prob = .68)
g<- g+scale_color_discrete(name="")
g <- g+theme(legend.direction = "horizontal",
             legend.position = "top")
print(g)

### this graph shows that there is negative corelation
### between pc2 and sepal feaute and study byurself
### about pc1

#### prediction with principal component analysis
predict(pc,train) -> trg

trg
head(trg)
trg <- data.frame(trg,train[5])
trg
predict(pc,test) -> tst
tst <- data.frame(test,test[5])
tst

#### multiple logistic regression model with first two pcs
install.packages("nnet")
library(nnet)
### for only species setosa
trg$species <- relevel(trg$Species,ref = "setosa")
mymodel <- multinom(Species~PC1+PC2,data = trg)
summary(mymodel)
#### confusion matrix and missclassification error
p <- predict(mymodel,trg)
tab <- table(p,trg$Species)
tab
### also find out the missclassificatin error









