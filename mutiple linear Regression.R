####
library(DAAG)
library(ggplot2)
library(psych)

#### Book example
str(allbacks)
ggplot(allbacks,aes(x= volume,y=weight,col= cover))+
  geom_point()+
  geom_smooth(se=0,method = "lm")
pairs.panels(allbacks[1:3])


### multipla linear model
m <- lm(weight~ volume+area,allbacks)
summary(m)

p <- predict(m,allbacks)
plot(p~allbacks$weight)
 c <- cor(p, allbacks$weight)
 Rsqr <- c^2
Rsqr

#### CI for coefficients
confint(m)
t <- qt(.975,12)
t
.70821-(2.178813*0.060107)
.70821+(2.178813*0.060107)

### MLR without intercept
m <- lm(weight ~-1+volume+area, allbacks)
summary(m)
par(mfrow=c(2,2))
plot(m)
dev.off()

#### log transformation
data("AirPassengers")
?AirPassengers
summary(AirPassengers)
hist(AirPassengers)
### right skewed
plot(AirPassengers)
hist(log(AirPassengers))
plot(log(AirPassengers))

#### Irish Hill race
data("nihills")
str(nihills)
pairs.panels(nihills)
pairs.panels(log(nihills))

m1 <- lm(log(time)~log(dist)+log(climb),nihills)
summary(m1)
m1 <- lm(log(time)~log(dist)+log(climb/dist),nihills)
summary(m1)
 ### to calculate the corelation between distance and climb

c <- cor(log(nihills$climb),log(nihills$dist))
Rsqr <- c^2
Rsqr

with(nihills, cor(log(dist),log(climb/dist)))

### ouse brain weight
str(litters)
pairs.panels(litters)
summary(lm(brainwt~lsize+bodywt,litters))

#### influence on the regression coefficients

m <- lm(weight~ volume+area,allbacks)
summary(m)
d <- dfbetas(m)
### it shows that if first point will remove
### change in intercept will be like this as shown in dfbetas
### function
m <- lm(weight ~-1+volume+area, allbacks)
par(mfrow=c(2,2))
plot(d[,1])
plot(d[,2])

### this is the another way to see the suspicious point
### when point lies outside the scale , considered as suspicious.


AIC(m)
BIC(m)
### thse are basically used in the camparison 


#### Multiple linear Regression part2
library(DAAG)
library(ggplot2)
library(psych)
library(compositions)
library(car)
library(olsrr)
library(PerformanceAnalytics)
library(UsingR)
library(dplyr)



data("Salaries")
str(Salaries)
m4 <- lm(salary ~., Salaries)
summary(m4)

#### Data patition
set.seed(25)
ind <- sample(2,nrow(Salaries),
              replace = T,prob = c(.5,.5))
trainig <- Salaries[ind==1,]
testing  <- Salaries[ind== 2,]
head(trainig)
head(testing)

m4 <- lm(salary ~., trainig)
summary(m4)



### try all possible steps
### here we have prepared model by taking 1 model , 2model,
### or so on or by applying differnt permutation.

all <- ols_step_all_possible(m4)
all

##ols is ordinary least sqr
### this method run all possible combination as shown above

### To find best model on the basis of AIC
m5 <- stepAIC(m4,direction = "both",trace = F)
m5

### Predicton
p <- predict(m5, testing)
head(p)
head(testing$salary)

### plot them on scatter
plot(p~testing$salary)
c <- cor(p,testing$salary)
R2 <- c^2
R2
### as valur of R2 is low,performance is 
### very low for this model.

#### interaction 
### it is basically to know the joint 
### effect of two or more than varibles

m6 <- lm(salary~.,Salaries)
summary(m6)
m6 <- lm(salary~.*.,Salaries)
summary(m6)
m6 <- lm(salary ~.+discipline:yrs.service,Salaries)
summary(m6)

Salaries %>% ggplot(aes(x=yrs.service,y=salary,color=discipline))+
  geom_point()+
  stat_smooth(se=0,method = "lm")
### from the plot it is clear that for disciplineB
#### salary increse more with increase in year.
### this is what we see from the interaction,discplienA with
### yrs.service has different impact on reponse variable as
### compare to discipline B with yrs . service.
### if lines are crossed with each other , these is interaction
### if not crosses , no interaction.


### Polynomial
m7 <- lm(salary~yrs.service,trainig)
summary(m7)
data("Salaries")
m7 <- lm(salary ~ poly(yrs.service,4),trainig)
summary(m7)

### slide 20-22
data(Coxite)
str(Coxite)
View(Coxite)

data <- data.frame(Coxite)
str(data)

pairs.panels(data)

### hisher version of this graph

chart.Correlation(data)
### it tells that which of the corelaton is statistically
### significant

#### LR
m8 <- lm(porosity ~.,data)
summary(m8)

### these is contradiction, as none of the variable is significant
## there will be multicolin.
vif(m8)

m8 <- lm(porosity ~.-E,data)
vif(m8)
### which variable should we keep or nor
### can be explained from the graph
### showig significant corelation

m8 <- lm(porosity ~.-E-D,data)
summary()
vif(lm(depth~.-porosity,data = data))

m8 <- lm(porosity ~.-E-D-A,data)
vif(m8)
### here vaiables are small , so we adopted this method
### if vaiables are so large , we can't adopt this method 
### of elimination variables
### we use lasso regression here 
### or Ridge Regression
### or Elastic Net
### Principle component Analysis

### Read a Csv file
mydata <- read.csv("C:/Users/HP/Downloads/vehicle.csv")
str(mydata)
pairs.panels(mydata)
pairs.panels(mydata[c(4,5,6)])

new <- mydata[mydata$Mileage>20,]
m9 <- lm(lc ~lh+mc,new)
summary(m9)
vif(m9)




































































