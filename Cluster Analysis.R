### Cluster Analysis

read.csv(file.choose(),header = T)
mydata <-read.csv(file.choose(),header = T)
str(mydata)
pairs(mydata[2:9])

### Scatter PLot
plot(mydata$Fuel_Cost~mydata$Sales,
     data= mydata )

### labeling this plot by company name
with(mydata,text(mydata$Fuel_Cost~mydata$Sales,
                 labels=mydata$Company,
                 pos=4))
### Normalise 
## Because one variable can play a very dominant role
## in the clustering that will give very poor model 
library(dplyr)
### for removig first column of company 
z <- mydata[,-1]    
 ### here we have just removed one column,these is no surpise of using c(1,1)
means <- apply(z, 2, mean, na.rm=T)
sd <- apply(z,2,sd)

### for normalisation we use scale funtion
### This scale function calculates , automaticaly the value of Z , 
### by computing the formula
nor <- scale(z,center = means,scale = sd)


### calculate distance matrix
## now use this data to find the distance to form cluster
distance <- dist(nor)
print(distance,digits = 3)

### still we cant form cluster on the basid of distance
### we need algorithims to create clusters

### Hierarchical agglomerative clustering
## using complete distance
mydata.hclust <- hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust,labels = mydata$Company,
     main = "Default from hclust")
## for some modificatin we can plot like this
plot(mydata.hclust,hang = -1)

### hierarchical agglomerative clustering using average
mydata.hclust <- hclust(distance,method = "average")
plot(mydata.hclust,hang = -1)
rect.hclust(mydata.hclust,k=5)

### cluster membership
member <- cutree(mydata.hclust,3)
table(member)
### Characterising clusters





#### Silhoutte plot
install.packages("cluster")
library(cluster)
plot(silhouette(cutree(mydata.hclust,3),distance))
### if silhouette width is quite high 
### then clustering formation will be poor
### as shown in the  plot
### in cluster 1, silhoutte width is .015 which is less 
### these is one company having silhoutte width is in negative 
## which indicates poor formation of clustering formation


### Scree Plot

wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

### non hierarhical clustering
set.seed(123)

kc <- kmeans(nor,4)
### within sum of square , should be low for good cluster formation
### between sum of square , should be high between the cluster for good
### cluster formation

clusplot(mydata,
          kc$cluster,
          color=T,
          shade=T,
          labels=2,
          lines=0)
clusplot(mydata,
         kc$cluster)
































































