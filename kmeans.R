#https://www.r-bloggers.com/k-means-clustering-in-r/

library(datasets)
head(iris)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster

table(irisCluster$cluster, iris$Species)

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()

# Determine number of clusters
mydata <- iris
mydata$Species <- NULL

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
for (i in 2:10) wss[i] <- sum(kmeans(mydata, centers=i)$betweenss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Between groups sum of squares")








x<-matrix(1:20,ncol=4)
apply(x,1,mean)  #1表示对行进行处理，2表示对列进行处理, mean求平均数
apply(x,2,var)




