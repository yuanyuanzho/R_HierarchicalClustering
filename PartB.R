
tmp <- seq(60, 300, by=4)
x <- c()
for (i in tmp){
  i <- i * (pi/180)
  x <- c(x,i)
}
y <- sin(x) + runif(length(x),0,0.15)

set.seed(10)
plot(x,y)


x<-c(1.047198, 1.117011, 1.186824, 1.256637, 1.326450)
data.frame(x)


for(i in 2:15){ 
  xpower <- x^i
  colname <- paste("x_",i,sep = "")
  write.table(xpower,append = FALSE, quote = FALSE, col.names = colname)
}



liner <- function(data, power, models_to_plot)
{
  predictors <- matrix(x)
  tmp <- seq(2,power+1) 
  for(i in tmp)
    x <- paste("x_",i,sep = "")
  if(power >= 2)
    rbind(predictors, x)

}


# Ridge Regression
cement <- data.frame(X1 = c(7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10), 
                     X2 = c(26,29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68), 
                     X3 = c(6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8), 
                     X4 = c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26, 34, 12, 12), 
                     Y = c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5, 93.1, 115.9, 83.8, 113.3, 109.4))
                             
lm.sol <- lm(Y ~ ., data = cement)
summary(lm.sol)

install.packages("car")
library(car)
vif(lm.sol)



library(MASS)
ridge.sol <- lm.ridge(Y ~ ., lambda = seq(0, 150, length = 151), data = cement, model = TRUE)
names(ridge.sol)

ridge.sol $lambda[which.min(ridge.sol$GCV)]
ridge.sol$coef[which.min(ridge.sol$GCV)]

par(mfrow = c(1, 2))
matplot(ridge.sol$lambda, t(ridge.sol$coef), xlab = expression(lamdba), ylab = "Cofficients", 
        type = "l", lty = 1:20)
abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])

plot(ridge.sol$lambda, ridge.sol$GCV, type = "l", xlab = expression(lambda), 
     ylab = expression(beta))
abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])

par(mfrow = c(1, 1))

#install.packages("ridge")
library(ridge)
mod <- linearRidge(Y ~ ., data = cement)
summary(mod)


# Lasso Regression
install.packages("lars")
library(lars)

myData <- read.csv("/Users/eavy/Desktop/data.csv")
myData <- as.matrix(myData)
x<- myData[, 3:5]
y<- myData[,6]
la<-lars(x,y,type = "lar")
summary(la)

coef <- coef.lars(la,mode = "step", s= 2)
coef[coef!=0]
plot(la)













#------------------





