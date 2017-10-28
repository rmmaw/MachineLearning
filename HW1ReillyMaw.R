rm(list=ls())
##################### #### QUESTION 1 #### #####################
home_table <- read.table("HomePrices.txt", stringsAsFactors = F, header = T)
meanMSE <- sum((home_table$medv-mean(home_table$medv))**2)/nrow(home_table)
meanMSE
variance <- var(home_table$medv)*((nrow(home_table)-1)/nrow(home_table))
variance
scaled_ht<-scale(home_table[-13] ,center=TRUE)
scaled_ht<-cbind(scaled_ht,home_table[13])
head(scaled_ht)
set.seed(5072)
n <- nrow(scaled_ht)
trainprop <- 0.75  
validateprop <- 0.15
train  <-  sample(n, trainprop * n)
validate  <-  sample(setdiff(1:n, train), validateprop * n) 
test <- setdiff(setdiff(1:n, train), validate)
trainset <- scaled_ht[train,]
validateset <- scaled_ht[validate,]
testset <- scaled_ht[test,]
head(trainset,1)
head(validateset,1)
head(testset,1)
train.x <- trainset[-13]
train.y <- trainset$medv
validate.x <- validateset[-13]
validate.y <- validateset$medv
test.x <- testset[-13]
test.y <- testset$medv
seqx<-seq(19,1,-2)
require(FNN)
numreps <- 10
validate.errors <- rep(0, length(seqx))
train.errors <- rep(0, length(seqx))
for(i in 1:length(seqx)) {
  knn.pred <- knn.reg(train.x, validate.x,  train.y, k = seqx[i])
  validate.errors[i] <- mean((knn.pred$pred - validate.y)^2)
  
  knn.pred <- knn.reg(train.x, train.x,  train.y, k = seqx[i])
  train.errors[i] <- mean((knn.pred$pred - train.y)^2)
}
plot(NULL, NULL, type='n', xlim=c(19, 1), ylim=c(0,max(c(validate.errors, train.errors))), xlab='Increasing Flexibility (Decreasing k)', ylab='Mean Squared Errors', main='MSEs as a Function of \n Flexibility for KNN Classification')
lines(seq(19, 1, -2), validate.errors, type='b', col=2, pch=16)
lines(seq(19, 1, -2), train.errors, type='b', col=1, pch=16)
legend("topright", legend = c("Validation MSEs", "Train MSEs"), col=c(2, 1), cex=.75, pch=16)
l <- which.min(validate.errors)
m <-which.min(train.errors)
sprintf("My 'best' validate MSE occured at k = %i and prduced a MSE of %f", seqx[l],validate.errors[which.min(validate.errors)])
sprintf("My 'best' training MSE occured at k = %i and prduced a MSE of %f", seqx[m],train.errors[which.min(train.errors)])
#predict medv for test set using optimal k ound for validate
knn.pred <- knn.reg(train.x, test.x,  train.y, k = seqx[l])
mytable <- mean((test.y-knn.pred$pred)^2)
print(paste("Test set error rate was ",(mytable)))



##################### #### QUESTION 2 #### #####################
LDT <- read.csv(file="LoanData.csv", header=TRUE, sep=",")
yesError <- sum(LDT$loan.repaid=="Yes")/nrow(LDT)
yesError
scaled_ldt<-scale(LDT[-8] ,center=TRUE)
scaled_ldt<-cbind(scaled_ldt,LDT[8])
head(scaled_ldt)
set.seed(5072)
n <- nrow(scaled_ldt)
trainprop <- 0.75  
validateprop <- 0.15
train  <-  sample(n, trainprop * n)
validate  <-  sample(setdiff(1:n, train), validateprop * n) 
test <- setdiff(setdiff(1:n, train), validate)
trainset <- scaled_ldt[train,]
validateset <- scaled_ldt[validate,]
testset <- scaled_ldt[test,]
head(trainset,1)
head(validateset,1)
head(testset,1)
train.x <- trainset[-8]
train.y <- trainset$loan.repaid
validate.x <- validateset[-8]
validate.y <- validateset$loan.repaid
test.x <- testset[-8]
test.y <- testset$loan.repaid
seqx<-seq(19,1,-2)
require(FNN)
numreps <- 10
validate.errors <- rep(0, length(seqx))
train.errors <- rep(0, length(seqx))
for(i in 1:length(seqx)) {
  knn.pred <- knn(train.x, validate.x,  train.y, k = seqx[i])
  validate.errors[i] <- mean(validate.y != knn.pred)
  
  knn.pred <- knn(train.x, train.x,  train.y, k = seqx[i])
  train.errors[i] <- mean(train.y != knn.pred)  
}
plot(NULL, NULL, type='n', xlim=c(19, 1), ylim=c(0,max(c(validate.errors, train.errors))), xlab='Increasing Flexibility (Decreasing k)', ylab='Mean Squared Errors', main='MSEs as a Function of \n Flexibility for KNN Classification')
lines(seq(19, 1, -2), validate.errors, type='b', col=2, pch=16)
lines(seq(19, 1, -2), train.errors, type='b', col=1, pch=16)
legend("topleft", legend = c("Validation MSEs", "Train MSEs"), col=c(2, 1), cex=.75, pch=16)
l <- which.min(validate.errors)
m <-which.min(train.errors)
sprintf("My 'best' validate error occured at k = %i and prduced a validate error of %f", seqx[l],validate.errors[which.min(validate.errors)])
sprintf("My 'best' training MSE occured at k = %i and prduced a MSE of %f", seqx[m],train.errors[which.min(train.errors)])
knn.pred <- knn(train.x, test.x,  train.y, k = seqx[l])
mytable <- mean((knn.pred != test.y)^2)
print(paste("Test set error rate was ",(mytable)))



##################### #### QUESTION 3 #### #####################
set.seed(5072)
validate.errors<- c()
test.errors <- c()
home_table <- read.table("HomePrices.txt", stringsAsFactors = F, header = T)
seqx<-seq(19,1,-2)
for (i in 1:50){
  validate.mse <- 0
  n <- nrow(scaled_ht)
  trainprop <- 0.75  
  validateprop <- 0.15
  train  <- sample(n, trainprop * n)
  validate  <- sample(setdiff(1:n, train), validateprop * n) 
  test <- setdiff(setdiff(1:n, train), validate)
  trainset <- scaled_ht[train,]
  validateset <- scaled_ht[validate,]
  testset <- scaled_ht[test,]
  head(trainset,1)
  head(validateset,1)
  head(testset,1)
  train.x <- trainset[-13]
  train.y <- trainset$medv
  validate.x <- validateset[-13]
  validate.y <- validateset$medv
  test.x <- testset[-13]
  test.y <- testset$medv
  require(FNN)
  for (h in 1:length(seqx)){
    knn.pred <- knn.reg(train.x, validate.x,  train.y, k = seqx[h])
    validate.mse[h] <- mean((knn.pred$pred - validate.y)^2)
  }
  validate.errors[i] <- validate.mse[which.min(validate.mse)]
  knn.pred <- knn.reg(train.x, test.x,  train.y, k = seqx[which.min(validate.mse)])
  test.mse <- mean(((test.y - knn.pred$pred)^2))
  test.errors[i] <- test.mse
  
  }
testMean<- rep(mean(test.errors),50)
validateMean<- rep(mean(validate.errors),50)
print(paste("Mean validate MSE is", validateMean[1]))
print(paste("Standard Deviation validate MSE is", sd(validate.errors)))
print(paste("Mean test MSE is", testMean[1]))
print(paste("Standard Deviation test MSE is", sd(test.errors)))
plot(NULL, NULL, type='n', xlim=c(0, 50), ylim=c(0,max(c(validate.errors, test.errors))), xlab='Increasing Flexibility (Decreasing k)', ylab='Mean Squared Errors', main='MSEs as a Function of \n Flexibility for KNN Classification')
lines(seq(1:50), validate.errors, type='b', col=2, pch=16,lty = 1)
lines(seq(1:50), test.errors, type='b', col=1, pch=16)
lines(seq(1:50), testMean, type='l', col=1, lty="dashed")
lines(seq(1:50), validateMean, type='l', col=2, lty="dashed")
legend("topleft", legend = c("Validation MSEs","Validation MSE Mean", "Test MSEs","Test MSEs Mean"), col=c(2,2,1,1), cex=.55, pch=c(16,NA,16,NA), lty=rep("dashed",4))
l <- which.min(validate.errors)
m <-which.min(test.errors)
print("We were not unlucky in Question 1. The mean MSE we found in Q1 was within one standard deviation of 50 sample MSE's taken in Q3.")



##################### #### QUESTION 4 #### #####################
college <- read.csv(file="applications.train.csv", header=TRUE, sep=",")
lm.fit <- lm(Applications ~ ., data =college)
summary(lm.fit)
# removing the variables that werent statistically significant in the linear regression summary above 
college<-college[,-c(2,3,5,7,8,9,12,13,14,16) ]
n <- nrow(college)
seqx<-seq(19,1,-2)
validate.errors<- c()
kval<-c()
test.errors <- c()
for (i in 1:50){
  validate.mse <- 0
  n <- nrow(college)
  trainprop <- 0.75  
  validateprop <- 0.15
  train  <- sample(n, trainprop * n)
  validate  <- sample(setdiff(1:n, train), validateprop * n) 
  test <- setdiff(setdiff(1:n, train), validate)
  trainset <- college[train,]
  validateset <- college[validate,]
  testset <- college[test,]
  head(trainset,1)
  head(validateset,1)
  head(testset,1)
  train.x <- trainset[-1]
  train.y <- trainset$Applications
  validate.x <- validateset[-1]
  validate.y <- validateset$Applications
  test.x <- testset[-1]
  test.y <- testset$Applications
  require(FNN)
  for (h in 1:length(seqx)){
    knn.pred <- knn.reg(train.x, validate.x,  train.y, k = seqx[h])
    validate.mse[h] <- mean((knn.pred$pred - validate.y)^2)
  }
  l <- which.min(validate.mse)
  kval <-cbind(kval,seqx[l])
  validate.errors[i] <- validate.mse[which.min(validate.mse)]
  knn.pred <- knn.reg(train.x, test.x,  train.y, k = seqx[which.min(validate.mse)])
  test.mse <- mean(((test.y - knn.pred$pred)^2))
  test.errors[i] <- test.mse
  m<- which.min(test.mse)
}
testMean<- rep(mean(test.errors),50)
validateMean<- rep(mean(validate.errors),50)
plot(NULL, NULL, type='n', xlim=c(0, 50), ylim=c(0,max(c(validate.errors, test.errors))), xlab='Increasing Flexibility (Decreasing k)', ylab='Mean Squared Errors', main='MSEs as a Function of \n Flexibility for KNN Classification')
lines(seq(1:50), validate.errors, type='b', col=2, pch=16,lty = 1)
lines(seq(1:50), test.errors, type='b', col=1, pch=16)
lines(seq(1:50), testMean, type='l', col=1, lty="dashed")
lines(seq(1:50), validateMean, type='l', col=2, lty="dashed")
legend("topleft", legend = c("Validation MSEs","Validation MSE Mean", "Test MSEs","Test MSEs Mean"), col=c(2,2,1,1), cex=.55, pch=c(16,NA,16,NA), lty=rep("dashed",4))
x<- table(kval)
x
print("k=3 is my models prediction for the lowest MSE")


