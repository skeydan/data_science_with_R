library(ISLR)
library(e1071)
library(ROCR)

#######################################################################
###                         Support vector classifier               ###
###                         fake data                               ###
#######################################################################

set.seed(1)

# generate data
X <- matrix(rnorm(20*2), ncol=2)
X
y <- c(rep(-1,10), rep(1,10))
y
X[y==1, ] <- X[y==1,] + 1

plot(X, col=(3-y)) # not linearly separable

dat <- data.frame(x=X, y=as.factor(y)) # encode response as factor in order to do classification, not regression
dat

?svm
# cost
svmfit <- svm(y ~ ., data=dat, kernel="linear", cost=10,scale=FALSE)

# crosses are support vectors
plot(svmfit, dat) # linear decision boundary

# indices of support vectors
svmfit$index

summary(svmfit)

# try smaller cost
# margin is wider now
svmfit <- svm(y~., data=dat, kernel="linear", cost=0.1,scale=FALSE)
plot(svmfit, dat)
svmfit$index

# cross-validation
set.seed(1)
tune_out <- tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune_out)
bestmod <- tune_out$best.model
summary(bestmod)

# generate a test set
xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

# make predictions using the best model
ypred <- predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)

# predictions for cost=0.01, for comparison
svmfit <- svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
ypred <- predict(svmfit,testdat)
table(predict=ypred, truth=testdat$y)

# now do this on data that _is_ linearly separable!
X <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))
X[y==1, ] <- X[y==1,] + 3
plot(X, col=(y+5)/2, pch=19)

dat <- data.frame(x=X,y=as.factor(y))
svmfit <- svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit) # now we have just 2 support vectors
plot(svmfit, dat)

# wider margin
svmfit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat) # one training obs is misclassified, but chances are better on test set than before!




#######################################################################
###                         Support vector machine                  ###
###                         fake data                               ###
#######################################################################

set.seed(1)
X <- matrix(rnorm(200*2), ncol=2)
X[1:100,] <- X[1:100,]+2
X[101:150,] <- X[101:150,]-2
y <- c(rep(1,150),rep(2,50))
dat <- data.frame(x=X,y=as.factor(y))
plot(X, col=y)

train <- sample(200,100)

# fit a radial kernel
svmfit <- svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)
plot(svmfit, dat[train,]) # many training errors at this cost
summary(svmfit)

# increase cost, this has lower error but is probably overfitting
svmfit <- svm(y ~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])

# cross-validation
set.seed(1)
tune_out <- tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune_out)
# view predictions on test set
table(true=dat[-train,"y"], pred=predict(tune_out$best.model,newdata=dat[-train,]))



#######################################################################
###                         Receiver operating curves               ###
###                         fake data                               ###
#######################################################################

?prediction
?performance

rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf,...)}

# get the scores instead of just the class assignments
svmfit_opt <- svm(y~., data=dat[train,], kernel="radial",gamma=2, cost=1,decision.values=T)
fitted <- attributes(predict(svmfit_opt,dat[train,],decision.values=TRUE))$decision.values
fitted

par(mfrow = c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data") # training data!

# increase gamma, higher flexibility
svmfit_flex <- svm(y~., data=dat[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted <- attributes(predict(svmfit_flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red") # add to plot!

# now finally test on test data!!
# again, compare the more and the less flexible fits
fitted <- attributes(predict(svmfit_opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted <- attributes(predict(svmfit_flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")
# on the test set, the less flexible fit is clearly better!!



#######################################################################
###                         Multi-class SVM                         ###
###                         fake data                               ###
#######################################################################

set.seed(1)
X <- rbind(X, matrix(rnorm(50*2), ncol=2))
y <- c(y, rep(0,50))
X[y==0,2] <- X[y==0,2]+2
dat <- data.frame(x=X, y=as.factor(y))

par(mfrow=c(1,1))
plot(X,col=(y+1))

svmfit <- svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)


#######################################################################
###                         Multi-class SVM                         ###
###                         Khan gene expression data               ###
#######################################################################

data(Khan)
?Khan
names(Khan)

dim(Khan$xtrain)
dim(Khan$xtest)

length(Khan$ytrain)
length(Khan$ytest)

table(Khan$ytrain)
table(Khan$ytest)

#train
dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
# very many number of features -> use linear kernel!
out <- svm(y~., data=dat, kernel="linear",cost=10)
summary(out)

# confusion matrix on training set
table(out$fitted, dat$y) # NO training errors!

#test
dat_te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred_te <- predict(out, newdata=dat_te)

# confusion matrix on test set
table(pred_te, dat_te$y) # just 2 errors on test set

