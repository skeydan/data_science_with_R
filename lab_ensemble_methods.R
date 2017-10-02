library(ISLR)
library(randomForest)
library(MASS)
library(gbm)

#######################################################################
###                         Bagging                                 ###
###                         Boston                                  ###
#######################################################################

data(Boston)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
boston_test <- Boston[-train,"medv"]
set.seed(1)

?randomForest
# mtry = number of candidate variables sampled at each split
# for bagging, this = number of variables overall
bag_boston <- randomForest(medv ~ ., data=Boston, subset=train, mtry=13, importance=TRUE)
bag_boston

yhat_bag <- predict(bag_boston, newdata = Boston[-train,])
plot(yhat_bag, boston_test)
abline(0,1)
mean((yhat_bag - boston_test)^2) # MSE

# use 25 instead of default 500 trees
bag_boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat_bag <- predict(bag_boston,newdata=Boston[-train,])
mean((yhat_bag-boston_test)^2)


#######################################################################
###                         random forest                           ###
###                         Boston                                  ###
#######################################################################

# use 6 variables at every split
rf_boston <- randomForest(medv ~ .,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat_rf <-  predict(rf_boston,newdata=Boston[-train,])
mean((yhat_rf-boston_test)^2)
importance(rf_boston)
varImpPlot(rf_boston)


#######################################################################
###                         boosting                                ###
###                         Boston                                  ###
#######################################################################

set.seed(1)
?gbm
boost_boston <- gbm(medv ~ ., data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost_boston)

par(mfrow=c(1,2))
# partial dependence plots
# marginal effect of variable after integrating out the others
plot(boost_boston,i="rm")
plot(boost_boston,i="lstat")

# predict on test set
yhat_boost <- predict(boost_boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat_boost-boston_test)^2) # test MSE

# increase shrinkage
boost_boston <- gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat_boost <- predict(boost_boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat_boost-boston_test)^2) # slightly reduced MSE




