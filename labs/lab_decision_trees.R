library(ISLR)
library(tree)
library(MASS)


#######################################################################
###                     Classification trees                        ###
###                         Carseats                               ###
#######################################################################

data("Carseats")
?Carseats

# binarize Sales for classification
High <- ifelse(Carseats$Sales <= 8, "No", "Yes")
High

Carseats <- data.frame(Carseats, High)
str(Carseats)

?tree
tree_carseats <- tree(High ~ . -Sales, Carseats)
summary(tree_carseats) 

# view the tree
tree_carseats
plot(tree_carseats)
text(tree_carseats, pretty=0)

# now do it properly with training and test sets
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats_test <- Carseats[-train, ]
High_test <- High[-train]
# train on training set
tree_carseats <- tree(High ~ . -Sales, Carseats, subset = train)
# predict on test set
tree_pred <- predict(tree_carseats, Carseats_test, type="class")
table(tree_pred, High_test)
# accuracy
mean(tree_pred == High_test)


# use cross-validation to determine optimal tree complexity
?cv.tree
cv_carseats <- cv.tree(tree_carseats, FUN=prune.misclass) # use error rate instead of default deviance
names(cv_carseats) # k is cost complexity parameter - dev really stands for error rate here
cv_carseats

par(mfrow=c(1,2))
plot(cv_carseats$size,cv_carseats$dev,type="b") # tree size against error rate
plot(cv_carseats$k,cv_carseats$dev,type="b")    # cost complexity against error rate

# now prune to get the best tree
prune_carseats <- prune.misclass(tree_carseats,best=9)
plot(prune_carseats)
text(prune_carseats,pretty=0)
tree_pred <- predict(prune_carseats, Carseats_test, type="class")
table(tree_pred,High_test)
# accuracy
mean(tree_pred == High_test) # tree is more interpretable AND performs better

# this larger tree performs worse
prune_carseats <- prune.misclass(tree_carseats, best=15)
plot(prune_carseats)
text(prune_carseats, pretty=0)
tree_pred=predict(prune_carseats, Carseats_test, type="class")
table(tree_pred,High_test)
# accuracy
mean(tree_pred == High_test)



#######################################################################
###                     Regression trees                            ###
###                         Boston                                  ###
#######################################################################

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree_boston <- tree(medv ~ ., Boston, subset=train)
summary(tree_boston)

par(mfrow=c(1,1))
plot(tree_boston)
text(tree_boston, pretty=0)

# cross-validation
cv_boston <- cv.tree(tree_boston)
plot(cv_boston$size,cv_boston$dev,type='b') # most complex tree is best by cv

# if we wanted to prune anyway...
prune_boston <- prune.tree(tree_boston, best=5)
plot(prune_boston)
text(prune_boston,pretty=0)
yhat <- predict(tree_boston, newdata = Boston[-train,])
boston_test <- Boston[-train,"medv"]
plot(yhat,boston_test)
abline(0,1)
mean((yhat - boston_test)^2) # MSE

