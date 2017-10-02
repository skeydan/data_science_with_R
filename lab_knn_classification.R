
library(ISLR)
library(GGally)
library(class)

#######################################################################
###                     KNN classification                          ###
###                    stock market dataset                         ###
#######################################################################

train <- Smarket[Smarket$Year < 2005, ]
test <- Smarket[!Smarket$Year < 2005, ]

?knn

# use just Lag2 and Lag1 which we know work best
X_train <- train[ , c("Lag1", "Lag2")]
X_test <- test[ , c("Lag1", "Lag2")]
y_train <- train[ , "Direction"]
y_test <- test[ , "Direction"]

set.seed(1)
# note: there are no separate training and test phases!!
# 1 nearest neighbor
preds <- knn(X_train, X_test, y_train, k=1)
table(preds, y_test)
mean(preds == y_test)

# 5 nearest neighbors
preds <- knn(X_train, X_test, y_train, k=3)
table(preds, y_test)
mean(preds == y_test)

# 10 nearest neighbors
preds <- knn(X_train, X_test, y_train, k=10)
table(preds, y_test)
mean(preds == y_test)


#######################################################################
###                     KNN classification                          ###
###                    caravan insurance data                       ###
#######################################################################

?Caravan
dim(Caravan)
summary(Caravan$Purchase)

# standardize the data
# necessary for distance-based algorithms like knn
X_std <- scale(Caravan[,-86]) # all but the result variable
var(Caravan[,1])
var(Caravan[,2])
var(X_std[,1])
var(X_std[,2])

# train-test split
X_train <- X_std[1001:nrow(X_std), ]
X_test <- X_std[1:1000, ]
y_train <- Caravan$Purchase[1001:nrow(Caravan)]
y_test <- Caravan$Purchase[1:1000]

set.seed(1)
preds <- knn(X_train, X_test, y_train, k = 1) 
mean(y_test != preds) # error rate
mean(y_test != "No") # this would be the error rate if we always predicted no  (majority vote classifier)
table(preds, y_test)

# try 3nn
preds <- knn(X_train, X_test, y_train, k = 3) 
mean(y_test != preds) 
table(preds, y_test)

# try 5nn
preds <- knn(X_train, X_test, y_train, k = 5) 
mean(y_test != preds) 
table(preds, y_test)

# Is accuracy/error rate the right measure?
# say there is significant cost involved in trying to sell insurance
# then a false positive (predict yes but person doesn't buy) is a problem
# while a false negative (predict no but person would have bought insurance) is not
# then we don't care so much about error rate
# but want to increase PRECISION (fraction of predictions that are actually correct)
set.seed(1)
preds <- knn(X_train, X_test, y_train, k = 1) 
(t1 <- table(preds, y_test))
t1[2,2]/(t1[2,1] + t1[2,2])

preds <- knn(X_train, X_test, y_train, k = 3) 
(t1 <- table(preds, y_test))
t1[2,2]/(t1[2,1] + t1[2,2])

preds <- knn(X_train, X_test, y_train, k = 5) 
(t1 <- table(preds, y_test))
t1[2,2]/(t1[2,1] + t1[2,2]) # !!!