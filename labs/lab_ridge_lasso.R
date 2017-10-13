library(ISLR)
library(glmnet)

#######################################################################
###                     Ridge Regression                            ###
###                         Hitters                                 ###
#######################################################################

data("Hitters")
str(Hitters)
Hitters <- na.omit(Hitters)

X <- model.matrix(Salary ~ ., Hitters)[ ,-1]
X
dim(X)
y <- Hitters$Salary

# search grid for the regularization parameter
grid <- 10^seq(10,-2,length=100)
grid

?glmnet
# alpha = the elasticnet mixing parameter, with 0 ≤ α ≤ 1.
# The penalty is defined as (1 - α)/2 * ||β||_2^2 + α * ||β||_1
# alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
ridge_mod <- glmnet(X, y, alpha=0, lambda=grid)
# ridge_mod
dim(coef(ridge_mod)) # 100 vectors of ridge coefficients, one for each lambda

# compare size of the coefficients at different lambdas
# the median lambda
ridge_mod$lambda[50]
coef(ridge_mod)[,50]
sqrt(sum(coef(ridge_mod)[-1,50]^2)) # the l2 norm

# a smaller lambda
ridge_mod$lambda[60]
coef(ridge_mod)[,60]
sqrt(sum(coef(ridge_mod)[-1,60]^2)) # much smaller l2 norm!


# split into test and training set to estimate test error of ridge regression
set.seed(1)
train <- sample(1:nrow(X), nrow(X)/2)
test <- (-train)
y_test <- y[test]

# fit on training set
ridge_mod <- glmnet(X[train, ], y[train], alpha = 0,lambda = grid, thresh = 1e-12)

# get MSE on test set for lambda = 4
ridge_pred <- predict(ridge_mod, s = 4, newx = X[test, ])
mean((ridge_pred - y_test)^2)
# get MSE on test set for lambda = 10^10 - this is the same as just fitting an intercept
ridge_pred <- predict(ridge_mod, s = 1e10, newx=X[test,])
mean((ridge_pred-y_test)^2)
# the least squares fit
ridge_pred <- predict(ridge_mod, s = 0, newx=X[test,], exact=T, x = X[train, ], y = y[train] )
mean((ridge_pred - y_test)^2)
# same coefficients as from lm
lm(y ~ X, subset=train) 
predict(ridge_mod, s = 0, exact = T, type="coefficients", x = X[train, ], y = y[train])[1:20,] # use predict to get coefficients for a new lambda


# nobody wants to manually compare MSEs
# use cross-validation for this!

set.seed(1)
cv_out <- cv.glmnet(X[train,], y[train], alpha=0)
plot(cv_out)

bestlambda <- cv_out$lambda.min
bestlambda

# get predictions for best lambda
ridge_pred <- predict(ridge_mod, s = bestlambda, newx = X[test,])
# and MSE
mean((ridge_pred - y_test)^2)

# now re-fit on whole dataset
out <- glmnet(X, y, alpha=0)
# final coefficients at best lambda
predict(out, type = "coefficients", s = bestlambda)[1:20,]

# none of the coefficients is 0
# ridge is not for variable selection!
plot(ridge_mod)


#######################################################################
###                          Lasso                                  ###
###                         Hitters                                 ###
#######################################################################

# set alpha=1 for lasso
lasso_mod <- glmnet(X[train,], y[train], alpha = 1, lambda = grid)
# inspect coefficient path
plot(lasso_mod)
plot(lasso_mod, xvar = "lambda")

# find best lambda by cross-validation
set.seed(1)
cv_out <- cv.glmnet(X[train,],y[train], alpha = 1)
plot(cv_out)
bestlambda <- cv_out$lambda.min
lasso_pred <- predict(lasso_mod, s = bestlambda, newx=X[test,])
mean((lasso_pred - y_test)^2) # much better than least squares

# final model on whole set
out <- glmnet(X, y, alpha = 1, lambda = grid)
# get best coefficients
lasso_coef <- predict(out, type ="coefficients",s = bestlambda)[1:20,]
lasso_coef
lasso_coef[lasso_coef!=0]

# now we have just 7 non-zero coefficients!


