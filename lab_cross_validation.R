library(ISLR)
library(boot)

#######################################################################
###                     Cross validation                            ###
###                    stock market dataset                         ###
#######################################################################

# The Validation Set Approach

data("Auto")
?Auto

# Leave-One-Out Cross-Validation

fit <- glm(mpg ~ horsepower, data = Auto)
coef(fit)

?cv.glm # needs results of a fit
# cv.glm(data, glmfit, cost, K)
# default for cost is MSE, default for k = n (LOOVC)
fit <- glm(mpg ~ horsepower, data = Auto)
fit
cv_err <- cv.glm(Auto, fit)
cv_err$delta # raw and adjusted estimates of prediction error (MSE on test)

# test different polynomial fits
cv_error <- rep(0,5)
for (i in 1:5){
 fit <- glm(mpg ~ poly(horsepower, i), data=Auto)
 cv_error[i] <- cv.glm(Auto, fit)$delta[1]
 }
cv_error


# k-Fold Cross-Validation

set.seed(17)
cv_error_10 <- rep(0,10)
for (i in 1:10){
 fit <- glm(mpg ~ poly(horsepower,i), data = Auto)
 cv_error_10[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
 }
cv_error_10

