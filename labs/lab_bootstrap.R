library(ISLR)
library(boot)

#######################################################################
###                     Bootstrap                                   ###
###                    Portfolio                                    ###
#######################################################################

data("Portfolio")
?Portfolio
Portfolio

alpha_fn <- function(data, index) {
 X <- data$X[index]
 Y <- data$Y[index]
 return ((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2 * cov(X,Y)))
}

alpha_fn(Portfolio,1:100)

set.seed(1)
alpha_fn(Portfolio, sample(100,100,replace = TRUE))

?boot
boot <- boot(Portfolio, alpha_fn,R = 1000)
boot
plot(boot)



#######################################################################
###                     Bootstrap                                   ###
###                    Linear Regression                            ###
#######################################################################

data(Auto)
boot_fn <- function(data,index) # get intercept and slope for simple linear regression
 return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
boot_fn(Auto, 1:392)

set.seed(1)
boot_fn(Auto, sample(392,392,replace=T))
boot_fn(Auto, sample(392,392,replace=T))
boot(Auto, boot_fn,1000) # standard errors for intercept and slope

summary(lm(mpg ~ horsepower, data = Auto))$coef # compare to lm

# same comparison for polynomial linear regression
boot_fn <- function(data,index)
 coefficients(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset= index))
set.seed(1)
boot(Auto,boot_fn,1000)
summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef

