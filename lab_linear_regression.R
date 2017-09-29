# Lab: Linear Regression

library(MASS)
library(ISLR)
library(ggplot2)
library(dplyr)

#######################################################################
###                   simple linear regression                      ###
###                    Boston dataset                               ###
#######################################################################

data(Boston)
?Boston

fit <- lm(medv ~ lstat, data = Boston)
fit
summary(fit)
confint(fit) # confidence intervals for the coefficients

predict(fit, newdata = data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(fit, newdata = data.frame(lstat=(c(5,10,15))), interval="prediction")

# base R plots
plot(Boston$lstat,Boston$medv)
abline(fit, lwd = 3, col = "red")

# ggplot2
ggplot(data = Boston, aes(x = lstat, y = medv)) + geom_point() + 
  geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2], color = "blue")

# predictions: confidence interval
newdata <- data.frame(lstat = seq(0, 50, by=.1))
preds <- fit %>% predict(newdata = newdata, interval = "confidence")
newdata <- newdata %>% bind_cols(pred = preds[ ,1])
ggplot(newdata, aes(x = lstat, y=pred)) + geom_line() + 
  geom_ribbon(aes(ymin = preds[ , 2], ymax = preds[ , 3]), alpha = 0.2)

# predictions: prediction interval
newdata <- data.frame(lstat = seq(0, 50, by=.1))
preds <- fit %>% predict(newdata = newdata, interval = "prediction")
newdata <- newdata %>% bind_cols(pred = preds[ ,1])
ggplot(newdata, aes(x = lstat, y=pred)) + geom_line() + 
  geom_ribbon(aes(ymin = preds[ , 2], ymax = preds[ , 3]), alpha = 0.2)


#######################################################################
###                 multiple linear regression                      ###
###                    Boston dataset                               ###
#######################################################################

fit <- lm(medv ~ lstat + age, data = Boston)
summary(fit)

fit <- lm(medv ~ .,data=Boston)
summary(fit)
summary(fit)$r.squared
summary(fit)$adj.r.squared

fit # exclude one predictor
fit <- lm (medv ~ . - age, data = Boston)
summary(fit)
summary(fit)$r.squared
summary(fit)$adj.r.squared

# interactions
summary(lm(medv ~ lstat * rm, data=Boston))


#######################################################################
###              non-linear transformations of the predictors       ###
###                    Boston dataset                               ###
#######################################################################

# quadratic term
fit <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(fit)

# compare simple and quadratic
simple_fit=lm(medv ~ lstat, data = Boston)
quadratic_fit <- lm(medv ~ lstat + I(lstat^2), data = Boston)

anova(simple_fit, quadratic_fit)

# polynomials
deg_5_fit <- lm(medv ~ poly(lstat,5), data = Boston)
summary(deg_5_fit)

deg_6_fit <- lm(medv ~ poly(lstat,6), data = Boston)
summary(deg_6_fit)

# log transformation
summary(lm(medv ~ log(rm), data = Boston))


#######################################################################
###                  Qualitative Predictors                         ###
###                    Carseats dataset                             ###
#######################################################################

data(Carseats)
?Carseats
str(Carseats)

fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data=Carseats)
summary(fit)
contrasts(Carseats$ShelveLoc)
