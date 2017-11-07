
library(ISLR)
library(GGally)
library(dplyr)

#######################################################################
###                     logistic regression                         ###
###                    stock market dataset                         ###
#######################################################################

?Smarket
names(Smarket)
summary(Smarket)

# is there a connection between today's returns (binarized: direction up/down of the market)
# and previous returns (lag1 - lag5)?
ggpairs(Smarket) 
cor(Smarket[,-9]) # look at correlations with "today"

# Logistic Regression
fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket,family = binomial)
summary(fit)
coef(fit)

# this would give predictions (on the same set)
probs <- predict(fit,type="response")
probs[1:10] # this is P(Up = 1|Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume)
with(Smarket, contrasts(Direction)) # Up was coded as 1 internally

# convert probabilities to class labels
preds <- rep("Down", 1250)
preds[probs > .5] <- "Up"

# confusion matrix and accuracy
table(preds = preds, actual = Smarket$Direction)
accuracy <- mean(preds==Smarket$Direction) # a great result but this was obtained on the training set!!

# do it right: split into training and test sets
train <- Smarket[Smarket$Year < 2005, ]
test <- Smarket[!Smarket$Year < 2005, ]
dim(train)
dim(test)

fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = train,family = binomial)
probs <- predict(fit, test, type="response")
preds <- rep("Down", nrow(test))
preds[probs > .5] <- "Up"
table(preds = preds, actual = test$Direction)
mean(preds == test$Direction) # accuracy
mean(preds != test$Direction) # error rate

# try using just the best 2 predictors
fit <- glm(Direction ~ Lag1 + Lag2, data = train,family = binomial)
probs <- predict(fit, test,type="response")
preds <- rep("Down", nrow(test))
preds[probs > .5] <- "Up"
table(preds = preds, actual = test$Direction)
mean(preds == test$Direction)
predict(fit, newdata = data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

# need more success? try QDA (quadratic discriminant analysis) from the book...



#######################################################################
###                     logistic regression                         ###
###                    caravan insurance data                       ###
#######################################################################
data(Caravan)
?Caravan
dim(Caravan)
summary(Caravan$Purchase)


# train-test split
X_train <- Caravan[1001:nrow(Caravan), ]
X_test <- Caravan[1:1000, ]
y_train <- Caravan$Purchase[1001:nrow(Caravan)]
y_test <- Caravan$Purchase[1:1000]

fit <- glm(Purchase ~ ., data = X_train, family = binomial)
summary(fit)
probs <- predict(fit, X_test, type="response")
preds <- rep("No",1000)
preds[probs > .5] <- "Yes" # threshold at 0.5 isn't of much use
(t <- table(preds, y_test))
(precision <- t[2,2]/(t[2,1] + t[2,2]))

preds <- rep("No", 1000)
preds[probs > .25] <- "Yes" # move threshold to 0.25
(t <- table(preds, y_test))
(precision <- t[2,2]/(t[2,1] + t[2,2])) # much more useful
