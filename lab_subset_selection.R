library(ISLR)
library(leaps)

#######################################################################
###                     Subset selection                            ###
###                         Hitters                                 ###
#######################################################################

data(Hitters)
?Hitters
names(Hitters)
dim(Hitters)

# remove when missing salary data
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

?leaps
# Since the algorithm returns a best model of each size,
# the results do not depend on a penalty model for model size:
# it doesn't make any difference whether you want to use AIC, BIC, CIC, DIC, ...

regfit_full <- regsubsets(Salary ~ ., Hitters) # up to 8 predictors included by default
summary(regfit_full)

regfit_full <- regsubsets(Salary ~ ., data = Hitters, nvmax=19) # all 19 model sizes
reg_summary <- summary(regfit_full)
names(reg_summary)
reg_summary$rsq # R^2 increases monotonically as more variables are included

# now we have best model per model size
# which is the overall best model?
# below, we will use cross validation for this, here we use statistics and information criteria
par(mfrow=c(2,2))
plot(reg_summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg_summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg_summary$adjr2)
points(11,reg_summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg_summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg_summary$cp)
points(10,reg_summary$cp[10],col="red",cex=2,pch=20)
which.min(reg_summary$bic)
plot(reg_summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg_summary$bic[6],col="red",cex=2,pch=20)

# which variables are included in the different fits?
# top row is best
plot(regfit_full,scale="r2")
plot(regfit_full,scale="adjr2")
plot(regfit_full,scale="Cp")
plot(regfit_full,scale="bic")

# get included variable names
coef(regfit_full,6)


# Forward and Backward Stepwise Selection

regfit_fwd <- regsubsets(Salary ~ ., data=Hitters, nvmax=19, method="forward")
summary(regfit_fwd)

regfit_bwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
summary(regfit_bwd)

# compare
coef(regfit_full,7)
coef(regfit_fwd,7)
coef(regfit_bwd,7)


# Choosing Among Models of different sizes
# using cross-validation
set.seed(1)

# demonstration using validation set (cross-validation follows)
train <- sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
validation <- (!train)
# imagine we had a test set, too

# compute best model per size on training set
regfit_best <- regsubsets(Salary~.,data=Hitters[train,], nvmax=19) 

# X matrix for the validation data
val_mat <- model.matrix(Salary~.,data=Hitters[validation,])
val_mat

val_errors <-rep(NA,19)
for(i in 1:19){
   coefi <-coef(regfit_best,id=i)
   print(coefi)
   print("#########################################")
   pred <- val_mat[ ,names(coefi)] %*% coefi #get the predictions
   val_errors[i] <- mean((Hitters$Salary[validation] - pred)^2)
}
val_errors
which.min(val_errors)
coef(regfit_best,10) # best on validation set

######

# to ease things, a predict function for regsubsets
predict.regsubsets <- function(object,newdata,id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars] %*% coefi
}

regfit.best <- regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit_best,10)
k <- 10
set.seed(1)
folds <- sample(1:k,nrow(Hitters),replace=TRUE)
cv_errors <- matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best_fit <- regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred <- predict(best.fit,Hitters[folds==j,],id=i)
    cv_errors[j,i] <- mean( (Hitters$Salary[folds==j]-pred)^2)
    }
  }
mean_cv_errors <- apply(cv.errors,2,mean)
mean_cv_errors
par(mfrow=c(1,1))
plot(mean_cv_errors,type='b')
reg.best <- regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)


