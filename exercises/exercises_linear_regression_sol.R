library(dplyr)
library(ggplot2)
library(readr)
library(GGally)


#######################################################################
###                      advertising data                           ###
#######################################################################

# sales is in thousands of units
# TV, radio and newspaper are in thousands of dollars
advertising <- read_csv("data/Advertising.csv", col_types = "_dddd")
advertising
summary(advertising)

cor(advertising)
ggpairs(advertising)

# Questions
# 1
# Which predictor do you think has most influence on sales - where would you spend the money?
lm(sales ~ ., data = advertising) %>% summary()

# For that channel, is the relationship linear? How good is a linear fit?
# 2
lm(sales ~ TV, data = advertising) %>% summary()    # TV has highest variance explained
lm(sales ~ radio, data = advertising) %>% summary() # radio has the steepest slope

# Is there a better fit for that variable?
# 3
lm(sales ~ I(log(TV)), data = advertising) %>% summary()
lm(sales ~ I(log(ifelse(radio >0, radio, 0.0000001))), data = advertising) %>% summary()

lm(sales ~ poly(TV,3), data = advertising) %>% summary()
lm(sales ~ poly(radio,3), data = advertising) %>% summary()

# 4
# Assuming you did multiple linear regression for the first question,
# compare to what you get when you regress sales on each of the predictors separately.
# What happens? Why?
lm(sales ~ newspaper, data = advertising) %>% summary()
cor(advertising$newspaper, advertising$radio)
# holding radio fixed, there is no additional effect of newspaper

#5
# Are the effects of TV advertising and radio advertising independent/additive 
# or is there some kind of synergy?
lm(sales ~ TV*radio, data = advertising) %>% summary()

# 6
# Predict expected sales for a single campaign when budgets for the three channels
# are at their median values (from the given data).
# Don't forget to account for uncertainty.
fit <- lm(sales ~ ., data = advertising)
fit %>% predict(newdata = data.frame(TV = median(advertising$TV),
                                     radio = median(advertising$radio),
                                     newspaper = median(advertising$newspaper)),
                interval = "prediction")




#######################################################################
###                      income data                                ###
#######################################################################

income <- read_csv("data/Income2.csv", col_types = "_ddd")
income

# Questions
# 1
# Inspect the data and formulate a hypothesis about what leads to a higher income.

cor(income)
ggpairs(income)

# 2
# Test your hypothesis (using regression)

linear_fit <- lm(Income ~ ., data = income)
summary(linear_fit)

cubic_fit <- lm(Income ~ Education + I(Education^2) + I(Education^3) + Seniority, data = income)
summary(cubic_fit)

# same adj. R^2, very different coefficients
poly_fit <- lm(Income ~ poly(Education,3) + Seniority, data = income)
summary(poly_fit)

# trying some higher-degree polynomials
# of course we would need a bigger dataset to split in train and test here...
poly_fit <- lm(Income ~ poly(Education,4) + Seniority, data = income)
summary(poly_fit)

poly_fit <- lm(Income ~ poly(Education,5) + Seniority, data = income)
summary(poly_fit)

# this would be nice but doesn't work so well
sigmoid <- function(x) 1/(1 + exp(-x))
sigmoid_fit <- lm(Income ~ Education + sigmoid(Education) + Seniority, data = income)
summary(sigmoid_fit)
