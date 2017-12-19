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


# For that channel, is the relationship linear? How good is a linear fit?
# 2


# Is there a better fit for that variable?
# 3


# 4
# Assuming you did multiple linear regression for the first question,
# compare to what you get when you regress sales on each of the predictors separately.
# What happens? Why?


#5
# Are the effects of TV advertising and radio advertising independent/additive 
# or is there some kind of synergy?

# 6
# Predict expected sales for a single campaign when budgets for the three channels
# are at their median values (from the given data).
# Don't forget to account for uncertainty.




#######################################################################
###                      income data                                ###
#######################################################################

income <- read_csv("data/Income2.csv", col_types = "_ddd")
income

# Questions
# 1
# Inspect the data and formulate a hypothesis about what leads to a higher income.



# 2
# Test your hypothesis (using regression)



