install.packages("car")
library(car)
data(Prestige)
help(Prestige)

################
# Question 1
################

## 1a
Prestige$professional <- as.factor(ifelse(is.na(Prestige$type), NA, ifelse(Prestige$type == "prof", 1, 0)))
table(Prestige$professional)


## 1b
model1 <- lm(prestige ~ income*professional, data = Prestige)
library(stargazer)  
stargazer(model1)

## 1c
# prestige = 21.142 + 0.003income + 37.781professional - 0.002income*professional

## 1d
# A one unit increase in income is associated with a 0.003 unit increase in prestige when
# professional is equal to 0 - i.e. when the individual is not a professional (is either a blue or 
# whit collar worker).

## 1e
# On average, moving from the baseline group (blue and white collar work) to a professional level
# is associated with a 37.78 unit increase in prestige score when income is equal to 0.

## 1f
inc_1000 <- 21.142+(0.003*1000)+37.781-(0.002*1000)
inc_0 <- 21.142+(0.003*0)+37.781-(0.002*0)
# Prestige level increases by 1 unit when income increases from 0 to 1000 units 
# for professionals 

## 1g
prof <- 21.142+(0.003*6000)+37.78-(0.002*6000)
non_prof <- 21.142+(0.003*6000)+37.78*0-(0)
prof - non_prof

# On average, professionals with an income of 6000 dollars have a prestige level
# of 25.78 units higher than non-professionals that earn 6000 dollars. Hence the 
# marginal effect of professional jobs when income is 6000 dollars is 25.78
# prestige points.

##############
# Question 2
##############

#2a

# Step 1: Assumptions
# Quantitative data
# Data randomly sampled
# Errors are normally distributed with constant variance and 0 mean 
# There is a linear relationship between the independent and dependent variables 

#Step 2: Hypotheses 
# H0: The yard signs have no effect on vote share 
# H1: The yard signs have an effect on vote share 

# Step 3: Test Statistic 
t_stat <- 0.042/0.016
# t-stat = 2.625

# Step 4: P-value
df <- 131-2-1
p_value <- 2 * (1 - pt(abs(t_stat), df))
p_value

# Conclusion 
# P-value is less than 0.05, therefore we can 
# reject the null hypothesis that the having
# the yard signs in a precinct doesn't affect vote
# share. There is evidence to suggest that there 
# is a statistically reliable positive relationship
# between the number of yard signs in a precinct 
# and the percentage of the vote that went to
# Ken Cuccinelli.

# 2b

# Step 1: Assumptions
# Same as previous

# Step 2: Hypotheses 
# H0: Being next to precincts with yard signs has no effect on vote share
# H1: Being next to precincts with yard signs has an effect on vote share

# Step 3: Test Statistic 
# t-stat = 3.23
t_stat2 <- 0.042/0.013

# Step 4: P-value 
df2 <- 131-2-1
p_val2 <- 2 * (1 - pt(abs(t_stat2), df2))

# Conclusion 
# The p-value is less than 0.05, therefore we can 
# reject the null hypothesis that being next to 
# precincts that have the yard sign does not affect vote share.
# The result indicates a statstically reliable positive relationship
# between being in the precinct next to yard signs and vote share.

# 2c
# The constant represents the proportion of the vote that went to Ken Cuccinelli 
# when the precinct was assigned no signs against Mcauliffe and the precinct 
# adjacent was exposed to no signs against Mcauliffe.
# Cucinelli's predicted vote share is 30.2% when no precint is assigned the 
# signs against Mcauliffe and the precinct 
# adjacent is exposed to no signs against Mcauliffe.

# 2d
# The R-squared value indicates that 9.4% of the variation in vote share can
# be explained by the variation in the precinct assigned lawn signs and the 
# precinct adjacent to lawn signs. This means that a lot of variation in
# vote share is not captured by the model. The model could therefore help 
# predict vote share more accurately if other independent variables that 
# are associated with vote share were included.