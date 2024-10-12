#########
# PS02
#########
install.packages("stargazer")
library(stargazer)
getwd()
setwd("C:\\Users\\dhene\\Documents\\GitHub\\StatsI_Fall2024\\problemSets\\PS02\\my_answers")

# Question 1a
# Expected = Row total*Column Total/Grand Total

row1_total <- 14 + 6 + 7
row2_total <- 7 + 7 + 1
column1_total <- 14 + 7
column2_total <- 6 + 7
column3_total <- 7 + 1
grand_total <- row1_total + row2_total 

fe1 <- (row1_total/grand_total)*column1_total
fe2 <- (row1_total/grand_total)*column2_total
fe3 <- (row1_total/grand_total)*column3_total
fe4 <- (row2_total/grand_total)*column1_total
fe5 <- (row2_total/grand_total)*column2_total
fe6 <- (row2_total/grand_total)*column3_total

# Chi^2 Stat = (fo - fe)^2/fe

Chi2 <- (14-fe1)^2/fe1 + (6-fe2)^2/fe2 + (7-fe3)^2/fe3 + (7-fe4)^2/fe4 + 
  (7-fe5)^2/fe5 + (1-fe6)^2/fe6 

Chi2

# Question 1b
# Number of rows = 2
# Number of columns = 3
# DF = (2-1)(3-1) = 2

pval_chi <- pchisq(Chi2, df = 2, lower.tail = FALSE)
pval_chi

# Since p-value > 0.1, there is not sufficient evidence to reject the null hypothesis
# that officers soliciting a bribe from drivers is statistically independent 
# of class when a = 0.1. We therefore conclude the two variables are 
# statistically independent of each other.

# Question 1c
# Calculate the standardised residuals for every cell in the table using the 
# given formula

# Standardised Residuals
c11 <- (14-fe1)/sqrt(fe1)  
c12 <- (6-fe2)/sqrt(fe2) 
c13 <- (7-fe3)/sqrt(fe3) 
c14 <- (7-fe4)/sqrt(fe4)
c15 <- (7-fe5)/sqrt(fe5)
c16 <- (1-fe6)/sqrt(fe6) 

# Question 1d
# Cell-by-cell comparison of the residuals helps describe the pattern of 
# association among the cells. A cell with a large standardized residual
# provides evidence against independence in that cell. When H0 is true,
# there is approximately a 5% chance that any cell has a standardized residual
# value that exceeds 2.
# In the table above, all of the standardized residuals are below 2 in absolute 
# value. This indicates that there is not strong evidence of dependence 
# in any of the cells - further backing up the chi-squared test results 
# which indicated the two variables are statistically independent.


# Question 2
# Load in and Inspect the Data
df <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
summary(df)


# Question 2a

#H0: The reservation policy has had no effect on the number of new or repaired
# drinking water facilities in the villages

#H1: The reservation policy has had an effect on the number of new or repaired
# drinking water facilities in the villages

# Question 2b
# Linear Regression Model 
model1 <- lm(df$water ~ df$reserved)
summary(model1)
stargazer(model1, type = "latex")
