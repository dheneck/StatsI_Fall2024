#PS03

#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd("C:\\Users\\dhene\\Documents\\GitHub\\StatsI_Fall2024\\problemSets\\PS03\\my_answers")

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

################
# Question 1
################
# Read in and inspect data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")
summary(inc.sub)

install.packages("stargazer")
library(stargazer)

# 1.1
# Regression Model
model1 <- lm(voteshare~difflog, data = inc.sub)
stargazer(model1, type = "latex")

# 1.2
# Scatter Plot
png("Plot1.png",width = 1600, height = 1000, res = 150)
plot(y=inc.sub$voteshare, x=inc.sub$difflog,
     main = "Relationship Between Spending 
     Difference and Incumbent Vote Share",
     ylab = "Incumbent Vote Share (%)",
     xlab = "Difference in Campaign Spending between 
     Incumbent and Challenger")
abline(model1, col = "red")
dev.off()

# 1.3
# Save residuals into new variable
residuals_model1 <- model1$residuals 
residuals_model1

# 1.4
# Prediction Equation 
print("voteshare = 0.579 + 0.041difflog + residuals")

#################
# Question 2
#################

# 2.1
model2 <- lm(presvote~difflog, data = inc.sub)
stargazer(model2, type = "latex")

# 2.2
png("Plot2.png",width = 1600, height = 1000, res = 150)
plot(y = inc.sub$presvote, x = inc.sub$difflog,
     main = "Relationship Between Spending 
Difference and Vote Share",
     ylab = "Vote Share of Incumbent's Party Candidate (%)",
     xlab = "Difference in Campaign Spending between 
     Incumbent and Challenger")
abline(model2, col = "red")
dev.off()

# 2.3
# Save Residuals in Object Called residuals_model2
residuals_model2 <- model2$residuals

# 2.4
print('presvote = 0.507 + 0.023difflog + residuals')

#################
# Question 3
#################

# 3.1
model3 <- lm(voteshare~presvote, data = inc.sub)
stargazer(model3, type = "latex")

# 3.2
png("Plot3.png",width = 1600, height = 1000, res = 150)
plot(y = inc.sub$voteshare, x = inc.sub$presvote,
     main = "Association Between Incumbent's 
Electoral Success and Vote Share",
     ylab = "Incumbent Vote Share (%)",
     xlab = "Vote Share of Incumbent's Party Candidate (%)")
abline(model3, col = "red")
dev.off()

# 3.3
print("voteshare = 0.441 + 0.388presvote + residuals")

#################
# Question 4
#################

# 4.1
resid_regr <- lm(residuals_model1 ~ residuals_model2)
stargazer(resid_regr, type = "latex")

# 4.2 
png("Plot4.png",width = 1600, height = 1000, res = 150)
plot(y = residuals_model1, x = residuals_model2,
     main = "Relationship Between Model Residuals",
     ylab = "Residuals from Model 1",
     xlab = "Residuals from Model 2")
abline(resid_regr, col = "red")
dev.off()


# 4.3
print("residuals model 1 = -0.00 + 0.256x + residuals")

#################
# Question 5
#################

# 5.1
mlr <- lm(voteshare~difflog + presvote, data = inc.sub)
stargazer(mlr, type = "latex")

# 5.2
print("voteshare = 0.448 + 0.035difflog + 0.256presvote + residuals")
