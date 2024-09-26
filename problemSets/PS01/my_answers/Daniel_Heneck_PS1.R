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
library(dplyr)
library(ggplot2)

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################
setwd("C:\\Users\\dhene\\Documents\\GitHub\\StatsI_Fall2024\\problemSets\\PS01\\my_answers")

### QUESTION 1.1
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
mean_y <- mean(y) # Point Estimate
std_error <- sd(y)/sqrt(length(y)) # Standard Error

# T-Score
t_score <- qt(0.95, df=length(y)-1)

# Upper Bound
upper_t <- mean(y)+(t_score)*(std_error)
upper_t

# Lower Bound
lower_t <- mean(y)-(t_score)*(std_error)
lower_t

### QUESTION 1.2

# Step 1: Assumptions
class(y)
# Data are quantitative and randomly sampled 
length(y)
# N < 30, which is below the CLT threshold.
# Hence, assume that data are approximately t-distributed 

# Step 2: Hypotheses
# H0: The average student IQ  in the counselor's school is less than or equal to 100
# H1: The average student IQ in the counselor's school is higher than 100

# Step 3: Calculating Test Statistic
t_score <- (mean_y-100)/(std_error)
t_score

# Step 4: Calculate P-value
p_value <- (1-pt(manual_t, length(y)-1))
p_value

# Step 5: Conclusion

##################### 
# Problem 2
#####################

# Load Data 
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

# Summary of Data Set 
summary(expenditure)

# Figure 1.1
png("Figure1.1.png",width = 1600, height = 1000, res = 150)
plot(y = expenditure$Y, x = expenditure$X1,
     ylab = "Per Capita Expenditure on Shelters/Housing Assistance",
     xlab = "Per Capita Income in State",
     main = "Relationship Between per capita Housing Assistance 
     Expenditure and per capita Income in State")
dev.off()
# There is a strong positive relationship between per capita income in state and
# per capita expenditure on shelter/housing assistance in state.
# Higher income per capita per state tends to be associated with higher 
# per capita expenditure on housing assistance and vice versa.

# Figure 1.2
png("Figure1.2.png",width = 1600, height = 1000, res = 150)
plot(y = expenditure$Y, x = expenditure$X2,
     ylab = "Per Capita Expenditure on Shelters/Housing Assistance",
     xlab = "Number of Financially Insecure Residents /100000 in State",
     main = "Relationship between Expenditure on Housing Assistance and 
     Financially Insecure Residents in State")
dev.off()
# There is no linear relationship between the number of financially insecure
# residents per 100000 in state and per capita expenditure on housing assistance.

# Figure 1.3
png("Figure1.3.png",width = 1600, height = 1000, res = 150)
plot(y = expenditure$Y, x = expenditure$X3,
     ylab = "Per Capita Expenditure on Shelters/Housing Assistance",
     xlab = "Number of People /1000 Residing in Urban Areas in State",
     main = "Relationship Between Expenditure on Housing Assistance and
     Number of People in Urban Areas in State")
dev.off()
# There is a positive relationship between the number of people residing in
# urban areas and per capita expenditure on housing assistance.

# Figure 1.4
png("Figure1.4.png", width = 1600, height = 1000, res = 150)
plot(expenditure$X1, expenditure$X2,
     ylab = "Number of Financially Insecure Residents /100000 in State",
     xlab = "Per Capita Income in State",
     main = "Relationship between Per Capita 
     Income and Number of Financially Insecure Residents")
dev.off()
# There is no correlation between per capita income and number of financially 
# insecure residents in state 

# Figure 1.5
png("Figure1.5.png",width = 1600, height = 1000, res = 150)
plot(expenditure$X1, expenditure$X3,
     ylab = "Number of People /1000 Residing in Urban Areas in State",
     xlab = "Per Capita Income in State",
     main = "Relationship between Per Capita Income and Number of People
     Residing in Urban Areas in State")
dev.off()
# There is a weak linear correlation between per capita income and number
# of people per 1000 residing in urban areas. There is one outlier point where a 
# relatively high per capita income state corresponds to a low number of people
# living in urban areas in the state.

# Figure 1.6
png("Figure1.6.png",width = 1600, height = 1000, res = 150)
plot(expenditure$X2, expenditure$X3,
     ylab = "Number of Financially Insecure Residents /100000 in State",
     xlab = "Number of People /1000 Residing in Urban Areas in State",
     main = "Relationship Between Number of People Residing in Urban Areas
     and Number of Financially Insecure People in State")
dev.off()

# There is no correlation between number of people in urban areas and number of 
# financially insecure residents in state.

# Figure 2.1
typeof(expenditure$Region)

expenditure$Region <- factor(expenditure$Region, levels = c(1, 2, 3, 4),
                             labels = c("Northeast", "North Central", "South", "West"))

png("Figure2.1.png",width = 1600, height = 1000, res = 150)                          
plot(y = expenditure$Y, x = expenditure$Region, 
ylab = "Per Capita Expenditure on Shelters/Housing Assistance", xlab = "Reigon",
main = "Expenditure on Shelter by Region")
dev.off()

# On average, the West region has the highest per capita spending on 
# housing assistance. 

# Figure 2.2
png("Figure2.2.png",width = 1600, height = 1000, res = 150)
plot(y = expenditure$Y, x = expenditure$X1, 
     ylab = "Per Capita Expenditure on Shelters/Housing Assistance",
     xlab = "Per Capita Income in State",
     main = "Relationship Between per capita Housing Assistance 
     Expenditure and per capita Income in State")
dev.off()

# There is a positive relationship between Per capita income and 
# expenditure on shelters/housing assistance.

# Figure 2.3
png("Figure2.3.png",width = 1600, height = 1000, res = 150)
plot(y = expenditure$Y, x = expenditure$X1, col = expenditure$Region, 
     pch = as.numeric(expenditure$Region),
     ylab = "Per Capita Expenditure on Shelters/Housing Assistance in State",
     xlab = "Per Capita Income in State",
     main = "Relationship Between Expenditure on Shelters/Housing 
     Assistance and Personal Income by Reigon")
legend(1000,130,
       legend=c("Northeast", "North Central", "West", "South"),
       col=c("black","red", "blue", "green"),
        pch=1:4) 
dev.off()
