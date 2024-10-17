#### Econometrics 1 - TD4 ####

# Install packages
if(!require(haven)) install.packages("haven")  
if(!require(car)) install.packages("car")
if(!require(estimatr)) install.packages("estimatr")  
if(!require(lmtest)) install.packages("lmtest")

#Load packages
library(haven)
library(car)
library(estimatr)
library(lmtest)

# Set directory
setwd("Your path")  

#####################   
#     Exercise 1    #
#####################

rm(list=ls()) #Clean workspace
data <- read_dta("Tennis-Sleep.dta")

####################
#### Question 1 ####
####################

# Q1 - Compute descriptive statistics
summary(data)


####################
#### Question 2 ####
####################

# Q2 - Run the linear regression of DifgamesP1P2 on SleepP1
reg1 = lm_robust(DifgamesP1P2~SleepP1, data = data, se_type = "stata") # Runs OLS with errors robust to heteroskedasticity
summary(reg1)


####################
#### Question 4 ####
####################

# Q4 - Run the linear regression of DifgamesP1P2 on SleepP1 and AlcoholP1
reg2 = lm_robust(DifgamesP1P2~SleepP1 + AlcoholP1, data = data, se_type = "stata")
summary(reg2)

# Let us compute the terms of the omitted variable bias formula: 
lambda = lm(AlcoholP1~SleepP1, data = data)$coefficients[2] # <0
total_bias = lambda * reg2$coefficients[3]                  # <0 (Counter intuitive)
# We find the long regression coefficient below:
reg1$coefficients[2] - total_bias


####################
#### Question 6 ####
####################

# Q6 - Run the new regression including AlcoholP2 and SleepP2

# Display previous regression coefficients to compare them:
summary(reg2)
# Actual "long" regression:
reg3 = lm_robust(DifgamesP1P2~SleepP1 + AlcoholP1 + SleepP2 + AlcoholP2, data = data, se_type = "stata")
summary(reg3)


####################
#### Question 7 ####
####################

# Q7 - Test the equality of the effects of SleepP1 and SleepP2 on the outcome DifgamesP1P2

# Let us build the linear combination matrices R and b (see Chapter 2 slide 29)
R = c(0,1,0,1,0) # Line matrix to test: Beta_1 + Beta_3
b = 0            #                      = 0
# We run a F - test:
test3 = lht(reg3, hypothesis.matrix = R, rhs = b, test ='F')[2,3:4]
test3



#####################################
#### Question 7 - Another method ####
#####################################

# Q7 - Another method:
data$DifsleepP1P2 = data$SleepP1-data$SleepP2
reg3.2 = lm_robust(DifgamesP1P2~DifsleepP1P2 +  AlcoholP1 + SleepP1 + AlcoholP2, data= data, se_type = 'stata')
# We can remark that all other coefficients are identical
reg3.2
# We run a F - test:
test3.2 = lht(reg3.2, c("SleepP1 = 0"), test ='F')[2,3:4]
test3.2

#Other command for simple tests 
coeftest(reg3.2,df=Inf)

#####################   
#     Exercise 2    #
#####################

rm(list=ls()) #Clean workspace
data <- read_dta("hprice1.dta")


####################
#### Question 0 ####
####################

# Some descriptive statistics
summary(data)


####################
#### Question 2 ####
####################

reg1 = lm_robust(price ~ assess, data = data , se_type = "stata")
reg1

# Perform a joint test: Beta_0 = 0 and Beta_1 = 1:
R = diag(2)
b = c(0,1)
lht(reg1, hypothesis.matrix = R, rhs = b, test = "F")[2,]


# => We do reject the hypothesis at levels 1%,5%,10 % > 2.985e-05

# Test that the coefficient of the constant is 0:
R = c(1,0)
b = 0
lht(reg1, hypothesis.matrix = R, rhs = b, test = "F")[2,]

# And that the coefficient of the slope is 1:
R = c(0,1) 
b = 1 
lht(reg1, hypothesis.matrix = R, rhs = b, test = "F")[2,]

####################
#### Question 3 ####
####################

# Regress lprice on sqrft and bdrms:
reg2 = lm_robust(lprice ~sqrft + bdrms, data=data, se_type = "stata")
reg2
# Effects of exogeneous changes:
as.double(c(0,150,1) %*% reg2$coefficients) # Remember the model is log - level.


####################
#### Question 4 ####
####################

# Another method to get a confidence interval on the effect directly:
# 4.1 - Create the new variable:
data$sqrft150b =   data$sqrft - 150 * data$bdrms
# 4.2 - Run the regression:
reg3 = lm_robust(lprice~sqrft150b + bdrms, data = data, se_type = "stata")
reg3
# 4.3 - Get the coefficient and its interval on the variable bdrms:
reg3$coefficients[3]
# The confidence interval is:
c('low_bound' = reg3$conf.low[3], 'high_bound' = reg3$conf.high[3])



