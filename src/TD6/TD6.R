# ----------------------- Econometrics 1 - TD 6 --------------------- #


# load necessary libraries
library(tidyverse)

# clean the environment
rm(list=ls())

# choose a seed to be able to reproduce the results
set.seed(2024)

# set the number of observations at 10,000
n <- 10000

# create an empty dataset to store the observations
data <- data.frame(i = 1:n)


# Q6. Simulations, delta value (ATE) ----------------------------------

## Q6.1. Simulation of potential outcomes ----

# Y(0) and Y(1) follow a univariate normal distribution

# Y(0) = outcome if not treated - N(0,1)
data$Y_0 <- rnorm(n, mean = 0, sd = 1)
# Y(1) = outcome if treated - N(1,1)
data$Y_1 <- rnorm(n, mean = 1, sd = 1)

## Q6.2. Average Treatment Effect (ATE) ----

# Delta = Y(1) - Y(0)
data <- data %>% mutate(Delta = Y_1 - Y_0)
summary(data$Delta)

# mean of Delta ATE = E[Y(1) - Y(0)]
delta <- mean(data$Delta)
cat("delta (ATE) :", round(delta,3), "\n") # 1.004


# Q7. Simulations, delta_T (ATT) and beta_D values ------------------

## Q7.1. Definition of treatment and outcome variables ----

# Model 2: difference in potential outcomes above a threshold (c=1)

# define treated individuals: D = 1 if Delta > 1.5
data <- data %>% mutate(D = as.numeric(Delta > 1.5))

# compute the share of treated individuals
cat("Share of treated individuals: ", round(mean(data$D) * 100, 1), "%\n", sep="")  # 36.2%

# compute the observed outcome : Y = D*Y(1) + (1-D)*Y(0)
data <- data %>% mutate(Y = D * Y_1 + (1 - D) * Y_0)
summary(data$Y)



## Q7.2. Estimations of treatment effect ----

# (1) delta_T : Average Treatment Effect on Treated (ATT)
delta_T <- mean(data %>% filter(D==1) %>% pull(Delta))
cat("delta_T (ATT) :", round(delta_T,3), "\n")  # 2.483 

# (2) beta_d : Difference in outcome between treated and non treated
# compute the average of the outcome for the treated and for the non treated
av_y_treated <- mean(data %>% filter(D==1) %>% pull(Y))
av_y_nontreated <- mean(data %>% filter(D==0) %>% pull(Y))  
cat("Average outcome of treated:", round(av_y_treated,3), "\n") # 1.751
cat("Average outcome of non treated:", round(av_y_nontreated,3), "\n") # 0.405 

# compute the difference between the two groups
beta_D <- av_y_treated - av_y_nontreated
cat("beta_D (outcome difference) :", round(beta_D,3), "\n") # 1.346

# alternative method to compute beta_D: with a regression
# estimate the coefficient of D in the regression of Y
reg <- lm(Y ~ D, data=data)
beta_D_reg <- coef(reg)["D"]
cat("beta_D (via regression) :", round(beta_D_reg,3), "\n") # 1.346 

# (3) selection bias B = beta_D - delta_T
B <- beta_D - delta_T
cat("Selection bias B :", round(B,3), "\n")  # -1.137





