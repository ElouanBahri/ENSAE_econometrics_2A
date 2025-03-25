

# Import packages
install.packages("dplyr")
library(haven)
library(dplyr)

# Set the working directory
setwd("~//Documents/Econo1/TD9/TD9")
data <- read_dta("jtpa.dta")

########################################################
################ Question 1  ###########################
########################################################
  
means <- data %>%group_by(z) %>%summarise(mu = mean(y, na.rm = TRUE),d_mean = mean(d, na.rm = TRUE))

# Extract the means
mu1 <- means$mu[means$z == 1]
mu0 <- means$mu[means$z == 0]
d1 <- means$d_mean[means$z == 1]
d0 <- means$d_mean[means$z == 0]

# Compute deltac
deltac <- (mu1 - mu0) / (d1 - d0)
deltac

########################################################
################ Question 2  ###########################
########################################################

PAT <- d0
PNT <- 1-d1
PC <- 1-PAT-PNT

print(PAT)
print(PNT)
print(PC)
#PC is positive, so we do not directly reject the monotonicity assumption

########################################################
################ Question 3  ###########################
########################################################
  
########################### Gender ######################################
means_sex <- data %>%
group_by(z, sex) %>%
summarise(mean_d = mean(d, na.rm = TRUE), .groups = "drop")

# Extract relevant means
woman_0 <- means_sex %>% filter(z == 0, sex == 0) %>% pull(mean_d)
man_0 <- means_sex %>% filter(z == 0, sex == 1) %>% pull(mean_d)
woman_1 <- means_sex %>% filter(z == 1, sex == 0) %>% pull(mean_d)
man_1 <- means_sex %>% filter(z == 1, sex == 1) %>% pull(mean_d)

# Calculate g_woman and g_man
g_woman <- (woman_1 - woman_0) / PC
g_man <- (man_1 - man_0) / PC

# Display results
print(g_woman)
print(g_man)

########################### Operational function #################################

#Function to automatize the computation of g(x)
calculate_g <- function(data, group_var, PC) {
  # Dynamically group by z and the specified variable
  means <- data %>%
    group_by(z, !!sym(group_var)) %>%  # Dynamically pass column name
    summarise(mean_d = mean(d, na.rm = TRUE), .groups = "drop")
  
  # Extract means for the group_var == 1 case
  var_0 <- means %>% filter(z == 0, !!sym(group_var) == 1) %>% pull(mean_d)
  var_1 <- means %>% filter(z == 1, !!sym(group_var) == 1) %>% pull(mean_d)
  #This function does not work for the case of women, would need to change group_var to 0
  
  # Compute the scaled difference
  g_var <- (var_1 - var_0) / PC
  return(g_var)
}

########################### Ethnicity ######################################

calculate_g(data,'black', PC)
calculate_g(data,'hispanic', PC)

########################### Level of education ######################################

calculate_g(data,'hsorged', PC)

########################### Age group ######################################

calculate_g(data,'age2225', PC)
calculate_g(data,'age2629', PC)
calculate_g(data,'age3035', PC)
calculate_g(data,'age3644', PC)
calculate_g(data,'age4554', PC)

########################################################
################ Question 4  ###########################
########################################################
  
# Summary statistics for d
mean_d <- mean(data$d, na.rm = TRUE)

# Calculate lambda
lambda <- (mean_d - PAT) / mean_d

print(lambda)

########################################################
################ Question 5  ###########################
########################################################
  
reg_model <- lm(y ~ d, data = data)
betad <- reg_model$coefficients['d']

B <- betad - deltac
B

########################################################
################ Question 6  ###########################
########################################################
  
##### Recompute delta_c

# Calculate means for females (sex == 0)
means_woman_y <- data %>% 
  filter(sex == 0) %>% 
  group_by(z) %>% 
  summarise(mean_y = mean(y, na.rm = TRUE), .groups = "drop")

means_woman_d <- data %>% 
  filter(sex == 0) %>% 
  group_by(z) %>% 
  summarise(mean_d = mean(d, na.rm = TRUE), .groups = "drop")

# Extract values for females
mu1_woman <- means_woman_y %>% filter(z == 1) %>% pull(mean_y)
mu0_woman <- means_woman_y %>% filter(z == 0) %>% pull(mean_y)

d1_woman <- means_woman_d %>% filter(z == 1) %>% pull(mean_d)
d0_woman <- means_woman_d %>% filter(z == 0) %>% pull(mean_d)

# deltac for females
deltac_woman <- (mu1_woman - mu0_woman) / (d1_woman - d0_woman)

# Repeat for males (sex == 1)
means_man_y <- data %>% 
  filter(sex == 1) %>% 
  group_by(z) %>% 
  summarise(mean_y = mean(y, na.rm = TRUE), .groups = "drop")

means_man_d <- data %>% 
  filter(sex == 1) %>% 
  group_by(z) %>% 
  summarise(mean_d = mean(d, na.rm = TRUE), .groups = "drop")

# Extract values for males
mu1_man <- means_man_y %>% filter(z == 1) %>% pull(mean_y)
mu0_man <- means_man_y %>% filter(z == 0) %>% pull(mean_y)

d1_man <- means_man_d %>% filter(z == 1) %>% pull(mean_d)
d0_man <- means_man_d %>% filter(z == 0) %>% pull(mean_d)

# deltac for males
deltac_man <- (mu1_man - mu0_man) / (d1_man - d0_man)

##### Recompute probabilities

# Probabilities for females
PAT_woman <- d0_woman
PNT_woman <- 1 - d1_woman
PC_woman <- 1 - PAT_woman - PNT_woman

# Probabilities for males
PAT_man <- d0_man
PNT_man <- 1 - d1_man
PC_man <- 1 - PAT_man - PNT_man

##### Recompute lambda

# Lambda for females
mean_d_woman <- mean(data$d[data$sex == 0], na.rm = TRUE)
lambda_woman <- (mean_d_woman - PAT_woman) / mean_d_woman

# Lambda for males
mean_d_man <- mean(data$d[data$sex == 1], na.rm = TRUE)
lambda_man <- (mean_d_man - PAT_man) / mean_d_man

##### Recompute bias

reg_woman <- lm(y ~ d, data = data %>% filter(sex == 0))
betad_woman <- reg_woman$coefficients['d']

B_woman <- betad_woman - deltac_woman

reg_man <- lm(y ~ d, data = data %>% filter(sex == 1))
betad_man <- reg_man$coefficients['d']

B_man <- betad_man - deltac_man

##### Show results

deltac
deltac_woman
deltac_man

B
B_woman
B_man

########################################################
################ Question 9  ###########################
########################################################
  
### Compute standard deviations
  
# Standard deviation of y when z == 0
sigma0 <- sd(data$y[data$z == 0], na.rm = TRUE)

# Standard deviation of y when z == 0 and sex == 0 (females)
sigma0_woman <- sd(data$y[data$z == 0 & data$sex == 0], na.rm = TRUE)

# Standard deviation of y when z == 0 and sex == 1 (males)
sigma0_man <- sd(data$y[data$z == 0 & data$sex == 1], na.rm = TRUE)

### Compute kappa (formula from question 7)

# Mean of z
mean_z <- mean(data$z, na.rm = TRUE)

# lambda_prime and kappa for the whole population
lambda_prime <- (1 - mean_z) / (1 - mean_d)
kappa <- 1 - lambda_prime * PC

# Mean of z for females
mean_z_woman <- mean(data$z[data$sex == 0], na.rm = TRUE)
lambda_prime_woman <- (1 - mean_z_woman) / (1 - mean_d_woman)
kappa_woman <- 1 - lambda_prime_woman * PC_woman

# Mean of z for males
mean_z_man <- mean(data$z[data$sex == 1], na.rm = TRUE)
lambda_prime_man <- (1 - mean_z_man) / (1 - mean_d_man)
kappa_man <- 1 - lambda_prime_man * PC_man

### Compute rho

# Calculate rho for the whole population
rho <- (B * PC * (1 - PC)) / (kappa * sigma0 * dnorm(qnorm(PC)))

# Calculate rho for females
rho_woman <- (B_woman * PC_woman * (1 - PC_woman)) / 
  (kappa_woman * sigma0_woman * dnorm(qnorm(PC_woman)))

# Calculate rho for males
rho_man <- (B_man * PC_man * (1 - PC_man)) / 
  (kappa_man * sigma0_man * dnorm(qnorm(PC_man)))

rho
rho_woman
rho_man