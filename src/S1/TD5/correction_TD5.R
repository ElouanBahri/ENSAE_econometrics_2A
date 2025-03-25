######### Econometrics 1 - TD 5
### 14/11/2024

# Install packages
if(!require(haven)) install.packages("haven")  
if(!require(glmnet)) install.packages("glmnet")
if(!require(plotmo)) install.packages("plotmo")  
if(!require(dplyr)) install.packages("dplyr")
if(!require(knitr)) install.packages("knitr")
if(!require(leaps)) install.packages("leaps")

# Load necessary libraries
library(haven)     # For reading Stata files
library(glmnet)    # For Lasso and Ridge regression
library(plotmo)    # For plots using Lasso and Ridge
library(dplyr)     # For data manipulation
library(knitr)     # For data manipulation
library(leaps)     # For data manipulation

# Set path
setwd("")

# Load the data
data <- read_dta("prostate.dta")

### Question 1 - Prediction error using the full set of covariates

train_data <- data %>% filter(train == "T")
test_data <- data %>% filter(train == "F")
variables <- c("lcavol", "lweight", "age", "lbph", "svi", "lcp", "gleason", "pgg45")

# Linear regression on training data
full_model <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = train_data)
train_data$lpsa_hat <- predict(full_model, newdata = train_data)
test_data$lpsa_hat <- predict(full_model, newdata = test_data)

# Calculate prediction error on training set
train_data$errsq_full <- (train_data$lpsa - train_data$lpsa_hat)^2
train_error_full <- mean(train_data$errsq_full)

# Calculate prediction error on validation set
test_data$errsq_full <- (test_data$lpsa - test_data$lpsa_hat)^2
test_error_full <- mean(test_data$errsq_full)

# Prediction error using only a constant for covariates
constant_model <- lm(lpsa ~ 1, data = train_data)
train_data$lpsa_hat <- predict(constant_model, newdata = train_data)
test_data$lpsa_hat <- predict(constant_model, newdata = test_data)

# Calculate prediction error on training set with constant model
train_data$errsq_constant <- (train_data$lpsa - train_data$lpsa_hat)^2
train_error_constant <- mean(train_data$errsq_constant)

# Calculate prediction error on validation set with constant model
test_data$errsq_constant <- (test_data$lpsa - test_data$lpsa_hat)^2
test_error_constant <- mean(test_data$errsq_constant)

train_error_full
train_error_constant

test_error_full
test_error_constant


#### Question 3 - Determine the best sub-models for k = 1 to k = 8

# Subset selection using `leaps`
leaps_result <- regsubsets(as.formula(paste("lpsa ~", paste(variables, collapse = " + "))),
                           data = train_data, nvmax = length(variables), method = "exhaustive")

# Extract summary of the subset selection
leaps_summary <- summary(leaps_result)

# Get the variables of the best model for each subset size
best_model_vars <- list()
for (i in 1:(leaps_result$np - 1)) {
  best_model_vars[[i]] <- names(coef(leaps_result, i))[-1]  # Exclude intercept
}

# Print the best model for each subset size based on BIC
for (i in 1:length(best_model_vars)) {
  cat("Best model with", i, "variables:", best_model_vars[[i]], "\n")
}

# Determine the best model based on the validation sample

errsq_sub_models <- list()

for (i in 1:(leaps_result$np - 1)) {
  # Get the formula for the current model
  formula <- as.formula(paste("lpsa ~", paste(best_model_vars[[i]], collapse = " + ")))
  
  # Fit the model with the selected variables
  model <- lm(formula, data = train_data)
  
  # Predict 
  test_data$lpsa_hat <- predict(model, newdata = test_data)
  
  # Calculate prediction error on training set with constant model
  test_data$errsq <- (test_data$lpsa - test_data$lpsa_hat)^2
  errsq_sub_models[[i]] <- mean(test_data$errsq)
}

# Print the MSE for each subset size
for (i in 1:length(best_model_vars)) {
  cat("MSE of the validation sample using the best model with", i, "variables:", errsq_sub_models[[i]], "\n")
}

#### Question 4 - optimal models from AIC and BIC

## Note : for AIC and BIC, we use the training sample

# Identify the best model for each subset size based on AIC

aic_values <- numeric(leaps_result$np - 1)

# Calculate AIC for each subset model
for (i in 1:length(aic_values)) {
  # Get the formula for the current model
  selected_vars <- names(coef(leaps_result, i))[-1]  # Exclude intercept
  formula <- as.formula(paste("lpsa ~", paste(selected_vars, collapse = " + ")))
  
  # Fit the model with the selected variables
  model <- lm(formula, data = train_data)
  
  # Calculate AIC and store it
  aic_values[i] <- AIC(model)
}

# Identify the best model for each subset size based on BIC
baseline_bic <- BIC(lm(lpsa ~ 1, data = train_data)) - log(dim(train_data)[1])
bic_values <- baseline_bic + leaps_summary$bic

# Print the AIC values for each subset size
for (i in 1:length(aic_values)) {
  cat("Subset Size:", i, "| AIC:", aic_values[i], "| BIC:", bic_values[i], "| Variables:", names(coef(leaps_result, i))[-1], "\n")
}


#### Question 5 - Standardization ####

scale(data[2:8])

data <- data %>%
  mutate(across(all_of(variables), ~ scale(.)[, 1]))

#### Question 6 - Lasso regression and plot of coefficients ####

train_data <- data %>% filter(train == "T")
test_data <- data %>% filter(train == "F")

# Lasso regression on training data
x_train <- as.matrix(train_data[, variables])
y_train <- train_data$lpsa
lasso_model <- glmnet(x_train, y_train, alpha = 1)

# Plot the coefficient paths
plot_glmnet(lasso_model, label = TRUE)

#### Question 7 - Cross-validation for Lasso and post-lasso regression ####

# Cross-validation to determine best lambda
set.seed(1)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)

# Plot cross-validation error
plot(cv_lasso)

# Best lambda for Lasso
set.seed(1)
lambda_opt <- cv_lasso$lambda.min

### Lasso
lasso_opt <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_opt)
lasso_coefficients <- coef(lasso_opt)
print(lasso_coefficients)

### Post Lasso
selected_features <- rownames(lasso_coefficients)[lasso_coefficients[, 1] != 0][-1]  # Exclude intercept

# Fit OLS model with selected variables from Lasso
post_lasso_model <- lm(as.formula(paste("lpsa ~", paste(selected_features, collapse = " + "))), data = train_data)

# Summary of the post-LASSO model
summary(post_lasso_model)


#### Question 8 - error from post-Lasso regression ####

# Prediction error with Post-Lasso on test set
test_data$lpsa_hat <- predict(post_lasso_model, newdata = test_data)
test_data$errsq_postlasso <- (test_data$lpsa - test_data$lpsa_hat)^2
post_lasso_error <- mean(test_data$errsq_postlasso, na.rm = TRUE)


#### Question 9 - Ridge regression with cross-validation ####

# Cross-validation for Ridge
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)

# Best lambda for Ridge
ridge_lambda_opt <- cv_ridge$lambda.min

# Ridge model with optimal lambda
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = ridge_lambda_opt)

# Post-Ridge prediction on test set
test_data$lpsa_hat_ridge <- predict(ridge_model, newx = as.matrix(test_data[, variables]), s = ridge_lambda_opt)
test_data$errsq_ridge <- (test_data$lpsa - test_data$lpsa_hat_ridge)^2
ridge_error <- mean(test_data$errsq_ridge, na.rm = TRUE)


### Print all results

mse_results <- tibble(
  Model = c("Full Model", 
            "Constant Model", 
            "Best Model", 
            "AIC", 
            "BIC", 
            "Post-Lasso", 
            "Ridge"),
  MSE = c(test_error_full, 
          test_error_constant, 
          errsq_sub_models[3], 
          errsq_sub_models[8], 
          errsq_sub_models[2], 
          post_lasso_error, 
          ridge_error)
)

# Create and print a pretty table using knitr::kable

mse_table <- mse_results %>%
  kable(caption = "Mean Squared Error (MSE) for Different Models") 

