
# Set up ----
# Load necessary libraries
library(haven)     # For reading Stata files
library(dplyr)     # For data manipulation
library(estimatr)  # For robust regression
library(stats)     # For performing test

# Load the data

getwd()
data <- read_dta('nsw.dta')

# Q2: Descriptive statistics ----
summary(data)
data %>% 
  count(education) %>% 
  mutate(perc = round(prop.table(n),3),
         cum = round(cumsum(prop.table(n)),3))

# Q3: compute Ybar(1) - Ybar(0) ----

ybar <- data %>% group_by(treat) %>% summarise(mean = mean(re78))
ybar
diff_ybar <- filter(ybar, treat == 1)$mean - filter(ybar, treat == 0)$mean
diff_ybar

# Q4: Perform a regression of Y on D. Was the result on the coefficient expected? ----

model = lm_robust(re78 ~ treat, se_type = "stata", data = data)
summary(model)

# Q5: How can we test that the assignment to treatment was indeed made randomly? Perform these tests and comment them. ----

# Continuous variable: re75 and age --> perform Kolmogorov-Smironov test

# For income (re75)
# Get re75 distribution by group
income_control <- data %>% filter(treat == 0) %>% pull(re75)
income_treated <- data %>% filter(treat == 1) %>% pull(re75)

# Perform Kolmogorov-Smirnov test for re75 by treat
ks_test_re75 <- ks.test(income_control, income_treated)
ks_test_re75

# Same for age
# Get age distribution by group
age_control <- data %>% filter(treat == 0) %>% pull(age)
age_treated <- data %>% filter(treat == 1) %>% pull(age)

# Perform Kolmogorov-Smirnov test for age by treat
ks_test_age <- ks.test(age_treated, age_control)
ks_test_age

# Categorical variable: education and age (age can be both) --> perform chi2 test

# For education
education_treat_table <- table(education = data$education, treat = data$treat)
education_treat_table
chi2_test_educ <- chisq.test(education_treat_table)
chi2_test_educ

# Same for education
age_treat_table <- table(age = filter(data, age <= 35)$age, treat = filter(data, age <= 35)$treat)
age_treat_table
chi2_test_age <- chisq.test(age_treat_table)
chi2_test_age

# For dummy variables: student test
dummy_vars <- c("black", "hispanic", "married", "nodegree")

# Loop through each variable and perform t-test
t_test_results <- lapply(dummy_vars, function(var) {
  # Subset the data by treatment group
  control <- data[data$treat == 0, var, drop = TRUE]
  treated <- data[data$treat == 1, var, drop = TRUE]
  
  # Perform t-test assuming unequal variances
  t_test <- t.test(control, treated, var.equal = FALSE)
  
  # Store results in a list
  list(variable = var, t_test = t_test)
})

# Print the results
for (result in t_test_results) {
  cat("\nVariable:", result$variable, "\n")
  print(result$t_test)
}

# Optional: plot distribution fort some variables
library(tidyr)
library(ggplot2)
vars_to_plot = c('age', 'education', 're75', 're78')
data_long <- data %>%
  select(treat, all_of(vars_to_plot)) %>%
  pivot_longer(cols = -treat, names_to = "variable", values_to = "value")

# Plot CDFs
ggplot(data_long, aes(x = value, color = as.factor(treat))) +
  stat_ecdf() +  # Empirical CDF
  facet_wrap(~variable, scales = "free") +  # One panel per variable
  scale_color_manual(values = c("blue", "red"), labels = c("Control (0)", "Treatment (1)")) +
  # coord_cartesian(xlim = c(0, 30000)) +
  labs(
    x = "Value",
    y = "Cumulative Probability",
    color = "Treatment Status",
    title = "CDF by Treatment Status for Selected Variables"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Q6: Run a regression of $Y$ on 1975 income and then on nodegree. ----
model2 = lm_robust(re78 ~ re75, se_type = "stata", data = data)
summary(model2)
model3 = lm_robust(re78 ~ nodegree, se_type = "stata", data = data)
summary(model3)

# Q7: Merge the two tables. Make some descriptive statistics of the included variables according to their original sample ----

# Load second dataset
data2 <- read_dta('psid_controls.dta', col_select = names(data))

# Combine (row bind) the two datasets
data = rbind(data, data2)

# Get mean by dataset for all variables
data %>% group_by(data_id) %>% summarise_all(mean)

# Q8: drop the control observations from the experimental data ----
# With the remaining observations, compute an estimate of the treatment effect using a simple linear regression.
data = data %>% filter(data_id == 'PSID' | (data_id == 'Lalonde Sample' & treat == 1))
model4 = lm_robust(re78 ~ treat, se_type = "stata", data = data)
summary(model4)

# Recall estimates on experimental sample only
summary(model)

# Why the result are so different?
variables <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75")

# Perform t-test for each variable
t_test_results <- lapply(variables, function(var) {
  # Extract values for each treatment group
  group1 <- data[data$treat == 0, var, drop = TRUE] # control in representative sample
  group2 <- data[data$treat == 1, var, drop = TRUE] # treated in experimental sample
  
  # Perform t-test assuming unequal variances
  t_test <- t.test(group1, group2, var.equal = FALSE)
  
  # Store results in a list
  list(variable = var, t_test = t_test)
})

# Print the results
for (result in t_test_results) {
  cat("\nVariable:", result$variable, "\n")
  print(result$t_test)
}

# Q10: Perform such a regression. Comment on the obtained estimates and standard errors. ----
model5 <- lm_robust(re78 ~ treat + age + education + black + hispanic + married + nodegree + re75,
                    se_type = "stata", data = data)
summary(model5)

