#### Preamble ####
# Purpose: Models the relationships between variables in the data.
# Author: Andrew Yu Xuan Goh
# Date: 1 November 2024
# Contact: andrew.goh@mail.utoronto.ca
# License: MIT
# Pre-requisites: Has analysis data saved as a parquet
#### Workspace setup ####
library(tidyverse)
library(arrow)
library(brms)
library(broom)
library(caret)

#### Load data ####
# Read analysis data from the parquet file
analysis_data <- read_parquet(here::here("./data/analysis_data/analysis_data.parquet"))

# Rename columns to remove spaces
colnames(analysis_data) <- gsub(" ", "_", colnames(analysis_data))

# Check the new column names
colnames(analysis_data)

#### Data Preprocessing ####
# Select relevant columns for modeling
data <- analysis_data %>%
  dplyr::select(Four_Year_Graduation_Rate, 
         Total_Expenses, 
         percent_no_degree, 
         percent_low_income, 
         percentage_spent_on_facilities)

# Normalize data using caret package for scaling
data_scaled <- data %>%
  mutate(across(everything(), ~ scale(.) %>% as.vector()))

#### Linear Regression Model ####
# Fit the linear regression model
lm_model <- lm(Four_Year_Graduation_Rate ~ Total_Expenses + percent_no_degree + percent_low_income + percentage_spent_on_facilities, data = data_scaled)

# Print model summary
lm_summary <- summary(lm_model)
lm_stats <- broom::tidy(lm_model)

# Print R-squared and p-values
print("Linear Model Summary:")
print(lm_summary)

# Calculate R-squared
rsq <- lm_summary$r.squared
print(paste("R-squared for Linear Regression: ", round(rsq, 3)))

# Print p-values for each predictor
print("P-values for each predictor in the linear regression model:")
print(lm_stats)

#### Bayesian Linear Regression Model ####
# Fit the Bayesian linear regression model using the brms package
bayesian_model <- brm(
  Four_Year_Graduation_Rate ~ Total_Expenses + percent_no_degree + percent_low_income + percentage_spent_on_facilities, 
  data = data_scaled,
  family = gaussian(),
  iter = 2000, 
  warmup = 1000, 
  chains = 4
)

# Print model summary
print("Bayesian Linear Model Summary:")
print(summary(bayesian_model))

# Extract and print the posterior statistics
posterior_summary <- summary(bayesian_model)$fixed
print("Bayesian Model Posterior Summary:")
print(posterior_summary)

#### Generalized Linear Model (GLM) ####
# Fit the Generalized Linear Model (GLM) with a Gaussian family
glm_model <- glm(Four_Year_Graduation_Rate ~ Total_Expenses + percent_no_degree + percent_low_income + percentage_spent_on_facilities, 
                 data = data_scaled, 
                 family = gaussian())

# Print model summary
glm_summary <- summary(glm_model)
glm_stats <- broom::tidy(glm_model)

# Print AIC and p-values
print("GLM Model Summary:")
print(glm_summary)

# Calculate AIC for the GLM
aic_glm <- AIC(glm_model)
print(paste("AIC for GLM: ", round(aic_glm, 3)))

# Print p-values for each predictor
print("P-values for each predictor in the GLM model:")
print(glm_stats)

#### Model Comparison ####
# Compare models using AIC for GLM and R-squared for LM
model_comparison <- tibble(
  Model = c("Linear Regression", "GLM"),
  R_squared_AIC = c(round(rsq, 3), round(aic_glm, 3))
)

print("Model Comparison (R-squared / AIC):")
print(model_comparison)

# Save models to disk for further analysis if needed
saveRDS(lm_model, here::here("./models/lm_model.rds"))
saveRDS(bayesian_model, here::here("./models/bayesian_model.rds"))
saveRDS(glm_model, here::here("./models/glm_model.rds"))