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
library(broom)
library(caret)
library(glmnet)      # For Ridge and Lasso regression

#### Load data ####
analysis_data <- read_parquet(here::here("./data/analysis_data/analysis_data.parquet"))
colnames(analysis_data) <- gsub(" ", "_", colnames(analysis_data))

#### Data Preprocessing ####
data <- analysis_data %>%
  dplyr::select(Four_Year_Graduation_Rate, 
                percent_no_degree, 
                percent_low_income, 
                percentage_spent_on_facilities,
                expenses_per_quota,
                Total_Enrolment)

# Normalize data
data_scaled <- data %>%
  mutate(across(everything(), ~ scale(.) %>% as.vector()))

# Prepare data for glmnet
x <- model.matrix(Four_Year_Graduation_Rate ~ ., data_scaled)[, -1]
y <- data_scaled$Four_Year_Graduation_Rate

#### Linear Regression Model ####
lm_model <- lm(Four_Year_Graduation_Rate ~ ., data = data_scaled)

#### Ridge Regression ####
ridge_cv <- cv.glmnet(x, y, alpha = 0)
ridge_best_lambda <- ridge_cv$lambda.min
ridge_model <- glmnet(x, y, alpha = 0, lambda = ridge_best_lambda)

#### Lasso Regression ####
lasso_cv <- cv.glmnet(x, y, alpha = 1)
lasso_best_lambda <- lasso_cv$lambda.min
lasso_model <- glmnet(x, y, alpha = 1, lambda = lasso_best_lambda)

#### Polynomial Regression ####
poly_model <- lm(Four_Year_Graduation_Rate ~ 
                   poly(percent_no_degree, 2) + 
                   poly(percent_low_income, 2) +
                   poly(percentage_spent_on_facilities, 2) +
                   poly(expenses_per_quota, 2) +
                   poly(Total_Enrolment, 2),
                 data = data_scaled)

#### Model Comparison with MSE ####
# Predictions
lm_preds <- predict(lm_model, newdata = data_scaled)
poly_preds <- predict(poly_model, newdata = data_scaled)
ridge_preds <- predict(ridge_model, newx = x, s = ridge_best_lambda)
lasso_preds <- predict(lasso_model, newx = x, s = lasso_best_lambda)

# Calculate MSE
lm_mse <- mean((lm_preds - data_scaled$Four_Year_Graduation_Rate)^2)
poly_mse <- mean((poly_preds - data_scaled$Four_Year_Graduation_Rate)^2)
ridge_mse <- mean((ridge_preds - y)^2)
lasso_mse <- mean((lasso_preds - y)^2)

# Model Comparison Table
model_comparison <- tibble(
  Model = c("Linear Regression", "Polynomial Regression", "Ridge", "Lasso"),
  Metric = c(
    paste("MSE: ", round(lm_mse, 3)),
    paste("MSE: ", round(poly_mse, 3)),
    paste("MSE: ", round(ridge_mse, 3)),
    paste("MSE: ", round(lasso_mse, 3))
  )
)

print("Model Comparison with MSE:")
print(model_comparison)

#### Save Models ####
saveRDS(lm_model, here::here("./models/lm_model.rds"))
saveRDS(poly_model, here::here("./models/poly_model.rds"))
saveRDS(ridge_model, here::here("./models/ridge_model.rds"))
saveRDS(lasso_model, here::here("./models/lasso_model.rds"))