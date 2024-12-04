#### Preamble ####
# Purpose: Tests the analysis dataset.
# Author: Andrew Yu Xuan Goh
# Date: 1 December 2024
# Contact: andrew.goh@mail.utoronto.ca
# License: MIT
# Pre-requisites: Has the analysis data in the form of a parquet.
#### Workspace setup ####
library(tidyverse)
library(arrow)
library(testthat)

#### Load data ####
# Load analysis data from the parquet file
analysis_data <- read_parquet(here::here("./data/analysis_data/analysis_data.parquet"))

#### Define sanity check function ####
# Function to perform tests on the dataset
run_sanity_checks <- function(data, dataset_name) {
  test_that(paste("Sanity Checks for", dataset_name), {
    
    #### Data structure tests ####
    # Check if the dataset is a dataframe or tibble
    expect_true(is.data.frame(data), info = paste(dataset_name, "is not a dataframe or tibble"))
    
    # Verify that the dataset has an appropriate number of rows
    expect_true(nrow(data) >= 5 && nrow(data) <= 1000,
                info = paste(dataset_name, "does not have between 5 and 1000 rows")
    )
    
    # Ensure the dataset has exactly 12 columns
    expect_equal(ncol(data), 12,
                 info = paste(dataset_name, "does not have exactly 12 columns")
    )
    
    # Confirm all required columns are present
    required_cols <- c(
      "Board Number", "Board Name", "Region", "City",
      "Four Year Graduation Rate", "Total Expenses",
      "Expenditure on Facilities", "Total Enrolment",
      "percent_low_income", "percent_no_degree", "percentage_spent_on_facilities",
      "expenses_per_quota"
    )
    expect_true(all(required_cols %in% colnames(data)),
                info = paste(dataset_name, "is missing required columns")
    )
    
    #### Data type tests ####
    # Check the types of key variables
    expect_type(data$`Board Number`, "character")
    expect_type(data$`Board Name`, "character")
    expect_type(data$Region, "character")
    expect_type(data$City, "character")
    expect_type(data$`Four Year Graduation Rate`, "double")
    expect_type(data$`Total Expenses`, "double")
    expect_type(data$`Expenditure on Facilities`, "double")
    expect_type(data$`Total Enrolment`, "double")
    expect_type(data$percent_low_income, "double")
    expect_type(data$percent_no_degree, "double")
    expect_type(data$percentage_spent_on_facilities, "double")
    expect_type(data$expenses_per_quota, "double")
    
    #### Value range tests ####
    # Verify graduation rates are between 0 and 1
    expect_true(all(data$`Four Year Graduation Rate` >= 0 & data$`Four Year Graduation Rate` <= 1),
                info = paste(dataset_name, "`Four Year Graduation Rate` values are out of range")
    )
    
    # Ensure total expenses and facility expenditures are non-negative
    expect_true(all(data$`Total Expenses` >= 0),
                info = paste(dataset_name, "`Total Expenses` has negative values")
    )
    expect_true(all(data$`Expenditure on Facilities` >= 0),
                info = paste(dataset_name, "`Expenditure on Facilities` has negative values")
    )
    
    # Validate calculated percentages for facility expenditures
    calculated_percentage <- (data$`Expenditure on Facilities` / data$`Total Expenses`) * 100
    expect_true(all(abs(calculated_percentage - data$percentage_spent_on_facilities) < 1e-3),
                info = paste(dataset_name, "`percentage_spent_on_facilities` is not calculated correctly")
    )
    
    # Ensure `expenses_per_quota` is calculated correctly
    calculated_quota <- data$`Total Expenses` / data$`Total Enrolment`
    expect_true(all(abs(calculated_quota - data$expenses_per_quota) < 1e-3),
                info = paste(dataset_name, "`expenses_per_quota` is not calculated correctly")
    )
    
    # Confirm income and education percentages are between 0 and 100
    expect_true(all(data$percent_low_income >= 0 & data$percent_low_income <= 100),
                info = paste(dataset_name, "`percent_low_income` values are out of range")
    )
    expect_true(all(data$percent_no_degree >= 0 & data$percent_no_degree <= 100),
                info = paste(dataset_name, "`percent_no_degree` values are out of range")
    )
    
    #### Decimal place tests ####
    # Function to check if a numeric value has 3 decimal places
    has_3_decimals <- function(x) {
      all(abs(x - round(x, 3)) < 1e-6)
    }
    
    # Check all numeric columns for 3 decimal places
    numeric_columns <- c(
      "Four Year Graduation Rate", "Total Expenses", "Expenditure on Facilities", 
      "percent_low_income", "percent_no_degree", "percentage_spent_on_facilities", "expenses_per_quota"
    )
    for (col in numeric_columns) {
      expect_true(has_3_decimals(data[[col]]),
                  info = paste(dataset_name, "column", col, "does not have all values rounded to 3 decimal places"))
    }
    
    #### Missing value tests ####
    # Ensure there are no missing values
    expect_false(any(is.na(data)),
                 info = paste(dataset_name, "contains missing values")
    )
    
    #### Duplicate row tests ####
    # Confirm there are no duplicate rows
    expect_false(any(duplicated(data)),
                 info = paste(dataset_name, "contains duplicate rows")
    )
  })
}

#### Run tests ####
# Execute sanity checks on the dataset
run_sanity_checks(analysis_data, "Analysis Dataset")
