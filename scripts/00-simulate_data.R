#### Preamble ####
# Purpose: Simulates data from the final analysis dataset
# Author: Andrew Yu Xuan Goh
# Date: 1 December 2024
# Contact: andrew.goh@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
#### Workspace setup ####
library(tidyverse)
library(arrow)

#### Define simulation parameters ####
# Set random seed for reproducibility
set.seed(42)

# Define the number of rows to simulate
n_rows <- 100

#### Simulate data ####
# Generate simulated data for each column
simulated_data <- tibble(
  `Board Number` = sprintf("B%03d", 1:n_rows),  # Simulated board numbers (e.g., B001, B002)
  `Board Name` = paste("Board", 1:n_rows),     # Simulated board names (e.g., Board 1, Board 2)
  Region = sample(c("Region A", "Region B", "Region C"), n_rows, replace = TRUE),  # Random regions
  City = sample(c("City X", "City Y", "City Z"), n_rows, replace = TRUE),          # Random cities
  `Four Year Graduation Rate` = round(runif(n_rows, 0.7, 1), 3),                   # Graduation rates between 0.7 and 1
  `Total Expenses` = round(runif(n_rows, 1e6, 1e8), 3),                            # Total expenses between $1M and $100M
  `Expenditure on Facilities` = round(runif(n_rows, 1e5, 1e7), 3),                 # Facility expenses between $100K and $10M
  `Total Enrolment` = round(runif(n_rows, 100, 5000)),                             # Total enrolment between 100 and 5000 students
  percent_low_income = round(runif(n_rows, 10, 50), 3),                            # Low-income percentage between 10% and 50%
  percent_no_degree = round(runif(n_rows, 5, 30), 3)                               # No-degree percentage between 5% and 30%
) %>%
  # Calculate percentage_spent_on_facilities based on Total Expenses and Expenditure on Facilities
  mutate(
    percentage_spent_on_facilities = round((`Expenditure on Facilities` / `Total Expenses`) * 100, 3)
  )

#### Save simulated data ####
# Write the simulated data to a parquet file
write_parquet(simulated_data, here::here("./data/simulated_data/simulated_data.parquet"))

#### Summary ####
# Print a message indicating the data simulation is complete
cat("Simulated data saved to './data/simulated_data/simulated_data.parquet'.\n")


