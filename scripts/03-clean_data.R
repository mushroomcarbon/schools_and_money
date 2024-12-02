#### Preamble ####
# Purpose: Cleans the raw data recorded from data.ontario.ca and saves it as a parquet
# Author: Andrew Yu Xuan Goh
# Date: 27 November 2024
# Contact: andrew.goh@mail.utoronto.ca
# License: MIT
# Pre-requisites: the aforementioned raw data
#### Workspace setup ####
library(dplyr)
library(tidyverse)
library(readxl)
library(arrow)

#### Clean data ####
# Read academic performance data
raw_data <- read_excel(here::here("./data/raw_data/academic_performance.xlsx"))

# Remove unused variables in the data
cleaned_data <- raw_data %>%
  select(-contains("Progress"), 
         -contains("Credit Accumulation"), 
         -contains("Results"), 
         -contains("Language"),
         -contains("Type"),
         -contains("Five")) %>%
  drop_na()  # Cleans out N/A values

# Read school expenditures data
raw_data_2 <- read_csv(here::here("./data/raw_data/school_expenditures.csv"))

# Remove unused variables in the data (as well as filler rows/columns in this case)
cleaned_data_2 <- raw_data_2 %>%
  slice(c(1, 16, 46)) %>%  # Select specific rows
  select(9:ncol(raw_data_2))       # Select columns from the 10th onward

# Transpose data to be in the same format as the first table
cleaned_data_2 <- t(cleaned_data_2)
cleaned_data_2 <- as.data.frame(cleaned_data_2)
colnames(cleaned_data_2) <- c("Board Number", "Total Expenses", "Expenditure on Facilities")  # Assign new column names

# Merge the data from the two tables based on board_number
merged_data <- cleaned_data %>%
  left_join(cleaned_data_2, by = "Board Number")

# Read education indicators data
table3 <- read_excel(here::here("./data/raw_data/education_indicators.xlsx"))

# Select desired variables from the data
relevant_data <- table3 %>%
  select(board_number = 1, 
         enrolment = 22, 
         percent_low_income = 47, 
         percent_no_degree = 48)

# Set numeric variables to numeric variables
relevant_data <- relevant_data %>%
  mutate(
    enrolment = as.numeric(enrolment),
    percent_low_income = as.numeric(percent_low_income),
    percent_no_degree = as.numeric(percent_no_degree)
  )

# Filter out rows where numeric columns have NA (invalid conversion)
relevant_data <- relevant_data %>%
  filter(!is.na(enrolment), !is.na(percent_low_income), !is.na(percent_no_degree))


# Group by board_number and calculate weighted averages for percentages
aggregated_data <- relevant_data %>%
  group_by(board_number) %>%
  summarise(
    total_enrolment = sum(enrolment, na.rm = TRUE),
    percent_low_income = sum(percent_low_income * enrolment, na.rm = TRUE) / total_enrolment,
    percent_no_degree = sum(percent_no_degree * enrolment, na.rm = TRUE) / total_enrolment
  )

# Rename board_number to be the same as the merged table
aggregated_data <- aggregated_data %>%
  rename("Board Number" = board_number)

# Merge the tables to form a final data table
final_data <- merged_data %>%
  left_join(aggregated_data, by = 'Board Number')

# Rename some variables
final_data <- final_data%>%
  rename("Total Enrolment" = total_enrolment)

final_data <- final_data%>%
  rename("Four Year Graduation Rate" = `Four Year Graduation Rate 2019-2020 Grade 9 Cohort`)

# Make sure numeric variables are numeric, and add a new column
final_data <- final_data %>%
  mutate(
    `Expenditure on Facilities` = as.numeric(`Expenditure on Facilities`),
    `Four Year Graduation Rate` = as.numeric(`Four Year Graduation Rate`),
    `Total Expenses` = as.numeric(`Total Expenses`),
    percentage_spent_on_facilities = (`Expenditure on Facilities` / `Total Expenses`) * 100
  )

# Round everything to 3 decimal places
final_data <- final_data %>%
  mutate(across(where(is.numeric), ~ round(. , 3)))

final_data <- na.omit(final_data)

# Save data as a parquet
write_parquet(final_data, here::here("./data/analysis_data/analysis_data.parquet"))
