#### Preamble ####
# Purpose: Cleans the raw data recorded from dataontario and statcan
# Author: Andrew Goh Yu Xuan
# Date: 27 November 2024
# Contact: andrew.goh@mail.utoronto.ca
# License: MIT
# Pre-requisites: the aforementioned raw data
#### Workspace setup ####
library(dplyr)
library(tidyverse)
library(readxl)
#### Clean data ####
raw_data <- read_excel(here::here("./data/raw_data/academic_performance.xlsx"))

cleaned_data <- raw_data %>%
  select(-contains("Progress"), 
         -contains("Credit Accumulation"), 
         -contains("Results"), 
         -contains("Language"),
         -contains("Type"),
         -contains("Five")) %>%
  drop_na()  

raw_data_2 <- read_csv(here::here("./data/raw_data/school_expenditures.csv"))

cleaned_data_2 <- raw_data_2 %>%
  slice(c(1, 16, 46)) %>%  # Select specific rows
  select(9:ncol(raw_data_2))       # Select columns from the 10th onward

data_values <- cleaned_data_2[1:3, ]
# Transpose data_values to align with board numbers
data_values <- t(data_values)
data_values <- as.data.frame(data_values)
colnames(data_values) <- c("Board Number", "Total Expenses", "Expenditure on Facilities")  # Assign new column names


# Merge the data based on board_number
merged_data <- cleaned_data %>%
  left_join(data_values, by = "Board Number")



table3 <- read_excel(here::here("./data/raw_data/education_indicators.xlsx"))

relevant_data <- table3 %>%
  select(board_number = 1, 
         enrolment = 22, 
         percent_low_income = 47, 
         percent_no_degree = 48)

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

aggregated_data <- aggregated_data %>%
  rename("Board Number" = board_number)

final_data <- merged_data %>%
  left_join(aggregated_data, by = 'Board Number')

final_data <- final_data%>%
  rename("Total Enrolment" = total_enrolment)

final_data <- final_data%>%
  rename("Four Year Graduation Rate" = `Four Year Graduation Rate 2019-2020 Grade 9 Cohort`)

final_data <- final_data %>%
  mutate(
    `Expenditure on Facilities` = as.numeric(`Expenditure on Facilities`),
    `Four Year Graduation Rate` = as.numeric(`Four Year Graduation Rate`),
    `Total Expenses` = as.numeric(`Total Expenses`),
    percentage_spent_on_facilities = (`Expenditure on Facilities` / `Total Expenses`) * 100
  )

final_data <- final_data %>%
  mutate(across(where(is.numeric), ~ round(. , 3)))


