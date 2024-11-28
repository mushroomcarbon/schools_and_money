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
  slice(c(1, 16, 47)) %>%  # Select specific rows
  select(10:ncol(raw_data_2))       # Select columns from the 10th onward

data_values <- cleaned_data_2[1:3, ]
# Transpose data_values to align with board numbers
data_values <- t(data_values)
data_values <- as.data.frame(data_values)
colnames(data_values) <- c("Board Number", "Total Expenses", "Expenditure on Facilities")  # Assign new column names


# Merge the data based on board_number
merged_data <- cleaned_data %>%
  left_join(data_values, by = "Board Number")

# View the final merged dataset
head(merged_data)




cleaned_data <-
  raw_data |>
  janitor::clean_names() |>
  select(wing_width_mm, wing_length_mm, flying_time_sec_first_timer) |>
  filter(wing_width_mm != "caw") |>
  mutate(
    flying_time_sec_first_timer = if_else(flying_time_sec_first_timer == "1,35",
                                   "1.35",
                                   flying_time_sec_first_timer)
  ) |>
  mutate(wing_width_mm = if_else(wing_width_mm == "490",
                                 "49",
                                 wing_width_mm)) |>
  mutate(wing_width_mm = if_else(wing_width_mm == "6",
                                 "60",
                                 wing_width_mm)) |>
  mutate(
    wing_width_mm = as.numeric(wing_width_mm),
    wing_length_mm = as.numeric(wing_length_mm),
    flying_time_sec_first_timer = as.numeric(flying_time_sec_first_timer)
  ) |>
  rename(flying_time = flying_time_sec_first_timer,
         width = wing_width_mm,
         length = wing_length_mm
         ) |> 
  tidyr::drop_na()

#### Save data ####
write_csv(cleaned_data, "outputs/data/analysis_data.csv")
