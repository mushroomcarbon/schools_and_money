#### Preamble ####
# Purpose: Downloads and saves the data from data.ontario.ca
# Author: Andrew Yu Xuan Goh
# Date: 30 November 2024
# Contact: andrew.goh@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
#### Workspace setup ####
library(httr)
library(here)

#### Download data ####
# Define the URL and local file path
url <- "https://data.ontario.ca/dataset/71244894-f3e1-4be0-a69a-9bdc4eb45ac1/resource/9d81dc27-32ef-4864-94b5-f09950d00c72/download/sbpr_data_table_en_sept2024.xlsx"
referrer <- "https://data.ontario.ca/dataset/71244894-f3e1-4be0-a69a-9bdc4eb45ac1"
local_file <- here::here("data", "raw_data", "academic_performance.xlsx")
# Use httr with Referer and User-Agent headers
response <- GET(
  url,
  add_headers(
    `Referer` = referrer,
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"
  )
)

# Check if the request was successful
if (status_code(response) == 200) {
  writeBin(content(response, "raw"), local_file)
  message("File successfully downloaded to: ", local_file)
} else {
  message("Failed to download the file. HTTP status: ", status_code(response))
}


url <- "https://data.ontario.ca/dataset/f9393864-17ae-43f4-a0e1-062fddc6c99c/resource/56598892-c180-4f0d-bde5-63f971d6ef9a/download/efis_2024-25_estimates_-_board_submitted.csv"
referrer <- "https://data.ontario.ca/dataset/f9393864-17ae-43f4-a0e1-062fddc6c99c"
local_file <- here::here("data", "raw_data", "school_expenditures.csv")

# Use httr with Referer and User-Agent headers
response <- GET(
  url,
  add_headers(
    `Referer` = referrer,
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"
  )
)

# Check if the request was successful
if (status_code(response) == 200) {
  writeBin(content(response, "raw"), local_file)
  message("File successfully downloaded to: ", local_file)
} else {
  message("Failed to download the file. HTTP status: ", status_code(response))
}

url <- "https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/e0e90bd5-d662-401a-a6d2-60d69ac89d14/download/new_sif_data_table_2021_22prelim_en_october2024.xlsx"
referrer <- "https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5"
local_file <- here::here("data", "raw_data", "education_indicators.xlsx")

# Use httr with Referer and User-Agent headers
response <- GET(
  url,
  add_headers(
    `Referer` = referrer,
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"
  )
)

# Check if the request was successful
if (status_code(response) == 200) {
  writeBin(content(response, "raw"), local_file)
  message("File successfully downloaded to: ", local_file)
} else {
  message("Failed to download the file. HTTP status: ", status_code(response))
}