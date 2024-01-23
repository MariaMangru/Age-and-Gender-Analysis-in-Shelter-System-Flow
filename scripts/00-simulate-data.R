#### Preamble ####
# Purpose: Simulates a data set resembling the Toronto Shelter System Flow data.
# Author: Maria Mangru
# Date: 17th January 2024
# Contact: maria.mangru@mail.utoronto.ca
# License: none
# Pre-requisites: tidyverse package


#### Workspace setup ####
library(tidyverse)
install.packages("testthat")
library(testthat)

#### Simulate data ####
# Function to simulate a numeric column
numeric_column <- function(n, min, max) {
  runif(n, min, max)
}

# Function to simulate a categorical column
categorical_column <- function(n, categories) {
  sample(categories, n, replace = TRUE)
}

# Function to simulate a date column
date_column <- function(n, start_date, end_date) {
  seq_dates <- seq(as.Date(start_date), as.Date(end_date), by="month")
  sample(seq_dates, n, replace = TRUE)
}

# Number of rows to simulate
n <- 600


# Simulating columns
id <- 1:n
date <- date_column(n, "2018-01-01", "2024-01-01")
population_group <- categorical_column(n, c("All Population", "Chronic", "Refugees", "Families", "Youth", "Single Adult", "Non-Refugees"))
returned_from_housing <- numeric_column(n, 0, 100) 
returned_to_shelter <- numeric_column(n, 0, 500) 
newly_identified <- numeric_column(n, 0, 1500) 
moved_to_housing <- numeric_column(n, 0, 1000) 
became_inactive <- numeric_column(n, 0, 1500) 
actively_homeless <- numeric_column(n, 0, 12000) 
ageunder16 <- numeric_column(n, 0, 2000) 
age16_24 <- numeric_column(n, 0, 2000) 
age25_44 <- numeric_column(n, 0, 5000) 
age45_64 <- numeric_column(n, 0, 4000) 
age65over <- numeric_column(n, 0, 1000) 
gender_male <- numeric_column(n, 0, 5000) 
gender_female <- numeric_column(n, 0, 5000) 
gender_transgender_nonbinary_or_two_spirit <- numeric_column(n, 0, 100) 
population_group_percentage <- numeric_column(n, 0, 100) 

# Combine into a data frame
simulated_data <- tibble(id, date, population_group, returned_from_housing, returned_to_shelter,
                         newly_identified, moved_to_housing, became_inactive, actively_homeless,
                         ageunder16, age16_24, age25_44, age45_64, age65over, gender_male,
                         gender_female, gender_transgender_nonbinary_or_two_spirit, population_group_percentage)

# Convert date column an appropriate format
simulated_data$date <- format(as.Date(simulated_data$date), "%b-%y")

# Print the first few rows of the simulated data
print(head(simulated_data))




#### Data Tests ####
# Check if all population groups are represented
test_that("All population groups are present", {
  expected_groups <- c("All Population", "Chronic", "Refugees", "Families", "Youth", "Single Adult", "Non-Refugees")
  expect_setequal(unique(simulated_data$population_group), expected_groups)
})

# Check if the number of rows matches the expected count
test_that("Correct number of rows", {
  expect_equal(nrow(simulated_data), n)
})

# Check if the date format is correct
test_that("Date format is correct", {
  expect_true(all(grepl("^\\w{3}-\\d{2}$", simulated_data$date)))
})


# Check if numeric columns are within expected ranges
test_that("Numeric columns within range", {
  expect_true(all(simulated_data$returned_from_housing >= 0 & simulated_data$returned_from_housing <= 100))
  expect_true(all(simulated_data$returned_to_shelter >= 0 & simulated_data$returned_to_shelter <= 500))
  #finish adding the other columns here
})

# Check if percentage values are within 0 to 100
test_that("Percentage values are valid", {
  expect_true(all(simulated_data$population_group_percentage >= 0 & simulated_data$population_group_percentage <= 100))
})

# Check data types of columns
test_that("Data types are correct", {
  expect_is(simulated_data$date, "character")
  expect_is(simulated_data$population_group, "factor")  # or "character", depending on your preference
  #finish adding the other columns here
})
