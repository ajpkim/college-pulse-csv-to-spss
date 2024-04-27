
# This script is used to convert the raw data into a format that can be read by SPSS.
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("haven")) install.packages("haven")
if (!require("jsonlite")) install.packages("jsonlite")

# Load libraries
library(tidyverse)
library(haven) #for writing to .sav for SPSS
library(jsonlite)  #extracting JSON for ImportID

# Load the raw data
encoded_tibble <- read_csv("raw_data/data_encoded.csv")

# SPSS requires variable names to follow certain rules:
# Length: Variable names must not exceed 64 characters. 
# Characters: Only alphanumeric characters and underscores are allowed. 
# Spaces: No spaces are allowed. 
# Special characters: No special characters (e.g., parentheses, dashes) are allowed. 
# Starting character: Names must not start with a number.

# Get data types of each column in the dataframe
data_types <- sapply(encoded_tibble, class)

# Count each type of data
type_count <- table(data_types)

# Print the count of each data type
print(type_count)

# Removing spaces and disallowed characters.
clean_encoded_tibble <- encoded_tibble %>%
  # Replace spaces with underscores
  rename_with(~ str_replace_all(.x, "\\s+", "_")) %>%  
  # Remove non-alphanumeric characters
  rename_with(~ str_replace_all(.x, "[^A-Za-z0-9_]", "")) %>%  
  # Truncate to 64 characters
  rename_with(~ if_else(nchar(.x) > 64, str_sub(.x, 1, 64), .x))%>%
  # Removes starting or ending underscores.
  rename_with(~ str_remove_all(str_remove_all(.x, "^_+"), "_+$"))

# Getting rid of duplicate column names.
# Add an number index to duplicate column names.
names(clean_encoded_tibble) <- tolower(names(clean_encoded_tibble))
names(clean_encoded_tibble) <- make.unique(names(clean_encoded_tibble))

# Adding Labels
# Taking the question description from the second row and importID JSON labels from the originally third row and saving as a label.
# Extract labels from the first two rows and combine them
labels <- sapply(clean_encoded_tibble[1:2, ], function(x) paste(x[1], x[2], sep="  "))

# Remove the first two rows used for labels
clean_encoded_tibble <- clean_encoded_tibble[-(1:2), ]

# Apply labels to each column using mutate() with across()
clean_encoded_tibble_labelled <- clean_encoded_tibble %>%
  mutate(across(everything(), ~labelled(.x, label = labels[[cur_column()]])))

# Looking for Date fields
clean_encoded_tibble_labelled %>%
  select(contains("date")) %>%
  slice(1:50) %>%
  print()

# Define a function to test if a column contains date/time values
# Function to test if any date parsing function succeeds
is_date_column <- function(column) {
  # Remove NA values for testing
  column <- na.omit(column)
  
  # Check if the column can be parsed by any common date parsing functions
  any(!is.na(ymd(column, quiet = TRUE))) ||
    any(!is.na(mdy(column, quiet = TRUE))) ||
    any(!is.na(dmy(column, quiet = TRUE))) ||
    any(!is.na(ymd_hms(column, quiet = TRUE))) ||
    any(!is.na(mdy_hms(column, quiet = TRUE))) ||
    any(!is.na(dmy_hms(column, quiet = TRUE)))
}

# Apply the function across all columns and return those that contain date/time values
date_columns <- sapply(clean_encoded_tibble_labelled, is_date_column)

# All of the potential columns that contain date/time values
names(clean_encoded_tibble_labelled)[date_columns]

# Manual list of date/time fields.Be careful to not include fields labelled as "update" etc.
# Add future columns to this list as needed.
date_vars <- c("startdate", 
               "enddate", 
               "recordeddate",
               "today",
               "lastupdated", 
               "lastdemoupdate",
               "scraped_enddate",
               "lastsurveycomplete")

# Convert date/time fields to datetime objects
clean_encoded_tibble_labelled <- clean_encoded_tibble_labelled %>%
  mutate(across(all_of(date_vars), ~ymd_hms(gsub("T", " ", .), tz = "America/Denver")))

#Writing to SPSS
#Adjust path to be what you want.
write_sav(clean_encoded_tibble_labelled, "clean_data/clean_encoded_tibble.sav")
