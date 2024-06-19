######################################################################
# Author: Alex Kim
# Date: 2024-04-26
#
# This script parses and converts encoded csv data into decoded SPSS
# file with variable labels derived from the encoded file.
#
# Usage:
# - Ensure the required libraries are installed in current R env
# - Update the 3 variables for input/output file paths
# - Run the script
######################################################################
suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
    library(haven)
})
######################################################################
# User defined variables

data_dict_path <- './data/script_updates/v3/data_dict.csv'
encoded_data_path <- './data/script_updates/v3/data_encoded_cleaned.csv'
spss_output_path <- './data/script_updates/v3/output.sav'

######################################################################

load_files <- function(encoded_data_path, data_dict_path) {
    data_dict <- read_csv(data_dict_path, show_col_types = FALSE)
    # Extract column names and descriptions from the first two rows before actually loading data
    tmp_data <- read_csv(encoded_data_path, col_names = FALSE, show_col_types = FALSE)
    col_names <- as.character(tmp_data[1, ])
    # Reload the data, skipping the first two rows to avoid the col description row
    data <- read_csv(encoded_data_path, skip = 2, col_names = col_names, show_col_types = FALSE)

    return(list(data = data, data_dict = data_dict))
}

# Important for adding variable labels for SPSS.
# The 2nd row in the encoded data files contain column descriptions.
get_col_descriptions <- function(encoded_data_path) {
    df <- read_csv(encoded_data_path, col_names = FALSE, show_col_types = FALSE)
    col_names <- as.character(df[1, ])
    col_descriptions <- as.character(df[2, ])
    col_descriptions <- setNames(col_descriptions, col_names)

    return(col_descriptions)
}

# Add variable labels as metadata for SPSS
add_var_labels <- function(data, encoded_data_path) {
    # Add variable labels for SPSS from the 2nd row of encoded data files
    col_descriptions <- get_col_descriptions(encoded_data_path)

    for(col in names(col_descriptions)) {
        if(col %in% names(data)) {
            attr(data[[col]], "label") <- col_descriptions[[col]]
        }
    }
    return (data)
}

# Parse the data dict for the encoded variable values
# and add as value labels for SPSS file.
get_value_labels <- function(data_dict) {
  # Filter out duplicate value-label pairs
  data_dict <- data_dict[!duplicated(paste(data_dict$variable, data_dict$label)), fromLast = TRUE]

  # Cast 'label' column to integers for nicer output in SPSS
  data_dict$label <- as.integer(data_dict$label)

  # Transform the data dictionary into a list of value labels
  value_labels_list <- split(data_dict, data_dict$variable)

  # Transform into a named vector for each variable
  value_labels <- lapply(value_labels_list, function(x) {
    setNames(x$label, x$value)
  })

  return(value_labels)
}

# Function to add value labels to the data
add_value_labels <- function(data, data_dict) {
    value_labels <- get_value_labels(data_dict)

    for(var in names(value_labels)) {
        # Check if the variable exists in the data
        if (!var %in% names(data)) {
            print(paste("Data dict variable", var, "not found in data. Skipping."))
            next
        }

        # Convert the list of labels to a named vector
        labels_vector <- value_labels[[var]]

	# Cast to int here for pretty SPSS labels
        data[[var]] <- labelled(as.integer(data[[var]]), labels = labels_vector)
    }
    return(data)
}

main <- function(encoded_dath_path, data_dict_path, spss_output_path) {
    # Import the encoded data and the data dict
    result <- load_files(encoded_data_path, data_dict_path)
    data <- result$data
    data_dict <- result$data_dict

    # Add factor/value labels for SPSS
    data <- add_value_labels(data, data_dict)

    # Add variable labels for SPSS
    data <- add_var_labels(data, encoded_data_path)

    # Export the processed data to an SPSS file
    write_sav(data, spss_output_path)
    print(paste("Data exported successfully to", spss_output_path))
}

main(encoded_data_path, data_dict_path, spss_output_path)