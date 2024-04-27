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

library(readr)
library(dplyr)
library(haven)

######################################################################
# User defined variables

data_dict_path <- './data/data_dictionary.csv'
encoded_data_path <- './data/data_encoded_04_24_2024.csv'
spss_output_path <- './data/spss-output_04_24_2024.sav'

######################################################################

load_files <- function(encoded_data_path, data_dict_path) {
    data_dict <- read_csv(data_dict_path, show_col_types = FALSE)
    # Extract column names and descriptions from the first two rows before actually loading data
    tmp_data <- read_csv(encoded_data_path, col_names = FALSE, show_col_types = FALSE)
    col_names <- as.character(tmp_data[1, ])
    # Reload the data, skipping the first two rows to avoid the col description row
    encoded_data <- read_csv(encoded_data_path, skip = 2, col_names = col_names, show_col_types = FALSE)

    return(list(encoded_data = encoded_data, data_dict = data_dict))
}


get_col_descriptions <- function(encoded_data_path) {
    # The 2nd row in the encoded data files are columnd description
    # which we pass along to SPSS as variable labels.
    df <- read_csv(encoded_data_path, col_names = FALSE, show_col_types = FALSE)
    col_names <- as.character(df[1, ])
    col_descriptions <- as.character(df[2, ])
    col_descriptions <- setNames(col_descriptions, col_names)

    return(col_descriptions)
}

decode_data <- function(encoded_data, data_dict) {
    # Ensure data_dict has proper columns
    if (!("variable" %in% names(data_dict)) || !("label" %in% names(data_dict)) || !("value" %in% names(data_dict))) {
        stop("data_dict must contain 'variable', 'label', and 'value' columns.")
    }

    # Loop over the coded variables defined in data_dict and replace encoded values
    for (var in unique(data_dict$variable)) {
        if (var %in% names(encoded_data)) {
            # Extract mapping for this variable
            mapping <- data_dict %>%
                       filter(variable == var)

            # Replace encoded numeric codes with descriptive text values
            # Find matches for each value in the column
            matches <- match(encoded_data[[var]], mapping$label)

            # Use matches to replace only those values that have a corresponding match in the data dictionary
            # NA values in 'matches' mean there was no corresponding key in 'mapping$label' for that value
            decoded_values <- ifelse(is.na(matches), encoded_data[[var]], mapping$value[matches])

            # Convert the entire column to character type, now safely handling non-matching cases
            encoded_data[[var]] <- as.character(decoded_values)
        } else {
            warning(paste("Variable", var, "not found in encoded_data."))
        }
    }
    return (encoded_data)
}



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

get_categorical_cols <- function(decoded_data) {
    # Identify numeric columns first
    numeric_cols <- names(decoded_data)[sapply(decoded_data, is.numeric)]
    # Define the pattern to match column names starting with 'Q' followed by numbers and possibly a decimal
    pattern <- "^Q\\d+(\\.\\d+)?"
    # Filter numeric column names based on the pattern
    cat_cols <- grep(pattern, numeric_cols, value = TRUE)

    return(cat_cols)
}

convert_categorical_cols_to_factors <- function(decoded_data) {
    # Identify all the categorical cols we want to convert to factors
    cat_cols <- get_categorical_cols(decoded_data)
    # Loop through the cat_cols and convert each specified column to a factor
    for (col in cat_cols) {
        decoded_data[[col]] <- as.factor(decoded_data[[col]])
    }
    return(decoded_data)
}

main <- function(encoded_dath_path, data_dict_path, spss_output_path) {
    # Import the encoded data and the data dict
    result <- load_files(encoded_data_path, data_dict_path)
    encoded_data <- result$encoded_data
    data_dict <- result$data_dict
    # Decode data
    data <- decode_data(encoded_data, data_dict)
    # Add variable labels for SPSS
    data <- add_var_labels(data, encoded_data_path)
    # Convert categorical cols to factor type
    data <- convert_categorical_cols_to_factors(data)
    # Export the processed data to an SPSS file
    write_sav(data, spss_output_path)
    print(paste("Data exported successfully to", spss_output_path))

    return(data)
}

main(encoded_data_path, data_dict_path, spss_output_path)