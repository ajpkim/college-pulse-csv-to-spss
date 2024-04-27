# Clear the environment
rm(list = ls())

# DOWNLOAD data from GOOGLE DRIVE

# Get the file ID from the Google Drive link
encoded_file_id <- "https://drive.google.com/file/d/19SCGr-uPlAli11baWjjb5Ty0lXtGSiXH/view?usp=drive_link"
raw_file_id <- "https://drive.google.com/file/d/1ZfyMrB9TRR17JFYEQ6AgSpVaTl_L5eMn/view?usp=sharing"
weighted_file_id <- NA
past_wave_1_file_id <- NA
past_wave_2_file_id <- NA
columnMatching_file_id <- NA


# OUTPUT folder
shared_drive_id <- "15-FwQmtXBabPL66FIR-cmoAbCsLpaWUZ"

#Download Demographic Codebook
dem_codebook_file_id <- "https://drive.google.com/file/d/14-fAbd24qC09ldY-Q2TPe-I-e9CzIrzN/view?usp=drive_link"

#completionCriteria: select the variable to use to accept responses. 
completionCriteria <- "AcceptedResponse_full"
exclusionCriteria_past_wave_1 <- NA
exclusionCriteria_past_wave_1_answer <- NA

#If you are using progress, enter a number
progressCutoff <- 85 

# List of packages to check and install if necessary
required_packages <- c("googledrive", "readr", "dplyr", "ggplot2", "tidyr", "lubridate")

# Function to check and install packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Check and install packages
lapply(required_packages, install_if_missing)

# Load required libraries
library(googledrive)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)


# Authenticate with Google Drive
#drive_auth()

# Download the file to your local environment
drive_download(file = drive_get(raw_file_id), path = "raw_new.csv", overwrite = TRUE)
drive_download(file = drive_get(encoded_file_id), path = "encoded_new.csv", overwrite = TRUE)
drive_download(file = drive_get(dem_codebook_file_id), path = "dem_codebook.csv", overwrite = TRUE)

if(!is.na(weighted_file_id)) {
  drive_download(file = drive_get(weighted_file_id), path = "weights.csv", overwrite = TRUE)
}

if(!is.na(past_wave_1_file_id)) {
  drive_download(file = drive_get(past_wave_1_file_id), path = "past_wave_1.csv", overwrite = TRUE)
}
if(!is.na(past_wave_2_file_id)) {
  drive_download(file = drive_get(past_wave_2_file_id), path = "past_wave_2.csv", overwrite = TRUE)
}

if(!is.na(columnMatching_file_id)) {
  drive_download(file = drive_get(columnMatching_file_id), path = "columnMatching.csv", overwrite = TRUE)
}

# Read the data into data frames
data_raw <- read_csv("raw_new.csv")
data_encoded <- read_csv("encoded_new.csv")
dem_codebook <- read_csv("dem_codebook.csv")
if(!is.na(weighted_file_id)) {
  weights <- read_csv("weights.csv")
}
if(!is.na(past_wave_1_file_id)) {
  data_pastwave1 <- read_csv("past_wave_1.csv")
  data_pastwave1$wave <- 1
  data_raw$wave <- 2
  
  #PastWaveCriteria
  if (!is.na(exclusionCriteria_past_wave_1)) {
    # Filter out rows where the column named exclusionCriteria_past_wave_1 equals exclusionCriteria_past_wave_1_answer
    data_pastwave1 <- data_pastwave1 %>%
      filter(!is.na(.data[[exclusionCriteria_past_wave_1]]) &
               .data[[exclusionCriteria_past_wave_1]] != exclusionCriteria_past_wave_1_answer)
  }
  
  if (!is.na(completionCriteria)) {
    # Create a new column with the name stored in completionCriteria and mark it as TRUE
    data_pastwave1[[completionCriteria]] <- "true"
  }
  
  if (!is.na(columnMatching_file_id)) {
    # Read the column matching file
    column_matching <- read_csv("columnMatching.csv")
    # Track the columns that are changed and not changed
    changed_columns <- vector()
    unchanged_columns <- vector()
    # Iterate over each column index in data_pastwave1
    for (i in 1:length(colnames(data_pastwave1))) {
      col_index <- i
      # Find the matching row index in column_matching for the current column index
      match_index <- which(column_matching[,1] == colnames(data_pastwave1)[col_index])
      
      if (length(match_index) > 0) {
        # Check if column 2 is not blank
        if (!is.na(column_matching[match_index, 2]) && column_matching[match_index, 2] != "") {
          # Change the column name in data_pastwave1 to the corresponding name in column 2 of column_matching
          new_col_name <- column_matching[match_index, 2]
          colnames(data_pastwave1)[col_index] <- new_col_name
          
          # Track changed columns
          changed_columns <- c(changed_columns, col_index)
        } 
      }
    }
    
    #MERGE THE DATASETS
    
    data_pastwave1$wave <- 1
    data_raw$wave <- 2
    
    # Convert all columns to characters in data_pastwave1
    data_pastwave1[] <- lapply(data_pastwave1, as.character)
    
    # Convert all columns to characters in data_raw
    data_raw[] <- lapply(data_raw, as.character)
    
    
    # Bind the rows of data_pastwave1 to data_raw
    merged_data <- bind_rows(data_pastwave1, data_raw)
    
    # Reorder columns to have 'wave' as the first column
    merged_data <- merged_data[, c("wave", setdiff(colnames(merged_data), "wave"))]
    
    data_raw <- merged_data
  }
}

if(!is.na(past_wave_2_file_id)) {
  data_pastwave2 <- read_csv("past_wave_2.csv")
  data_pastwave1$wave <- 1
  data_pastwave2$wave <- 2
  data_raw$wave <- 3
}

#append weights
if(!is.na(weighted_file_id)) {
  # Identify columns containing the word 'weight' in data_raw
  #weight_columns <- grep("weight", names(weights), value = TRUE, ignore.case = TRUE)
  # Convert identified columns to character in data_raw
  #weights[, weight_columns] <- lapply(weights[, weight_columns_raw], as.numeric)
  
  colnames(weights)[colnames(weights) == "responseId"] <- "ResponseId"
  
  # Merge weights with data_raw
  merged_raw <- merge(data_raw, weights, by = "ResponseId", all = TRUE)
  
  # Merge weights with data_encoded
  merged_encoded <- merge(data_encoded, weights, by = "ResponseId", all = TRUE)
  
  data_raw <- merged_raw
  data_encoded <- merged_encoded
}

#SAVE THE FIRST ROW FOR LATER
# Find the row in data_raw with the specified institution_unitid
row_index_raw <- which(data_raw$institution_unitid == "institution_unitid")

#make the second column key
first_row <- data_raw[row_index_raw, ]

# Identify columns containing the word 'weight'
weight_columns_to_replace <- grep("weight", names(first_row), value = TRUE, ignore.case = TRUE)

# Set all values in the first row of weight columns to NA
first_row[1, weight_columns_to_replace] <- NA

# Convert the identified columns to characters
first_row[, weight_columns_to_replace] <- lapply(first_row[, weight_columns_to_replace], as.character)


#MODIFY data

# Get rid of test responses
data_raw <- data_raw[!grepl("collegepulse\\.com", data_raw$RecipientEmail, ignore.case = TRUE), ]
data_encoded <- data_encoded[!grepl("collegepulse\\.com", data_encoded$RecipientEmail, ignore.case = TRUE), ]



# get rid of incentive survey questions

# List of texts to check for (without curly braces)
texts_to_remove <- c(
  '"ImportId":"QID4_TEXT"', '"ImportId":"QID5_TEXT"', '"ImportId":"QID6_TEXT"',
  '"ImportId":"QID7_TEXT"', '"ImportId":"QID8_TEXT"', '"ImportId":"QID12_TEXT"', '"ImportId":"QID1318784929"',
  '"ImportId":"QID13_TEXT"', '"ImportId":"QID1318369978_TEXT"', '"ImportId":"QID3_TEXT"',	'"ImportId":"QID2"'
)

# Function to check if a column contains any of the specified texts
contains_text <- function(col) {
  any(sapply(texts_to_remove, function(text) any(grepl(text, col))))
}

# Identify columns to keep
columns_to_keep <- !sapply(data_raw, contains_text)
encoded_to_keep <- !sapply(data_encoded, contains_text)

# Remove columns from data_raw and encoded
data_raw <- data_raw[, columns_to_keep, drop = FALSE]
data_encoded <- data_encoded[, encoded_to_keep, drop = FALSE]

# If completionCriteria is NOT progress, Filter out rows where completionCriteria variable !=true 


if(completionCriteria!="Progress"){
  data_raw <- data_raw %>% 
    filter(get(completionCriteria) == "true")
  if ("completionCriteria" %in% names(data_encoded)) {
    data_encoded <- data_encoded %>% 
      filter(get(completionCriteria) == "true")
  }
}

# If completionCriteria is "progress, filter out rows where Progress is not equal to progressCutoff

if(completionCriteria=="Progress"){
  data_raw$Progress <- as.numeric(data_raw$Progress)
  data_encoded$Progress <- as.numeric(data_encoded$Progress)
  data_raw <- data_raw[data_raw$Progress >= progressCutoff, ]
  data_encoded <- data_encoded[data_encoded$Progress >= progressCutoff, ]
}



if('remove' %in% colnames(data_raw) && 'remove' %in% colnames(data_encoded)) {
  
  data_raw <- data_raw[is.na(data_raw$remove) | data_raw$remove == "NA", ]
  data_encoded <- data_encoded[is.na(data_encoded$remove) | data_encoded$remove == "NA", ]
}

#append weights
if(!is.na(weighted_file_id)) {
  # Identify columns containing the word 'weight' in data_raw
  weight_columns_raw <- grep("weight", names(data_raw), value = TRUE, ignore.case = TRUE)
  
  # Convert identified columns to numeric in data_raw
  data_raw[, weight_columns_raw] <- lapply(data_raw[, weight_columns_raw], as.numeric)
  weights[, weight_columns_raw] <- lapply(weights[, weight_columns_raw], as.numeric)
}


#Remove Columns

# Check if data_pastwave1 exists
if (exists("data_pastwave1")) {
  # Columns to keep from data_pastwave1
  pastwave_columns <- colnames(data_pastwave1)
} else {
  # If data_pastwave1 does not exist, set pastwave_columns to an empty vector
  pastwave_columns <- character(0)
}

# Columns to keep from data_raw
columns_to_keep <- c(
  "RecordedDate",
  grep("weight", names(data_raw), value = TRUE, ignore.case = TRUE),
  "PanelistID",
  "institution_unitid",
  "institution_institutionName",
  "institution_totalEnrollment",
  "institution_undergraduateEnrollment",
  "institution_sectorOfInstitution",
  "institution_fipsStateCode",
  "institution_undergradType",
  grep("^demographics_", names(data_raw), value = TRUE),
  grep("^Q[0-9.]+", names(data_raw), value = TRUE),
  "campus_1", "campus_2", "campus_3", "campus_4", "campus_5", "campus_6", "campus_7", "campus_3_TEXT", "campus_5_TEXT", "campus_6_TEXT", "hostpoc", "hostisrl", "polact", "hostjews", "hostmusl", "hostility_DO_hostpoc", "hostility_DO_hostisrl", "hostility_DO_polact", "hostility_DO_hostjews", "hostility_DO_hostmusl", "frndleft", "frndright", "frndjew", "frndmusl", "frndpolact", "Friends2_DO_frndleft", "Friends2_DO_frndright", "Friends2_DO_frndjew", "Friends2_DO_frndmusl", "Friends2_DO_frndpolact", "freindsIsrael", "pvrace", "pvcolon", "pvclmt", "pvdvrsty", "PolViews_DO_pvrace", "PolViews_DO_pvcolon", "PolViews_DO_pvclmt", "PolViews_DO_pvdvrsty", "pvmarg", "pvsex", "pvtax", "pvimmg", "PolViews2_DO_pvmarg", "PolViews2_DO_pvsex", "PolViews2_DO_pvtax", "PolViews2_DO_pvimmg", "AHAviolnc", "AHAbottm", "AHApay", "AHAlaws", "DTgood", "DTwrong", "AHA_DO_AHAviolnc", "AHA_DO_AHAbottm", "AHA_DO_AHApay", "AHA_DO_AHAlaws", "AHA_DO_DTgood", "AHA_DO_DTwrong", "News", "friendsIP", "friendsIP_DO_1", "friendsIP_DO_2", "friendsIP_DO_3", "friendsIP_DO_4", "friendsIP_DO_5", "favisrlgov", "favisrlppl", "favhamas", "favpalppl", "Active_1", "Active_2", "Active_3", "Active_4", "Active_5", "Active_6", "Active_7", "AIexist", "AIhamas", "AIsuffr", "AImedia", "AITROPES_DO_AIexist", "AITROPES_DO_AIhamas", "AITROPES_DO_AIsuffr", "AITROPES_DO_AImedia", "AJpower", "AJcare", "AJholcst", "AJaccnt", "ASTROPE_DO_AJpower", "ASTROPE_DO_AJcare", "ASTROPE_DO_AJholcst", "ASTROPE_DO_AJaccnt", "relimp", "RESPRELIG", "RESPRELIG_12_TEXT", "RELASIDE_1", "RELASIDE_2", "RELASIDE_3", "RELASIDE_4", "RELASIDE_5", "pollabel_1", "pollabel_2", "pollabel_3", "pollabel_4", "pollabel_5", "pollabel_6", "pollabel_7", "RELEMOT", "ASatapart", "ASatloyal", "ASexist", "ASpower", "ASriver", "ASrights", "ASgencde", "Statements_DO_ASatapart", "Statements_DO_ASatloyal", "Statements_DO_ASexist", "Statements_DO_ASpower", "Statements_DO_ASriver", "Statements_DO_ASrights", "Statements_DO_ASgencde", "ASIncidents_1", "ASIncidents_2", "ASIncidents_3", "ASIncidents_5", "ASIncidents_6", "ASIncidents_7", "Israel", "BRI", "polviews",
  pastwave_columns
)


data_raw <- data_raw[, columns_to_keep]
if (all(columns_to_keep %in% names(data_encoded))) {
  data_encoded <- data_encoded[, columns_to_keep]
}

# Remove specific demographics columns
columns_to_remove_demographics <- c(
  "twoYearUndergradWeight",
  "demographics_legacy",
  "demographics_greek",
  "institution_undergradType",
  "Antisemitism,Antizionism,andIslamophobia1_DO_Q7.1",
  "demographics_issueImportance",
  "demographics_twoYearFourYear",
  "demographics_raceOld",
  "demographics_gradProgram",
  "demographics_varsitySportType",
  "demographics_transfer",
  "demographics_geography",
  "demographics_transferLocation",
  "demographics_highschoolGradyear",
  "demographics_publicOrPrivate",
  "demographics_undergradUniversity",
  "demographics_undergradGradyear",
  "demographics_politicalIdeology",
  "demographics_livingSituation",
  "demographics_studentType",
  "demographics_preLawMed",
  "demographics_internatlStudent",
  "demographics_onlineClasses",
  "demographics_internatlCitizenship_one",
  "demographics_internatlCitizenship_two",
  "demographics_internatlCitizenship_three")

data_raw <- data_raw[, !names(data_raw) %in% columns_to_remove_demographics, drop = FALSE]
data_encoded <- data_encoded[, !names(data_encoded) %in% columns_to_remove_demographics, drop = FALSE]



#append weights
if(!is.na(weighted_file_id)) {
  weight_columns_to_replace <- grep("weight", names(data_raw), value = TRUE, ignore.case = TRUE)
  
  # Convert the identified columns to characters
  data_raw[, weight_columns_to_replace] <- lapply(data_raw[, weight_columns_to_replace], as.character)
}

#Bring back second row
# Create a new row with all NA values
new_row <- setNames(rep(NA, ncol(data_raw)), names(data_raw))

# Iterate through every column in data_raw
for (col_name in names(data_raw)) {
  # Check if the column name is present in first_row
  if (col_name %in% names(first_row)) {
    # Assign the corresponding value from first_row to the new_row
    new_row[[col_name]] <- first_row[[col_name]]
  }
}

# Bind the new row to data_raw
data_raw <- bind_rows(data_raw, new_row)
data_encoded <- bind_rows(data_encoded, new_row)


#CODEBOOK-encoding

#Fix certain columns encoding
#politicalLeaning - make lowercase

if("demographics_politicalLeaning" %in% names(data_raw)){
  
  data_raw$demographics_politicalLeaning <- tolower(data_raw$demographics_politicalLeaning)
  data_encoded$demographics_politicalLeaning <- tolower(data_encoded$demographics_politicalLeaning)
}
#firstGen - remove commas
if("demographics_firstGen" %in% names(data_raw)){
  
  data_raw$demographics_firstGen <- gsub(",", "", data_raw$demographics_firstGen)
  data_encoded$demographics_firstGen <- gsub(",", "", data_encoded$demographics_firstGen)
  
}
# Find common columns between data_encoded and dem_codebook
data_encoded_cols <- names(data_encoded)
dem_codebook_cols <- dem_codebook$Demographic_name

common_cols <- intersect(data_encoded_cols, dem_codebook_cols)

# Loop through common columns
for (column_name in common_cols) {
  # Find corresponding rows in dem_codebook
  encoding_row <- dem_codebook[grep(paste0("^", column_name, "$", collapse = "|"), dem_codebook_cols, ignore.case = TRUE), ]
  
  # Check if encoding information is found
  if (nrow(encoding_row) > 0) {
    # Loop through rows in data_encoded and replace values
    for (i in seq_along(data_encoded[[column_name]])) {
      matched_value <- data_encoded[[column_name]][i]
      matched_row <- dem_codebook[dem_codebook$Demographic_name == column_name & dem_codebook$Key == matched_value, ]
      
      if (nrow(matched_row) > 0) {
        # Update the value in data_encoded
        data_encoded[[column_name]][i] <- matched_row$Code
      }
    }
  } else {
    cat("No match found for column:", column_name, "\n")
  }
}


dem_codebook_used <- dem_codebook[dem_codebook$Demographic_name %in% common_cols, ]


# Column name to update
column_name <- "demographics_age"

# Check if the specified column exists in data_encoded
if (column_name %in% names(data_encoded)) {
  # Update values based on conditions
  data_encoded[[column_name]] <- ifelse(
    grepl("demographic", data_encoded[[column_name]], ignore.case = TRUE) | !grepl("\\D", data_encoded[[column_name]]),
    data_encoded[[column_name]],
    -99
  )
} else {
  cat("Column not found in data_encoded:", column_name, "\n")
}


# -99 for all empty demographics. 
# Identify columns with "Q" followed by a number in data_raw
matching_columns_raw <- names(data_raw)[grepl("Q[0-9]", names(data_raw))]

# Replace blanks with -99 for the rest in data_raw
data_raw <- data_raw %>% 
  mutate_at(vars(-matches(matching_columns_raw)), ~ ifelse(is.na(.), -99, .))

# Identify columns with "Q" followed by a number in data_encoded
matching_columns_encoded <- names(data_encoded)[grepl("Q[0-9]", names(data_encoded))]

# Replace blanks with -99 for the rest in data_encoded
data_encoded <- data_encoded %>% 
  mutate_at(vars(-matches(matching_columns_encoded)), ~ ifelse(is.na(.), -99, .))

# Replace "preferNotToSay" with -99 in data_raw
data_raw <- data_raw %>% 
  mutate_all(~ ifelse(. == "preferNotToSay", -99, .))

#Make the columns description row the first row

# Find the row in data_raw with the specified institution_unitid
row_index_raw <- which(data_raw$institution_unitid == "institution_unitid")

# Find the row in data_encoded with the specified institution_unitid
row_index_encoded <- which(data_encoded$institution_unitid == "institution_unitid")

# Move the row to the first position in data_raw
data_raw <- data_raw[c(row_index_raw, setdiff(1:nrow(data_raw), row_index_raw)), ]

# Move the row to the first position in data_encoded
data_encoded <- data_encoded[c(row_index_encoded, setdiff(1:nrow(data_encoded), row_index_encoded)), ]

# Assuming data_raw and data_encoded are the names of your dataframes

#append weights
if(!is.na(weighted_file_id)) {
  # Identify columns containing the word 'weight' in data_raw
  weight_columns_raw <- grep("weight", names(data_raw), value = TRUE, ignore.case = TRUE)
  
  # Replace rows with -99 in weight columns with ""
  data_raw[, weight_columns_raw][data_raw[, weight_columns_raw] == -99] <- ""
  
  # Identify columns containing the word 'weight' in data_encoded
  weight_columns_encoded <- grep("weight", names(data_encoded), value = TRUE, ignore.case = TRUE)
  
  # Replace rows with -99 in weight columns with ""
  data_encoded[, weight_columns_encoded][data_encoded[, weight_columns_encoded] == -99] <- ""
}


# Delete rows with institution_institutionName equal to -99 in data_raw
#data_raw <- data_raw[data_raw$institution_institutionName != -99, ]

# Delete rows with institution_institutionName equal to -99 in data_encoded
#data_encoded <- data_encoded[data_encoded$institution_institutionName != -99, ]

# Delete rows with _cp_user_id equal to -99 in data_raw
#data_raw <- data_raw[data_raw$`_cp_user_id` != -99, ]

# Delete rows with _cp_user_id equal to -99 in data_encoded
#data_encoded <- data_encoded[data_encoded$`_cp_user_id` != -99, ]

#CreateCrosstabs
# Create a copy of data_raw
data_crosstabs <- data_raw

# Select only columns that start with "Q" followed by a number and do not contain "DO"
q_columns <- grep("^Q[0-9]+", colnames(data_crosstabs), value = TRUE)
q_columns <- q_columns[!grepl("DO", q_columns)]

# Iterate over each selected column
for (col in q_columns) {
  # Concatenate the current column name with the data from the first row in the corresponding column
  new_col_name <- paste0(col, " ", data_raw[1, col])
  # Assign the new column name
  colnames(data_crosstabs)[colnames(data_crosstabs) == col] <- new_col_name
}

# Remove the first row
data_crosstabs <- data_crosstabs[-1, ]


# UPLOAD back to GOOGLE DRIVE

# Upload the modified data frames instead of the original ones
write_csv(data_raw, "data_raw.csv")  # Save the modified data frame to a new file
write_csv(data_crosstabs, "data_crosstabs.csv")  # Save the modified data frame to a new file
write_csv(data_encoded, "data_encoded.csv")  # Save the modified data frame to a new file
write_csv(dem_codebook_used, "dem_codebook.csv")

# Get the current date in the specified format
current_date <- format(Sys.Date(), format = "%m_%d_%Y")

# Specify the names for the files on Google Drive with the current date
drive_file_name_raw <- paste0("data_raw_", current_date, ".csv")
drive_file_name_encoded <- paste0("data_encoded_", current_date, ".csv")
drive_file_name_crosstabs <- paste0("data_crosstabs_", current_date, ".csv")


drive_file_name_dem_codebook <- paste0("demographic_codebook.csv")


# Upload the files to Google Drive without specifying the drive
uploaded_raw <- drive_upload(media = "data_raw.csv", name = drive_file_name_raw)
uploaded_encoded <- drive_upload(media = "data_encoded.csv", name = drive_file_name_encoded)
uploaded_dem_codebook <- drive_upload(media = "dem_codebook.csv", name = drive_file_name_dem_codebook)
uploaded_crosstabs <- drive_upload(media = "data_crosstabs.csv", name = drive_file_name_crosstabs)


# Set the parent folder for the uploaded files (assuming shared_drive_id is the correct shared drive ID)
drive_update(file = uploaded_raw$id, add_parents = shared_drive_id)
drive_update(file = uploaded_encoded$id, add_parents = shared_drive_id)
drive_update(file = uploaded_dem_codebook$id, add_parents = shared_drive_id)
drive_update(file = uploaded_crosstabs$id, add_parents = shared_drive_id)



