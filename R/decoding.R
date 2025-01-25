# Load required libraries
library(readxl)
library(dplyr)

# Diagnostic function
decode_column <- function(df, column_name, metadata_file, sheet_pattern, 
                          code_column = 1, name_column = 2) {
  # Print start of decoding
  cat("Attempting to decode column:", column_name, "\n")
  
  # Find the correct sheet
  all_sheets <- excel_sheets(metadata_file)
  matching_sheet <- all_sheets[all_sheets == sheet_pattern]
  
  # Detailed sheet matching diagnostics
  cat("All sheets found:", paste(all_sheets, collapse = ", "), "\n")
  cat("Matching sheet for", sheet_pattern, ":", matching_sheet, "\n")
  
  if (length(matching_sheet) == 0) {
    warning(paste("No sheet matching", sheet_pattern, "found"))
    return(df)
  }
  
  # Read the specific sheet from the metadata, skipping the first row
  metadata <- read_excel(metadata_file, sheet = matching_sheet, skip = 1)
  
  # Verify metadata is not empty
  if (nrow(metadata) == 0) {
    warning(paste("No data found in sheet", matching_sheet))
    return(df)
  }
  
  # Print metadata details
  cat("Metadata columns:", names(metadata), "\n")
  cat("Metadata dimensions:", dim(metadata), "\n")
  
  # Rename columns to make them more explicit
  names(metadata)[c(code_column, name_column)] <- c("code", "full_name")
  
  # Trim whitespace and convert to character
  metadata$code <- as.character(metadata$code)
  metadata$full_name <- as.character(metadata$full_name)
  metadata$code <- trimws(metadata$code)
  metadata$full_name <- trimws(metadata$full_name)
  
  # Create a lookup dictionary (named vector)
  lookup <- setNames(metadata$full_name, metadata$code)
  
  # Print lookup details
  cat("Lookup vector length:", length(lookup), "\n")
  print(head(lookup))
  
  # Create a new column with decoded names
  new_column_name <- paste0(column_name, "_decoded")
  
  # Create a new vector for decoding
  decoded_values <- lookup[as.character(df[[column_name]])]
  
  # Replace NA with original values
  decoded_values[is.na(decoded_values)] <- as.character(df[[column_name]])[is.na(decoded_values)]
  
  # Add the new column
  df[[new_column_name]] <- decoded_values
  
  return(df)
}

# Decode columns
credit_risk <- credit_risk %>%
  decode_column("LEI_Code", "data/Metadata.xlsx", "List of Institutions", 
                code_column = 3, name_column = 4) %>%
  decode_column("Portfolio", "data/Metadata.xlsx", "Portfolio") %>%
  decode_column("Exposure", "data/Metadata.xlsx", "Exposure") %>%
  decode_column("Status", "data/Metadata.xlsx", "Status") %>%
  decode_column("Perf_Status", "data/Metadata.xlsx", "Perf_status") %>%
  decode_column("NACE_codes", "data/Metadata.xlsx", "NACE_codes")