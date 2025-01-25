convert_status_code <- function(df) {
  # Comprehensive mapping of status codes to full names
  status_map <- c(
    "0" = "No breakdown by status",
    "1" = "Non defaulted assets", 
    "2" = "Defaulted assets",
    "3" = "New defaulted assets", 
    "4" = "Old defaulted assets"
  )
  
  # Validate input
  if (!"Status" %in% colnames(df)) {
    stop("Input dataframe must contain a 'Status' column")
  }
  
  # Create status_name column by mapping status codes
  df$status_name <- status_map[as.character(df$Status)]
  
  # Handle any unmapped status codes (optional: replace with NA or a default value)
  df$status_name[is.na(df$status_name)] <- "Unknown Status"
  
  return(df)
}