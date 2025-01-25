convert_exposure_code <- function(df) {
  # Comprehensive mapping of exposure codes to full names
  exposure_map <- c(
    # Total / No breakdown
    "0Total / No breakdown" = "Total / No breakdown",
    
    # Central banks and governments
    "101" = "Central banks",
    "102" = "General governments",
    "103" = "Central governments or central banks",
    "104" = "Regional governments or local authorities",
    "105" = "Public sector entities",
    "106" = "Multilateral Development Banks",
    "107" = "International Organisations",
    
    # Financial corporations
    "201" = "Credit institutions",
    "202" = "Financial corporations other than credit institutions",
    "203" = "Institutions",
    "204" = "Institutions without a short-term credit assessment",
    
    # Non-financial corporations
    "301" = "Non-financial corporations",
    "311" = "Non-financial corporations - Small and Medium-sized Enterprises",
    "312" = "Non-financial corporations - Collateralised by commercial immovable property",
    "302" = "Corporates - SME",
    "308" = "Corporates - Real estate. Commercial",
    "303" = "Corporates",
    "304" = "Corporates - Specialised Lending",
    "305" = "Corporates other than specialised lending",
    "306" = "Corporates without a short-term credit assessment",
    "307" = "Institutions and corporates with a short-term credit assessment",
    
    # Households and Retail
    "401" = "Households",
    "431" = "Households of which: Collateralised by residential immovable property",
    "432" = "Households of which: Credit for consumption",
    "402" = "Real estate. Residential",
    "403" = "Credit for consumption",
    "404" = "Retail",
    "405" = "Retail - SME",
    "406" = "Retail – Secured by real estate property",
    "407" = "Retail – Secured by real estate property - SME",
    "408" = "Retail – Secured by real estate property - NON SME",
    "409" = "Retail – Qualifying Revolving",
    "410" = "Retail – Other Retail",
    "411" = "Retail – Other Retail - SME",
    "412" = "Retail – Other Retail - NON SME",
    
    # Secured and Special Categories
    "501" = "Secured by mortgages on immovable property",
    "502" = "Secured by mortgages on immovable property - SME",
    
    # Special Exposures
    "601" = "Exposures in default",
    "602" = "Items associated with particularly high risk",
    "603" = "Covered bonds",
    "604" = "Claims on institutions and corporate with a short-term credit assessment",
    "605" = "Claims in the form of CIU",
    "606" = "Equity exposures",
    "607" = "Other items",
    "608" = "Securitisation",
    
    # Additional Secured Categories
    "700" = "Secured",
    "710" = "Of which secured with immovable property",
    "711" = "Of which instruments with LTV higher than 60% and lower or equal to 80%",
    "712" = "Of which instruments with LTV higher than 80% and lower or equal to 100%",
    "713" = "Of which instruments with LTV higher than 100%"
  )
  
  # Validate input
  if (!"Exposure" %in% colnames(df)) {
    stop("Input dataframe must contain an 'Exposure' column")
  }
  
  # Create exposure_name column by mapping exposure codes
  df$exposure_name <- exposure_map[as.character(df$Exposure)]
  
  # Handle any unmapped exposure codes (optional: replace with NA or a default value)
  df$exposure_name[is.na(df$exposure_name)] <- "Unknown Exposure"
  
  return(df)
}