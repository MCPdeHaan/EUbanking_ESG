library(writexl)

# Extract unique LEI codes
lei_codes <- unique(credit_risk$LEI_Code)

# Convert to data frame
lei_codes_df <- data.frame(LEI_Code = lei_codes)

# Export to Excel
write_xlsx(lei_codes_df, "LEI_Codes.xlsx")
