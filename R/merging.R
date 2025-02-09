library(dplyr)

# Merge datasets
dataset <- merge(
  ESG_tidy, 
  EU_trans_ex_wide, 
  by.x = c("Legal Entity ID (LEI)", "year"), 
  by.y = c("LEI_code", "Year"),
  all = TRUE  # Full outer join to keep all rows
)

# Restructering
dataset <- dataset %>%
  mutate(
    `Company Common Name` = coalesce(`Company Common Name`, Name),  # Fill missing Company names
    `Country of Headquarters` = coalesce(`Country of Headquarters`, Country),  # Fill missing Country
    `Country of Headquarters` = ifelse(`Country of Headquarters` == "Ireland; Republic of", 
                                       "Ireland", `Country of Headquarters`)  # Standardize Ireland name
  ) %>%
  select(
    -Country, -Name  # Remove Name, since unified with Company Common Name
  ) %>%
  rename(
    LEI = `Legal Entity ID (LEI)`, # More common notation
    Company = `Company Common Name`, # Easier to understand
    `Country of HQ` = `Country of Headquarters` # Shorter
  ) %>%
  relocate(year, .after = `Country of HQ`)  # Move year after Country of HQ

# Check structure of mismatches if needed
glimpse(country_mismatches)
glimpse(name_mismatches)