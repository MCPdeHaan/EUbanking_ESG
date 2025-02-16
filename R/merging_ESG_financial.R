library(dplyr); library(openxlsx)




# Merge datasets
dataset <- full_join(ESG_tidy, EU_trans_ex_wide, 
                     by = c("Legal Entity ID (LEI)" = "LEI_code", "year" = "Year")
)

dataset <- dataset %>%
  mutate(
    # Fill missing values using coalesce()
    `Company Common Name` = coalesce(`Company Common Name`, Name),
    `Country of Headquarters` = coalesce(`Country of Headquarters`, Country),
    # Standardize country names 
    `Country of Headquarters` = recode(`Country of Headquarters`,
                                       "Ireland; Republic of" = "Ireland")
  ) %>%
  # Remove redundant columns
  select(-Name, -Country) %>%
  # Rename columns for clarity
  rename(
    LEI = `Legal Entity ID (LEI)`,
    Company = `Company Common Name`,
    `Country of HQ` = `Country of Headquarters`
  ) %>%
  # Reorder columns: place 'year' after 'Country of HQ'
  relocate(year, .after = `Country of HQ`)



unmatched_ESG <- anti_join(ESG_tidy, EU_trans_ex_wide, 
                           by = c("Legal Entity ID (LEI)" = "LEI_code", "year" = "Year"))

# Check rows in EU_trans_ex_wide with no match in ESG_tidy
unmatched_EU <- anti_join(EU_trans_ex_wide, ESG_tidy, 
                          by = c("LEI_code" = "Legal Entity ID (LEI)", "Year" = "year"))

# Glimpse the mismatches
glimpse(unmatched_ESG)
glimpse(unmatched_EU)

common_keys <- intersect(unique(ESG_tidy$`Legal Entity ID (LEI)`), unique(EU_trans_ex_wide$LEI_code))
cat("Number of common LEI codes:", length(common_keys), "\n")

ESG_keys <- ESG_tidy %>% 
  mutate(key = paste(`Legal Entity ID (LEI)`, year, sep = "_")) %>% 
  pull(key)

EU_keys <- EU_trans_ex_wide %>% 
  mutate(key = paste(LEI_code, Year, sep = "_")) %>% 
  pull(key)

common_keys <- intersect(ESG_keys, EU_keys)
cat("Number of common (LEI, year) keys:", length(common_keys), "\n")



# LEI codes in ESG_tidy not in EU_trans_ex_wide
missing_in_EU <- setdiff(unique(ESG_tidy$`Legal Entity ID (LEI)`), unique(EU_trans_ex_wide$LEI_code))
print(missing_in_EU)

# LEI codes in EU_trans_ex_wide not in ESG_tidy
missing_in_ESG <- setdiff(unique(EU_trans_ex_wide$LEI_code), unique(ESG_tidy$`Legal Entity ID (LEI)`))
print(missing_in_ESG)

unmatched_ESG %>% count(year)
unmatched_EU %>% count(Year)

dataset_inner <- inner_join(ESG_tidy, EU_trans_ex_wide, 
                            by = c("Legal Entity ID (LEI)" = "LEI_code", "year" = "Year"))




colnames(dataset)
