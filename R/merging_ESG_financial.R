library(dplyr)

# 1. Merged dataset (inner join): only rows where both ESG and financial data exist
data_analysis <- inner_join(ESG, financial_year, 
                            by = c("LEI_code", "Year"), 
                            suffix = c(".esg", ".fin")) %>%
  mutate(
    Name_tmp    = if_else(is.na(`Name.esg`), `Name.fin`, `Name.esg`),
    Country_tmp = if_else(is.na(`Country.esg`), `Country.fin`, `Country.esg`)
  ) %>%
  select(-`Name.esg`, -`Name.fin`, -`Country.esg`, -`Country.fin`) %>%
  rename(Name = Name_tmp, Country = Country_tmp) %>%
  # Reorder columns automatically
  select(LEI_code, Year, Name, Country, everything())

# 2. Merged dataset (full join): keeps all rows from either dataset
data_full <- full_join(ESG, financial_year, 
                       by = c("LEI_code", "Year"), 
                       suffix = c(".esg", ".fin")) %>%
  mutate(
    Name_tmp    = if_else(is.na(`Name.esg`), `Name.fin`, `Name.esg`),
    Country_tmp = if_else(is.na(`Country.esg`), `Country.fin`, `Country.esg`)
  ) %>%
  select(-`Name.esg`, -`Name.fin`, -`Country.esg`, -`Country.fin`) %>%
  rename(Name = Name_tmp, Country = Country_tmp) %>%
  # Reorder columns automatically
  select(LEI_code, Year, Name, Country, everything())
