library(dplyr); library(janitor)

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

# unify names to prevent problems
data_analysis <- data_analysis %>% clean_names()
data_full <- data_full %>% clean_names()

# Compute summary statistics and bank distribution (needed later for sensitivity analysis)
summary_inner <- data_analysis %>%
  summarise(
    total_banks = n_distinct(lei_code),
    total_years = n_distinct(year),
    total_obs   = n()
  )
print(summary_inner)

year_distribution <- data_analysis %>%
  group_by(year) %>%
  summarise(
    n_obs   = n(),
    n_banks = n_distinct(lei_code)
  ) %>%
  arrange(year)
print(year_distribution)

bank_distribution <- data_analysis %>%
  group_by(name) %>%
  summarise(n_years = n_distinct(year)) %>%
  arrange(desc(n_years))
print(bank_distribution)
