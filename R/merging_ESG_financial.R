library(dplyr); library(tidyverse)

# merge rows where both ESG and financial data exist
data_analysis <- inner_join(ESG_good, financial_annual, 
                            by = c("lei_code", "year"), 
                            suffix = c(".esg", ".fin")) %>%
  # Handle duplicate name and country columns
  mutate(
    name = coalesce(name.esg, name.fin),
    country = coalesce(country.esg, country.fin)
  ) %>%
  select(-name.esg, -name.fin, -country.esg, -country.fin) %>%
  # Reorder columns for clarity
  select(lei_code, year, name, country, everything()) %>%
  # Add financial ratios and transformations
  mutate(
    # ESG categorizations
    esg_quartile = ntile(esg_score, 4),
    env_quartile = ntile(environmental_pillar_score, 4),
    soc_quartile = ntile(social_pillar_score, 4),
    gov_quartile = ntile(governance_pillar_score, 4),
    # Convert year to factor for analysis
    year_factor = as.factor(year),
    # Create ESG categories for descriptive analysis
    esg_category = cut(esg_score, 
                       breaks = c(0, 33, 66, 100),
                       labels = c("Low", "Medium", "High"),
                       include.lowest = TRUE)
  )

# Optional full join dataset 
data_full <- full_join(ESG, financial_annual, 
                       by = c("lei_code", "year"), 
                       suffix = c(".esg", ".fin")) %>%
  mutate(
    name = coalesce(name.esg, name.fin),
    country = coalesce(country.esg, country.fin)
  ) %>%
  select(-name.esg, -name.fin, -country.esg, -country.fin) %>%
  select(lei_code, year, name, country, everything()) %>%
  clean_names()

# export data_analysis
write_csv(data_analysis, "data/data_analysis.csv")