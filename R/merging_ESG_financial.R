library(dplyr); library(janitor); library(tidyverse)

# merge only rows where both ESG and financial data exist
data_analysis <- inner_join(ESG, financial_year, 
                            by = c("LEI_code", "Year"), 
                            suffix = c(".esg", ".fin")) %>%
  # Handle duplicate name and country columns
  mutate(
    Name = coalesce(Name.esg, Name.fin),
    Country = coalesce(Country.esg, Country.fin)
  ) %>%
  select(-Name.esg, -Name.fin, -Country.esg, -Country.fin) %>%
  # Reorder columns for clarity
  select(LEI_code, Year, Name, Country, everything()) %>%
  # Clean column names for consistency
  clean_names() %>%
  # Add financial ratios and transformations
  mutate(
    # Financial ratios
    equity_to_assets = total_equity / total_assets,
    loan_to_assets = gross_carrying_amount_on_loans_and_advances_including_at_amortised_cost_and_fair_value / 
      total_assets,
    provisions_ratio = provisions / 
      gross_carrying_amount_on_loans_and_advances_including_at_amortised_cost_and_fair_value,
    liquidity_ratio = cash_cash_balances_at_central_banks_and_other_demand_deposits / total_assets,
    
    # Log transformations
    log_assets = log(total_assets),
    
    # Risk measures
    loan_quality = gross_carrying_amount_on_loans_and_advances_including_at_amortised_cost_and_fair_value / 
      provisions,
    rwa_ratio = total_risk_exposure_amount / total_assets,
    
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

# 2. Optional full join dataset 
data_full <- full_join(ESG, financial_year, 
                       by = c("LEI_code", "Year"), 
                       suffix = c(".esg", ".fin")) %>%
  mutate(
    Name = coalesce(Name.esg, Name.fin),
    Country = coalesce(Country.esg, Country.fin)
  ) %>%
  select(-Name.esg, -Name.fin, -Country.esg, -Country.fin) %>%
  select(LEI_code, Year, Name, Country, everything()) %>%
  clean_names()
