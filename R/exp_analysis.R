# Step 1: Data preparation and basic exploration
# Create necessary ratios and transformations
data_analysis <- data_analysis %>%
  mutate(
    # Financial ratios
    equity_to_assets = total_equity / total_assets,
    loan_to_assets = gross_carrying_amount_on_loans_and_advances_including_at_amortised_cost_and_fair_value / total_assets,
    provisions_ratio = provisions / gross_carrying_amount_on_loans_and_advances_including_at_amortised_cost_and_fair_value,
    liquidity_ratio = cash_cash_balances_at_central_banks_and_other_demand_deposits / total_assets,
    
    # Log transformations
    log_assets = log(total_assets),
    
    # Loan quality
    loan_quality = gross_carrying_amount_on_loans_and_advances_including_at_amortised_cost_and_fair_value / provisions,
    
    # Create categorical variables for ESG scores (might reveal non-linear relationships)
    esg_quartile = ntile(esg_score, 4),
    env_quartile = ntile(environmental_pillar_score, 4),
    soc_quartile = ntile(social_pillar_score, 4),
    gov_quartile = ntile(governance_pillar_score, 4)
  )

# Step 2: Initial exploration - check correlations
esg_financial_cors <- data_analysis %>%
  select(esg_score, environmental_pillar_score, social_pillar_score, governance_pillar_score,
         common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         leverage_ratio_using_a_transitional_definition_of_tier_1_capital,
         equity_to_assets, loan_to_assets, provisions_ratio) %>%
  cor(use = "pairwise.complete.obs")

# Print correlation matrix
print(esg_financial_cors)

# export correlation to csv 
write.csv(esg_financial_cors, "esg_financial_cors.csv")


# Step 3: Visualization of key relationships
library(ggplot2)

# ESG score vs Capital Ratio
ggplot(data_analysis, aes(x = esg_score, 
                          y = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition)) +
  geom_point(aes(color = country)) +
  geom_smooth(method = "lm") +
  labs(title = "ESG Score vs CET1 Ratio",
       x = "ESG Score", y = "CET1 Ratio (%)") +
  theme_minimal()

# export to jpg
ggsave("esg_cet1_ratio.jpg")