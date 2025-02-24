library(dplyr); library(plm); library(lme4); library(car); library(psych)
library(lmtest); library(sandwich)

panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

# Model 1: Basic fixed effects model with lagged ESG
model1 <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    lag(esg_score) + log_assets + loan_quality + liquidity_ratio,
  data   = panel_data, 
  model  = "within", 
  effect = "twoways"
)
summary(model1)

# Model 2: ESG quartiles
model2 <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    factor(esg_quartile) + log_assets + loan_quality + liquidity_ratio,
  data   = panel_data,
  model  = "within", 
  effect = "twoways"
)
summary(model2)

# Model 3: ESG pillars
model3 <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    environmental_pillar_score + social_pillar_score + governance_pillar_score +
    log_assets + loan_quality + liquidity_ratio,
  data   = panel_data, 
  model  = "within", 
  effect = "twoways"
)
summary(model3)

# Model 4: Interaction effect (size moderates ESG)
model4 <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    esg_score * log_assets + loan_quality + liquidity_ratio,
  data   = panel_data, 
  model  = "within", 
  effect = "twoways"
)
summary(model4)

# Check for multicollinearity (VIF)
vif_model <- lm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    esg_score + environmental_pillar_score + social_pillar_score + governance_pillar_score +
    log_assets + loan_quality + liquidity_ratio + factor(country) + factor(year),
  data = data_analysis
)
vif_results <- vif(vif_model)
print(vif_results)

# 4. Test different dependent variables using a "most promising" specification

# List of alternative DVs
dependent_vars <- c(
  "tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
  "total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition",
  "leverage_ratio_using_a_transitional_definition_of_tier_1_capital",
  "equity_to_assets", 
  "loan_to_assets", 
  "provisions_ratio"
)

# Function to run a plm model with pillars and controls
test_model <- function(dep_var) {
  form <- as.formula(
    paste(
      dep_var, 
      "~ environmental_pillar_score + social_pillar_score + governance_pillar_score +",
      "log_assets + loan_quality + liquidity_ratio"
    )
  )
  plm(form, data = panel_data, model = "within", effect = "twoways")
}

# Apply to each DV and store results
model_results <- lapply(dependent_vars, test_model)
names(model_results) <- dependent_vars

# Print summaries (optional)
lapply(model_results, summary)

# 5. Fix high_esg variable creation 
data_analysis <- data_analysis %>%
  group_by(year) %>%
  mutate(high_esg = as.numeric(esg_score > median(esg_score, na.rm = TRUE))) %>%
  ungroup()

# Re-create panel_data after changes
panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

# 7. Test lagged effects of ESG (example: 1-period lag)
test_lagged_effects <- function() {
  # Create lagged columns in panel_data
  panel_data$lag1_esg_score <- lag(panel_data$esg_score, 1)
  panel_data$lag1_env_score <- lag(panel_data$environmental_pillar_score, 1)
  panel_data$lag1_soc_score <- lag(panel_data$social_pillar_score, 1)
  panel_data$lag1_gov_score <- lag(panel_data$governance_pillar_score, 1)
  
  fin_metrics <- c(
    "common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
    "tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
    "total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition",
    "leverage_ratio_using_a_transitional_definition_of_tier_1_capital"
  )
  
  lag_results <- list()
  
  for (metric in fin_metrics) {
    # Overall ESG lag
    f1 <- as.formula(
      paste(metric, "~ lag1_esg_score + log_assets + loan_quality + liquidity_ratio")
    )
    model_overall <- plm(f1, data = panel_data, model = "within", effect = "twoways")
    
    # Pillars lag
    f2 <- as.formula(
      paste(metric, "~ lag1_env_score + lag1_soc_score + lag1_gov_score +",
            "log_assets + loan_quality + liquidity_ratio")
    )
    model_pillars <- plm(f2, data = panel_data, model = "within", effect = "twoways")
    
    lag_results[[paste(metric, "overall")]]  <- summary(model_overall)
    lag_results[[paste(metric, "pillars")]]  <- summary(model_pillars)
  }
  return(lag_results)
}

test_lagged_effects()

# 8. Country-specific models (subset by country)
# Identify major countries (>= 5 observations)
country_counts  <- table(data_analysis$country)
major_countries <- names(country_counts[country_counts >= 5])

test_country_specific <- function() {
  results <- list()
  
  for (country_name in major_countries) {
    country_data  <- data_analysis[data_analysis$country == country_name, ]
    country_panel <- pdata.frame(country_data, index = c("lei_code", "year"))
    
    # Try a within model by country
    tryCatch({
      m <- plm(
        common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
          esg_score + log_assets + loan_quality + liquidity_ratio,
        data   = country_panel, 
        model  = "within", 
        effect = "individual"
      )
      results[[country_name]] <- summary(m)
    }, error = function(e) {
      message("Error with country: ", country_name)
    })
  }
  return(results)
}

country_models <- test_country_specific()

# 9. Non-linear specifications (quadratic ESG)
# Create squared terms in data
data_analysis <- data_analysis %>%
  mutate(
    esg_score_squared = esg_score^2,
    env_score_squared = environmental_pillar_score^2,
    soc_score_squared = social_pillar_score^2,
    gov_score_squared = governance_pillar_score^2
  )

# Update panel_data
panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

# Overall ESG quadratic
model_nl1 <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~
    esg_score + esg_score_squared + log_assets + loan_quality + liquidity_ratio,
  data   = panel_data,
  model  = "within",
  effect = "twoways"
)
summary(model_nl1)

# Pillars with quadratic terms
model_nl2 <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~
    environmental_pillar_score + env_score_squared +
    social_pillar_score         + soc_score_squared +
    governance_pillar_score     + gov_score_squared +
    log_assets + loan_quality + liquidity_ratio,
  data   = panel_data,
  model  = "within",
  effect = "twoways"
)
summary(model_nl2)

# 11. Additional interaction effects

# Size interaction
model_size_int <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~
    esg_score + log_assets + esg_score:log_assets +
    loan_quality + liquidity_ratio,
  data   = panel_data,
  model  = "within",
  effect = "twoways"
)
summary(model_size_int)

# Loan quality interaction
model_quality_int <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~
    esg_score + loan_quality + esg_score:loan_quality +
    log_assets + liquidity_ratio,
  data   = panel_data,
  model  = "within",
  effect = "twoways"
)
summary(model_quality_int)

# 12. Compare Fixed-Effects vs. Pooled OLS, with Clustered Robust SEs
# (A) Fixed Effects (Two-Way) with Clustered SE
model_fe <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~
    esg_score + log_assets + loan_quality + liquidity_ratio,
  data   = panel_data,
  model  = "within",
  effect = "twoways"
)

# Clustered by bank (group)
fe_robust_se <- vcovHC(model_fe, method = "arellano", cluster = "group")
fe_results   <- coeftest(model_fe, fe_robust_se)

cat("\n--- Fixed Effects (Two-Way) with Clustered Robust SEs ---\n")
print(fe_results)

# (B) Pooled OLS with Clustered SE
model_pool <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~
    esg_score + log_assets + loan_quality + liquidity_ratio,
  data   = panel_data,
  model  = "pooling"
)

# Clustered by bank (group)
pool_robust_se <- vcovHC(model_pool, method = "arellano", cluster = "group")
pool_results   <- coeftest(model_pool, pool_robust_se)

cat("\n--- Pooled OLS with Clustered Robust SEs ---\n")
print(pool_results)



library(dplyr); library(plm); library(lmtest); library(sandwich)

# 1. Data Preparation:
data_analysis <- data_analysis %>%
  arrange(lei_code, year) %>%
  # Construct the ratios from raw balance‐sheet columns:
  mutate(
    Equity_to_Assets = total_equity / total_assets,
    Loan_to_Assets = gross_carrying_amount_on_loans_and_advances_including_at_amortised_cost_and_fair_value / total_assets,
    Provisions_Ratio = provisions / gross_carrying_amount_on_loans_and_advances_including_at_amortised_cost_and_fair_value,
    # Control variables:
    log_total_assets = log(total_assets),
    Loan_quality = gross_carrying_amount_on_loans_and_advances_including_at_amortised_cost_and_fair_value / provisions,
    Liquidity_ratio = cash_cash_balances_at_central_banks_and_other_demand_deposits / total_assets
  )

# 2. Create lagged ESG variable (lagged by one period by bank)
data_analysis <- data_analysis %>%
  group_by(lei_code) %>%
  arrange(year) %>%
  mutate(lag_esg = lag(esg_score, n = 1)) %>%
  ungroup()

# 3. Define the list of dependent variables (financial metrics)
dep_vars <- c(
  "common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
  "tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
  "total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition",
  "leverage_ratio_using_a_transitional_definition_of_tier_1_capital"
)

# 4. Run Fixed‐Effects Panel Regressions for each dependent variable:
# Here, we control for bank fixed effects (via the "within" model) and include year fixed effects (as factor(year)).
models <- list()
for(dep in dep_vars) {
  # Build the formula: note that you can add more controls (e.g., country dummies) if appropriate.
  formula_str <- paste(dep, "~ lag_esg + log_total_assets + Loan_quality + Liquidity_ratio + factor(year)")
  model_formula <- as.formula(formula_str)
  
  # Run the fixed-effects (within) panel regression:
  models[[dep]] <- plm(model_formula, data = data_analysis, index = c("lei_code", "year"), model = "within")
  
  # Print a summary for each model:
  print(summary(models[[dep]]))
}

# 5. (Optional) Alternative Specification Including Pillar Scores:
# To test the influence of the pillar scores, you could run a regression that replaces or augments lag_esg with the individual pillar scores.
pillar_formula <- as.formula(
  "common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ lag_esg + social_pillar_score + governance_pillar_score + environmental_pillar_score + log_total_assets + Loan_quality + Liquidity_ratio + factor(year)"
)
pillar_model <- plm(pillar_formula, data = data_analysis, index = c("lei_code", "year"), model = "within")
print(summary(pillar_model))

# 6. (Optional) Compute Robust Standard Errors for one of the models:
# Here we compute robust (HC1) standard errors for the first dependent variable model.
robust_se <- coeftest(models[[ dep_vars[1] ]], vcov = vcovHC(models[[ dep_vars[1] ]], type = "HC1"))
print(robust_se)


library(dplyr)
library(fixest)

# Winsorizing function at the 1% and 99% quantiles
winsorize <- function(x, probs = c(0.01, 0.99)) {
  qnts <- quantile(x, probs = probs, na.rm = TRUE)
  x[x < qnts[1]] <- qnts[1]
  x[x > qnts[2]] <- qnts[2]
  return(x)
}

#############################
# 1. Capital Adequacy Model #
#############################
run_capital_regression <- function(data, esg_var = "esg_score") {
  
  # Sort and group data by bank (lei_code) and year
  data <- data %>% arrange(lei_code, year)
  
  # Create lagged variables by bank:
  # - lagged ESG score
  # - lagged total assets
  # - lagged credit risk (our risk control)
  data <- data %>% 
    group_by(lei_code) %>% 
    arrange(year) %>%
    mutate(
      esg_lag = lag(.data[[esg_var]]),
      total_assets_lag = lag(total_assets),
      credit_risk_lag = lag(credit_risk_excluding_ccr_and_securitisations)
    ) %>% 
    ungroup()
  
  # Winsorize the key variables:
  # - Dependent variable: tier-1 capital
  # - Lagged ESG score and controls
  data <- data %>% 
    mutate(
      cap_wins             = winsorize(tier_1_capital_transitional_period),
      esg_lag_wins         = winsorize(esg_lag),
      total_assets_lag_wins = winsorize(total_assets_lag),
      credit_risk_lag_wins = winsorize(credit_risk_lag)
    )
  
  # Control for bank size via log of lagged total assets
  data <- data %>% mutate(log_total_assets = log(total_assets_lag_wins))
  
  # Define regression formula with bank and year fixed effects.
  # Here we control for both bank size and credit risk.
  formula <- as.formula(
    "cap_wins ~ esg_lag_wins + log_total_assets + credit_risk_lag_wins | lei_code + year"
  )
  
  # Estimate the fixed-effects regression with clustered SEs (by lei_code)
  model <- feols(formula, data = data, cluster = "lei_code")
  
  return(model)
}

###########################
# 2. Credit Risk Model    #
###########################
run_credit_risk_regression <- function(data, esg_var = "esg_score") {
  
  # Sort and group data by bank (lei_code) and year
  data <- data %>% arrange(lei_code, year)
  
  # Create lagged variables by bank:
  # - lagged ESG score and lagged total assets.
  # (Note: Since our dependent variable is credit risk, we do not include its lag as a control here.)
  data <- data %>% 
    group_by(lei_code) %>% 
    arrange(year) %>%
    mutate(
      esg_lag = lag(.data[[esg_var]]),
      total_assets_lag = lag(total_assets)
    ) %>% 
    ungroup()
  
  # Winsorize the key variables:
  # - Dependent variable: credit risk
  # - Lagged ESG score and lagged total assets
  data <- data %>% 
    mutate(
      risk_wins            = winsorize(credit_risk_excluding_ccr_and_securitisations),
      esg_lag_wins         = winsorize(esg_lag),
      total_assets_lag_wins = winsorize(total_assets_lag)
    )
  
  # Control for bank size via log of lagged total assets
  data <- data %>% mutate(log_total_assets = log(total_assets_lag_wins))
  
  # Define regression formula with bank and year fixed effects.
  formula <- as.formula(
    "risk_wins ~ esg_lag_wins + log_total_assets | lei_code + year"
  )
  
  # Estimate the fixed-effects regression with clustered SEs (by lei_code)
  model <- feols(formula, data = data, cluster = "lei_code")
  
  return(model)
}

###########################
# Example Usage:
###########################

# 1. Run the Capital Adequacy regression:
result_capital_new <- run_capital_regression(data_analysis, esg_var = "esg_score")
summary(result_capital_new)

# 2. Run the Credit Risk regression:
result_credit_risk <- run_credit_risk_regression(data_analysis, esg_var = "esg_score")
summary(result_credit_risk)
