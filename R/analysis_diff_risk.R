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
