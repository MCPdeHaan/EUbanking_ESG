library(dplyr)
library(fixest)

# Winsorizing function at 1% and 99% quantiles
winsorize <- function(x, probs = c(0.01, 0.99)) {
  qnts <- quantile(x, probs = probs, na.rm = TRUE)
  x[x < qnts[1]] <- qnts[1]
  x[x > qnts[2]] <- qnts[2]
  return(x)
}

run_ESG_regression <- function(data, esg_var = "esg_score") {
  
  # Sort data by bank and year
  data <- data %>% arrange(lei_code, year)
  
  # Create lagged variables for ESG and controls by bank
  data <- data %>% 
    group_by(lei_code) %>% 
    arrange(year) %>%
    mutate(
      esg_lag = lag(.data[[esg_var]]),
      total_assets_lag = lag(total_assets),
      risk_exposure_lag = lag(total_risk_exposure_amount)
    ) %>% 
    ungroup()
  
  # Winsorize the dependent variable and lagged controls
  data <- data %>% 
    mutate(
      cet1_wins             = winsorize(common_equity_tier_1_cet1_capital_transitional_period),
      esg_lag_wins          = winsorize(esg_lag),
      total_assets_lag_wins = winsorize(total_assets_lag),
      risk_exposure_lag_wins = winsorize(risk_exposure_lag)
    )
  
  # Create a bank size control (log of lagged total assets)
  data <- data %>% 
    mutate(log_total_assets = log(total_assets_lag_wins))
  
  # Define the regression formula with bank and year fixed effects
  formula <- as.formula(
    "cet1_wins ~ esg_lag_wins + log_total_assets + risk_exposure_lag_wins | lei_code + year"
  )
  
  # Estimate the fixed effects regression with bank-clustered standard errors
  model <- feols(formula, data = data, cluster = "lei_code")
  
  return(model)
}

# regression for the total ESG Score:
result_total <- run_ESG_regression(data_analysis, esg_var = "esg_score")
summary(result_total)


result_env <- run_ESG_regression(bank_data, esg_var = "Environmental Pillar Score")
summary(result_env)

result_soc <- run_ESG_regression(bank_data, esg_var = "Social Pillar Score")
summary(result_soc)

result_gov <- run_ESG_regression(bank_data, esg_var = "Governance Pillar Score")
summary(result_gov)
