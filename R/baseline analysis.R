library(tidyverse); library(plm); library(lmtest); library(sandwich)
library(car); library(quantreg); library(ggplot2); library(AER); library(stargazer)

# Create panel regression data object
panel_data <- pdata.frame(regression_data, index = c("lei_code", "year"))

# 3Fixed Effects Models with Overall ESG Score
# Function to run fixed effects models with robust standard errors
run_fe_model <- function(formula, data) {
  model <- plm(formula, data = data, model = "within")
  robust_results <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  return(list(model = model, robust = robust_results))
}

# Run models for different dependent variables
fe_models_esg <- list(
  cet1 = run_fe_model(cet1_ratio ~ esg_score + log_size + rwa_ratio + year_factor, panel_data),
  leverage = run_fe_model(leverage_ratio ~ esg_score + log_size + rwa_ratio + year_factor, panel_data),
  total_capital = run_fe_model(total_capital_ratio ~ esg_score + log_size + rwa_ratio + year_factor, panel_data),
  rwa = run_fe_model(rwa_ratio ~ esg_score + log_size + year_factor, panel_data)
)
lapply(fe_models_esg, function(x) x$robust)

# 3.2 Fixed Effects Models with ESG Pillar Scores
fe_models_pillars <- list(
  cet1 = run_fe_model(cet1_ratio ~ environmental_pillar_score + social_pillar_score + 
                        governance_pillar_score + log_size + rwa_ratio + year_factor, panel_data),
  leverage = run_fe_model(leverage_ratio ~ environmental_pillar_score + social_pillar_score + 
                            governance_pillar_score + log_size + rwa_ratio + year_factor, panel_data),
  total_capital = run_fe_model(total_capital_ratio ~ environmental_pillar_score + social_pillar_score + 
                                 governance_pillar_score + log_size + rwa_ratio + year_factor, panel_data)
)
lapply(fe_models_pillars, function(x) x$robust)

# Quantile regression analysis
# Prepare data for quantile regression
qr_data <- regression_data %>% 
  select(cet1_ratio, esg_score, environmental_pillar_score, social_pillar_score,
         governance_pillar_score, log_size, rwa_ratio, year_factor, country) %>%
  na.omit()

# Function to run quantile regression for different quantiles
run_quantile_regressions <- function(formula, data, quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  results <- list()
  for (q in quantiles) {
    model <- rq(formula, data = data, tau = q)
    results[[as.character(q)]] <- summary(model)
  }
  return(results)
}

# Run quantile regressions for CET1 ratio with ESG score
qr_cet1_results <- run_quantile_regressions(
  cet1_ratio ~ esg_score + log_size + rwa_ratio + year_factor + country,
  qr_data
)

# Print results for median (0.5) quantile
print(qr_cet1_results[["0.5"]])

# Visualize quantile regression coefficients for ESG score
qr_coefs <- sapply(qr_cet1_results, function(x) x$coefficients["esg_score", 1])
qr_se <- sapply(qr_cet1_results, function(x) x$coefficients["esg_score", 2])
qr_df <- data.frame(
  quantile = as.numeric(names(qr_coefs)),
  coefficient = qr_coefs,
  lower_ci = qr_coefs - 1.96 * qr_se,
  upper_ci = qr_coefs + 1.96 * qr_se
)

p_quantile <- ggplot(qr_df, aes(x = quantile, y = coefficient)) +
  geom_line(size = 1.2, color = "blue") +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Effect of ESG Score on CET1 Ratio Across Quantiles",
       x = "Quantile",
       y = "Coefficient (ESG Score)") +
  theme_minimal()

ggsave("plots/regression/quantile_regression_esg.png", p_quantile, width = 8, height = 6)

# Non-linear relationship analysis
# squared terms in panel data
panel_data$esg_score_squared <- panel_data$esg_score^2
panel_data$env_score_squared <- panel_data$environmental_pillar_score^2
panel_data$soc_score_squared <- panel_data$social_pillar_score^2
panel_data$gov_score_squared <- panel_data$governance_pillar_score^2

# Test for non-linear relationship with overall ESG score
fe_nonlinear_esg <- run_fe_model(
  cet1_ratio ~ esg_score + esg_score_squared + log_size + rwa_ratio + year_factor,
  panel_data
)
print(fe_nonlinear_esg$robust)

# Test for non-linear relationships with individual pillar scores
fe_nonlinear_pillars <- run_fe_model(
  cet1_ratio ~ 
    environmental_pillar_score + env_score_squared +
    social_pillar_score + soc_score_squared +
    governance_pillar_score + gov_score_squared +
    log_size + rwa_ratio + year_factor,
  panel_data
)
print(fe_nonlinear_pillars$robust)

# Visualize the non-linear relationship if significant
if (coef(fe_nonlinear_esg$robust)["esg_score_squared"] < 0.05) {
  esg_range <- seq(min(panel_data$esg_score, na.rm = TRUE), 
                   max(panel_data$esg_score, na.rm = TRUE), 
                   length.out = 100)
  b1 <- coef(fe_nonlinear_esg$model)["esg_score"]
  b2 <- coef(fe_nonlinear_esg$model)["esg_score_squared"]
  
  predicted_values <- data.frame(
    esg_score = esg_range,
    predicted_effect = b1 * esg_range + b2 * esg_range^2
  )
  
  p_nonlinear <- ggplot(predicted_values, aes(x = esg_score, y = predicted_effect)) +
    geom_line(size = 1.2, color = "blue") +
    labs(title = "Non-linear Effect of ESG Score on CET1 Ratio",
         x = "ESG Score",
         y = "Predicted Effect on CET1 Ratio") +
    theme_minimal()
  
  ggsave("plots/regression/nonlinear_esg_effect.png", p_nonlinear, width = 8, height = 6)
}


# Addressing Endogeneity Concerns - Lagged ESG Score
# Create lagged ESG score (using panel data functions to respect time structure)
panel_data$esg_score_lag <- plm::lag(panel_data$esg_score, 1)

# Run model with lagged ESG score
fe_lagged_esg <- run_fe_model(
  cet1_ratio ~ esg_score_lag + log_size + rwa_ratio + year_factor,
  panel_data[!is.na(panel_data$esg_score_lag), ]
)
print("Model with Lagged ESG Score:")
print(fe_lagged_esg$robust)

# 6.2 Sub-Sample Analysis by Bank Size

# Create sub-samples by bank size (median split)
median_size <- median(panel_data$log_size, na.rm = TRUE)
large_banks <- panel_data[panel_data$log_size >= median_size, ]
small_banks <- panel_data[panel_data$log_size < median_size, ]

# Run models separately for large and small banks
fe_cet1_large <- run_fe_model(
  cet1_ratio ~ esg_score + log_size + rwa_ratio + year_factor,
  large_banks
)
fe_cet1_small <- run_fe_model(
  cet1_ratio ~ esg_score + log_size + rwa_ratio + year_factor,
  small_banks
)

print("Large Banks:")
print(fe_cet1_large$robust)
print("Small Banks:")
print(fe_cet1_small$robust)