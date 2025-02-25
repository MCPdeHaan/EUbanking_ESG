library(tidyverse); library(plm); library(lmtest); library(sandwich)
library(car); library(quantreg); library(ggplot2); library(AER)
library(stargazer)

# 3. CORRELATION ANALYSIS ----------------------------------------------------

# Select key variables for correlation analysis
key_vars <- c("esg_score", "environmental_pillar_score", "social_pillar_score", 
              "governance_pillar_score", "common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
              "tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
              "total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition",
              "leverage_ratio_using_a_transitional_definition_of_tier_1_capital",
              "rwa_ratio", "log_total_assets")

# Create correlation matrix
cor_matrix <- cor(data_clean[key_vars], use = "pairwise.complete.obs")
print(cor_matrix)

# Visualize correlation matrix
corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                   tl.col = "black", tl.srt = 45, 
                   title = "Correlation Matrix of Key Variables")

# 4. PANEL REGRESSION MODELS -------------------------------------------------

# 4.1 Fixed Effects Model with Overall ESG Score ------------------------------

# Create panel data object
panel_data <- pdata.frame(data_clean, index = c("lei_code", "year"))

# Fixed effects model for CET1 ratio
fe_cet1 <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    esg_score + log_total_assets + rwa_ratio + year_factor,
  data = panel_data,
  model = "within"
)

# Get robust standard errors
robust_se_cet1 <- coeftest(fe_cet1, vcov = vcovHC(fe_cet1, type = "HC1"))
print(robust_se_cet1)

# Fixed effects model for leverage ratio
fe_leverage <- plm(
  leverage_ratio_using_a_transitional_definition_of_tier_1_capital ~ 
    esg_score + log_total_assets + rwa_ratio + year_factor,
  data = panel_data,
  model = "within"
)

robust_se_leverage <- coeftest(fe_leverage, vcov = vcovHC(fe_leverage, type = "HC1"))
print(robust_se_leverage)

# Fixed effects model for total capital ratio
fe_total_capital <- plm(
  total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    esg_score + log_total_assets + rwa_ratio + year_factor,
  data = panel_data,
  model = "within"
)

robust_se_total_capital <- coeftest(fe_total_capital, vcov = vcovHC(fe_total_capital, type = "HC1"))
print(robust_se_total_capital)

# Fixed effects model for risk-weighted assets ratio
fe_rwa <- plm(
  rwa_ratio ~ 
    esg_score + log_total_assets + year_factor,
  data = panel_data,
  model = "within"
)

robust_se_rwa <- coeftest(fe_rwa, vcov = vcovHC(fe_rwa, type = "HC1"))
print(robust_se_rwa)

# 4.2 Fixed Effects Model with ESG Pillar Scores -----------------------------

# Fixed effects model for CET1 ratio with pillar scores
fe_cet1_pillars <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    environmental_pillar_score + social_pillar_score + governance_pillar_score + 
    log_total_assets + rwa_ratio + year_factor,
  data = panel_data,
  model = "within"
)

robust_se_cet1_pillars <- coeftest(fe_cet1_pillars, vcov = vcovHC(fe_cet1_pillars, type = "HC1"))
print(robust_se_cet1_pillars)

# Fixed effects model for leverage ratio with pillar scores
fe_leverage_pillars <- plm(
  leverage_ratio_using_a_transitional_definition_of_tier_1_capital ~ 
    environmental_pillar_score + social_pillar_score + governance_pillar_score + 
    log_total_assets + rwa_ratio + year_factor,
  data = panel_data,
  model = "within"
)

robust_se_leverage_pillars <- coeftest(fe_leverage_pillars, vcov = vcovHC(fe_leverage_pillars, type = "HC1"))
print(robust_se_leverage_pillars)

# Fixed effects model for total capital ratio with pillar scores
fe_total_capital_pillars <- plm(
  total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    environmental_pillar_score + social_pillar_score + governance_pillar_score + 
    log_total_assets + rwa_ratio + year_factor,
  data = panel_data,
  model = "within"
)

robust_se_total_capital_pillars <- coeftest(fe_total_capital_pillars, vcov = vcovHC(fe_total_capital_pillars, type = "HC1"))
print(robust_se_total_capital_pillars)

# 5. QUANTILE REGRESSION ANALYSIS --------------------------------------------
# Following Izcan & Bektas (2022)

# Prepare data for quantile regression
qr_data <- data_clean %>% 
  select(common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         esg_score, environmental_pillar_score, social_pillar_score, governance_pillar_score,
         log_total_assets, rwa_ratio, year_factor, country) %>%
  na.omit()

# Quantile regression for CET1 ratio
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)

qr_cet1_results <- list()
for (q in quantiles) {
  qr_cet1 <- rq(
    common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
      esg_score + log_total_assets + rwa_ratio + year_factor + country,
    data = qr_data,
    tau = q
  )
  qr_cet1_results[[as.character(q)]] <- summary(qr_cet1)
}

# Print results for median (0.5) quantile
print(qr_cet1_results[["0.5"]])

# 6. NON-LINEAR RELATIONSHIP ANALYSIS ----------------------------------------
# Following Ersoy et al. (2022)

# Add squared ESG score to test for non-linear relationship
panel_data$esg_score_squared <- panel_data$esg_score^2

# Fixed effects model with squared ESG score
fe_cet1_nonlinear <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    esg_score + esg_score_squared + log_total_assets + rwa_ratio + year_factor,
  data = panel_data,
  model = "within"
)

robust_se_cet1_nonlinear <- coeftest(fe_cet1_nonlinear, vcov = vcovHC(fe_cet1_nonlinear, type = "HC1"))
print(robust_se_cet1_nonlinear)

# Test for non-linear relationships with individual pillar scores
panel_data$env_score_squared <- panel_data$environmental_pillar_score^2
panel_data$soc_score_squared <- panel_data$social_pillar_score^2
panel_data$gov_score_squared <- panel_data$governance_pillar_score^2

fe_cet1_pillars_nonlinear <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    environmental_pillar_score + env_score_squared +
    social_pillar_score + soc_score_squared +
    governance_pillar_score + gov_score_squared +
    log_total_assets + rwa_ratio + year_factor,
  data = panel_data,
  model = "within"
)

robust_se_cet1_pillars_nonlinear <- coeftest(fe_cet1_pillars_nonlinear, 
                                             vcov = vcovHC(fe_cet1_pillars_nonlinear, type = "HC1"))
print(robust_se_cet1_pillars_nonlinear)

# 7. ROBUSTNESS CHECKS -------------------------------------------------------

# 7.1 Addressing Endogeneity Concerns ----------------------------------------
# Following Bannier et al. (2021) and Danisman & Tarazi (2023)

# Two-Stage Least Squares (2SLS) approach
# Use lagged ESG score as instrument
# Note: This is a simplified example of the approach

# Create lagged ESG scores
data_clean <- data_clean %>%
  group_by(lei_code) %>%
  mutate(esg_score_lag = lag(esg_score, 1))

# Create data for 2SLS
data_2sls <- data_clean %>%
  filter(!is.na(esg_score_lag)) %>%
  select(lei_code, year, common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         esg_score, esg_score_lag, log_total_assets, rwa_ratio, year_factor, country)

# First stage: regress ESG score on instrument (lagged ESG score)
first_stage <- lm(esg_score ~ esg_score_lag + log_total_assets + rwa_ratio + year_factor + country,
                  data = data_2sls)

# Get predicted values
data_2sls$esg_score_pred <- predict(first_stage, data_2sls)

# Second stage: use predicted ESG score
second_stage <- lm(common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
                     esg_score_pred + log_total_assets + rwa_ratio + year_factor + country,
                   data = data_2sls)

# Print results
summary(second_stage)

# 7.2 Sub-Sample Analysis ----------------------------------------------------

# Create sub-samples by bank size (median split)
median_assets <- median(data_clean$total_assets, na.rm = TRUE)
data_clean$size_category <- ifelse(data_clean$total_assets > median_assets, "Large", "Small")

# Run models separately for large and small banks
large_banks <- data_clean %>% filter(size_category == "Large")
small_banks <- data_clean %>% filter(size_category == "Small")

# Panel data objects for sub-samples
panel_large <- pdata.frame(large_banks, index = c("lei_code", "year"))
panel_small <- pdata.frame(small_banks, index = c("lei_code", "year"))

# CET1 model for large banks
fe_cet1_large <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    esg_score + log_total_assets + rwa_ratio + year_factor,
  data = panel_large,
  model = "within"
)

# CET1 model for small banks
fe_cet1_small <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    esg_score + log_total_assets + rwa_ratio + year_factor,
  data = panel_small,
  model = "within"
)

# Print results
robust_se_cet1_large <- coeftest(fe_cet1_large, vcov = vcovHC(fe_cet1_large, type = "HC1"))
robust_se_cet1_small <- coeftest(fe_cet1_small, vcov = vcovHC(fe_cet1_small, type = "HC1"))

print("Large Banks:")
print(robust_se_cet1_large)
print("Small Banks:")
print(robust_se_cet1_small)

# 8. CREATE SUMMARY TABLES FOR PUBLICATION -----------------------------------

# Combine main models into a table
models_esg <- list(fe_cet1, fe_leverage, fe_total_capital, fe_rwa)
models_pillars <- list(fe_cet1_pillars, fe_leverage_pillars, fe_total_capital_pillars)

# Generate table for models with overall ESG score
stargazer(models_esg, 
          type = "text",
          title = "Impact of ESG Score on Banking Financial Metrics",
          column.labels = c("CET1 Ratio", "Leverage Ratio", "Total Capital Ratio", "RWA Ratio"),
          dep.var.labels = c("CET1 Ratio", "Leverage Ratio", "Total Capital Ratio", "RWA Ratio"),
          covariate.labels = c("ESG Score", "Log Total Assets", "RWA Ratio"),
          omit = "year_factor",
          add.lines = list(c("Bank Fixed Effects", "Yes", "Yes", "Yes", "Yes"),
                           c("Year Fixed Effects", "Yes", "Yes", "Yes", "Yes")),
          model.numbers = FALSE)

# Generate table for models with ESG pillar scores
stargazer(models_pillars, 
          type = "text",
          title = "Impact of ESG Pillar Scores on Banking Financial Metrics",
          column.labels = c("CET1 Ratio", "Leverage Ratio", "Total Capital Ratio"),
          dep.var.labels = c("CET1 Ratio", "Leverage Ratio", "Total Capital Ratio"),
          covariate.labels = c("Environmental Score", "Social Score", "Governance Score", 
                               "Log Total Assets", "RWA Ratio"),
          omit = "year_factor",
          add.lines = list(c("Bank Fixed Effects", "Yes", "Yes", "Yes"),
                           c("Year Fixed Effects", "Yes", "Yes", "Yes")),
          model.numbers = FALSE)