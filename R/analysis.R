library(plm); library(lmtest); library(sandwich); library(dplyr); library(psych)
library(ggplot2)

panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

# Test individual components of ESG scores
test_individual_components <- function(panel_data) {
  esg_components <- c(
    "esg_score", "environmental_pillar_score", "social_pillar_score", "governance_pillar_score",
    "resource_use_score", "emissions_score", "environmental_innovation_score",
    "workforce_score", "human_rights_score", "community_score", "product_responsibility_score",
    "management_score", "shareholders_score", "csr_strategy_score"
  )
  
  fin_metrics <- c(
    "common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
    "tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
    "total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition",
    "leverage_ratio_using_a_transitional_definition_of_tier_1_capital",
    "equity_to_assets", "loan_to_assets", "provisions_ratio"
  )
  
  results <- data.frame(
    esg_component = character(),
    financial_metric = character(),
    coefficient = numeric(),
    std_error = numeric(),
    t_value = numeric(),
    p_value = numeric(),
    r_squared = numeric()
  )
  
  # Loop over each ESG component and financial metric
  for (component in esg_components) {
    for (metric in fin_metrics) {
      form <- as.formula(
        paste(metric, "~", component, "+ log_assets + loan_quality + liquidity_ratio")
      )
      tryCatch({
        m <- plm(form, data = panel_data, model = "within", effect = "twoways")
        sm <- summary(m)
        if (component %in% rownames(sm$coefficients)) {
          coef_info <- sm$coefficients[component, ]
          results <- rbind(
            results,
            data.frame(
              esg_component    = component,
              financial_metric = metric,
              coefficient      = coef_info["Estimate"],
              std_error        = coef_info["Std. Error"],
              t_value          = coef_info["t-value"],
              p_value          = coef_info["Pr(>|t|)"],
              r_squared        = sm$r.squared
            )
          )
        }
      }, error = function(e) {
        message("Error with model: ", component, " -> ", metric)
      })
    }
  }
  
  results <- results[order(results$p_value), ]
  return(results)
}

component_results <- test_individual_components(panel_data)
print(head(component_results, 20))

# 3. Test Lagged Effects of ESG (1-period lag)
test_lagged_effects <- function(panel_data) {
  # Create lagged variables in panel_data
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
    # Overall ESG lag specification
    f1 <- as.formula(
      paste(metric, "~ lag1_esg_score + log_assets + loan_quality + liquidity_ratio")
    )
    model_overall <- plm(f1, data = panel_data, model = "within", effect = "twoways")
    
    # Pillar-specific lag specification
    f2 <- as.formula(
      paste(metric, "~ lag1_env_score + lag1_soc_score + lag1_gov_score + log_assets + loan_quality + liquidity_ratio")
    )
    model_pillars <- plm(f2, data = panel_data, model = "within", effect = "twoways")
    
    lag_results[[paste(metric, "overall")]] <- summary(model_overall)
    lag_results[[paste(metric, "pillars")]] <- summary(model_pillars)
  }
  return(lag_results)
}

lag_results <- test_lagged_effects(panel_data)
print(lag_results)

# 4. Non-linear Specifications (Quadratic ESG)
# Add squared terms to data_analysis and update panel_data accordingly
data_analysis <- data_analysis %>%
  mutate(
    esg_score_squared = esg_score^2,
    env_score_squared = environmental_pillar_score^2,
    soc_score_squared = social_pillar_score^2,
    gov_score_squared = governance_pillar_score^2
  )
panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

# Overall ESG quadratic specification
model_nl1 <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~
    esg_score + esg_score_squared + log_assets + loan_quality + liquidity_ratio,
  data   = panel_data,
  model  = "within",
  effect = "twoways"
)
print(summary(model_nl1))

# ESG Pillars quadratic specification
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
print(summary(model_nl2))

# 5. Principal Component Analysis (PCA)
# Select ESG-related variables for PCA
esg_vars <- data_analysis %>%
  select(
    esg_score, environmental_pillar_score, social_pillar_score, governance_pillar_score,
    resource_use_score, emissions_score, environmental_innovation_score,
    workforce_score, human_rights_score, community_score, product_responsibility_score,
    management_score, shareholders_score, csr_strategy_score
  )

# Run PCA with varimax rotation (example with 3 factors)
pca_result <- principal(esg_vars, nfactors = 3, rotate = "varimax")
print(pca_result$loadings)

# Extract and add factor scores to data_analysis
pca_scores <- as.data.frame(pca_result$scores)
data_analysis$esg_factor1 <- pca_scores$RC1
data_analysis$esg_factor2 <- pca_scores$RC2
data_analysis$esg_factor3 <- pca_scores$RC3
panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

# Run panel regression using the PCA factors
model_pca <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~
    esg_factor1 + esg_factor2 + esg_factor3 + log_assets + loan_quality + liquidity_ratio,
  data   = panel_data,
  model  = "within",
  effect = "twoways"
)
print(summary(model_pca))

# 6. Fixed Effects & Pooled OLS Models with Robust SEs
# Fixed Effects (Two-Way) Model
model_fe <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    esg_score + log_assets + loan_quality + liquidity_ratio,
  data = panel_data, model = "within", effect = "twoways"
)
fe_robust_se <- vcovHC(model_fe, method = "arellano", cluster = "group")
fe_results <- coeftest(model_fe, fe_robust_se)
print("Fixed Effects Model with Robust SEs:")
print(fe_results)

# Pooled OLS Model
model_pool <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    esg_score + log_assets + loan_quality + liquidity_ratio,
  data = panel_data, model = "pooling"
)
pool_robust_se <- vcovHC(model_pool, method = "arellano", cluster = "group")
pool_results <- coeftest(model_pool, pool_robust_se)
print("Pooled OLS Model with Robust SEs:")
print(pool_results)

# 7. Diagnostic Tests
# Serial Correlation Test (Wooldridge)
serial_test <- pwartest(model_fe)
print("Serial Correlation Test (Wooldridge):")
print(serial_test)

# Heteroskedasticity Test (Breusch-Pagan)
hetero_test <- bptest(model_fe)
print("Heteroskedasticity Test (Breusch-Pagan):")
print(hetero_test)

# 8. Sensitivity Analysis: Complete Data Sample
# Identify banks with complete data (e.g., data available in all 4 years)
complete_banks <- bank_distribution %>% filter(n_years == 4) %>% pull(name)
data_complete <- data_analysis %>% filter(name %in% complete_banks)
panel_data_complete <- pdata.frame(data_complete, index = c("lei_code", "year"))

# Fixed Effects Model on Complete Banks
model_fe_complete <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
    esg_score + log_assets + loan_quality + liquidity_ratio,
  data = panel_data_complete, model = "within", effect = "twoways"
)
fe_complete_robust_se <- vcovHC(model_fe_complete, method = "arellano", cluster = "group")
fe_complete_results <- coeftest(model_fe_complete, fe_complete_robust_se)
print("Fixed Effects Model on Complete Banks with Robust SEs:")
print(fe_complete_results)

# Driscoll-Kraay Robust SEs for the complete-sample FE model
dk_robust_se <- vcovSCC(model_fe_complete, type = "HC0", maxlag = 2)
dk_results <- coeftest(model_fe_complete, dk_robust_se)
print("Fixed Effects Model on Complete Banks with Driscoll-Kraay Robust SEs:")
print(dk_results)

# 9. Predicted Effect of ESG Score
# Create a sequence over the range of ESG scores in the complete sample
esg_seq <- seq(min(panel_data_complete$esg_score, na.rm = TRUE),
               max(panel_data_complete$esg_score, na.rm = TRUE), length.out = 100)

# Calculate mean values for other covariates
mean_log_assets   <- mean(panel_data_complete$log_assets, na.rm = TRUE)
mean_loan_quality <- mean(panel_data_complete$loan_quality, na.rm = TRUE)
mean_liquidity    <- mean(panel_data_complete$liquidity_ratio, na.rm = TRUE)

# Prepare new data for predictions
pred_data <- data.frame(
  esg_score      = esg_seq,
  log_assets     = mean_log_assets,
  loan_quality   = mean_loan_quality,
  liquidity_ratio= mean_liquidity
)

# Extract coefficients from the complete FE model (note: FE models do not have an intercept)
coef_fe <- coef(model_fe_complete)
pred_data$pred <- coef_fe["esg_score"] * pred_data$esg_score +
  coef_fe["log_assets"] * pred_data$log_assets +
  coef_fe["loan_quality"] * pred_data$loan_quality +
  coef_fe["liquidity_ratio"] * pred_data$liquidity_ratio

coef_fe

# Plot the predicted relationship using ggplot2
ggplot(pred_data, aes(x = esg_score, y = pred)) +
  geom_line(color = "blue", size = 1.2) +
  labs(title = "Predicted Effect of ESG Score on Common Equity Ratio",
       x = "ESG Score",
       y = "Predicted Common Equity Ratio") +
  theme_minimal()
