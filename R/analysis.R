# Test individual ESG components separately (avoid collinearity)
test_individual_components <- function() {
  
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
  
  # Loop over all components & metrics
  for (component in esg_components) {
    for (metric in fin_metrics) {
      form <- as.formula(
        paste(metric, "~", component, "+ log_assets + loan_quality + liquidity_ratio")
      )
      # Safely fit model in case of errors
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
  
  # Sort by p-value
  results <- results[order(results$p_value), ]
  return(results)
}

# Run individual component tests
component_results <- test_individual_components()
head(component_results, 20)  # Just inspect top 20 rows


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



# 10. Principal Component Analysis (PCA) to reduce multicollinearity

# Select ESG variables for PCA
esg_vars <- data_analysis %>%
  select(
    esg_score, environmental_pillar_score, social_pillar_score, governance_pillar_score,
    resource_use_score, emissions_score, environmental_innovation_score,
    workforce_score, human_rights_score, community_score, product_responsibility_score,
    management_score, shareholders_score, csr_strategy_score
  )

# PCA with varimax rotation (nfactors = 3 as an example)
pca_result <- principal(esg_vars, nfactors = 3, rotate = "varimax")
print(pca_result$loadings)

# Extract factor scores
pca_scores <- as.data.frame(pca_result$scores)

# Add factor scores to data
data_analysis$esg_factor1 <- pca_scores$RC1
data_analysis$esg_factor2 <- pca_scores$RC2
data_analysis$esg_factor3 <- pca_scores$RC3

panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

# Model with the extracted PCA factors
model_pca <- plm(
  common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~
    esg_factor1 + esg_factor2 + esg_factor3 +
    log_assets + loan_quality + liquidity_ratio,
  data   = panel_data,
  model  = "within",
  effect = "twoways"
)
summary(model_pca)
