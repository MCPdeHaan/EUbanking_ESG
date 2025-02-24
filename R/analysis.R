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