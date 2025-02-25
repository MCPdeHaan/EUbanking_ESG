library(tidyverse); library(plm); library(lmtest); library(sandwich)
library(car); library(quantreg); library(ggplot2); library(corrplot)
library(stargazer); library(psych); library(gridExtra)

# Create panel data object
panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

# --- Panel regression model --- 

# Function to run fixed effects models with robust standard errors
run_fe_model <- function(formula, data) {
  model <- plm(formula, data = data, model = "within")
  robust_results <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  return(list(model = model, robust = robust_results))
}

# Fixed Effects Models with Overall ESG Score 
fe_models_esg <- list(
  cet1 = run_fe_model(cet1_ratio ~ esg_score + log_size + rwa_ratio + year_factor, panel_data),
  leverage = run_fe_model(leverage_ratio ~ esg_score + log_size + rwa_ratio + year_factor, panel_data),
  total_capital = run_fe_model(total_capital_ratio ~ esg_score + log_size + rwa_ratio + year_factor, panel_data),
  rwa = run_fe_model(rwa_ratio ~ esg_score + log_size + year_factor, panel_data)
)

# Print robust results
cat("\n=== Fixed Effects Models with Overall ESG Score ===\n")
for(model_name in names(fe_models_esg)) {
  cat("\nModel for", model_name, ":\n")
  print(fe_models_esg[[model_name]]$robust)
}

# Fixed Effects Models with ESG Pillar Scores 
fe_models_pillars <- list(
  cet1 = run_fe_model(cet1_ratio ~ environmental_pillar_score + social_pillar_score + 
                        governance_pillar_score + log_size + rwa_ratio + year_factor, panel_data),
  leverage = run_fe_model(leverage_ratio ~ environmental_pillar_score + social_pillar_score + 
                            governance_pillar_score + log_size + rwa_ratio + year_factor, panel_data),
  total_capital = run_fe_model(total_capital_ratio ~ environmental_pillar_score + social_pillar_score + 
                                 governance_pillar_score + log_size + rwa_ratio + year_factor, panel_data)
)

# Print robust results
cat("\n=== Fixed Effects Models with ESG Pillar Scores ===\n")
for(model_name in names(fe_models_pillars)) {
  cat("\nModel for", model_name, ":\n")
  print(fe_models_pillars[[model_name]]$robust)
}

# Component analysis with individual ESG components
test_individual_components <- function(panel_data) {
  # Define ESG components and selected financial metrics
  esg_components <- c(
    "esg_score", "environmental_pillar_score", "social_pillar_score", "governance_pillar_score",
    "resource_use_score", "emissions_score", "environmental_innovation_score",
    "workforce_score", "human_rights_score", "community_score", "product_responsibility_score",
    "management_score", "shareholders_score", "csr_strategy_score"
  )
  
  # Filter only available components
  available_components <- esg_components[esg_components %in% names(panel_data)]
  
  fin_metrics <- c(
    "cet1_ratio", "tier1_ratio", "total_capital_ratio",
    "leverage_ratio", "equity_to_assets", "loan_to_assets", "provisions_ratio"
  )
  
  # Prepare results dataframe
  results <- data.frame(
    esg_component = character(),
    financial_metric = character(),
    coefficient = numeric(),
    std_error = numeric(),
    t_value = numeric(),
    p_value = numeric(),
    r_squared = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop over ESG components and financial metrics
  for (component in available_components) {
    for (metric in fin_metrics) {
      if (all(c(component, metric, "log_size", "loan_quality", "liquidity_ratio") %in% names(panel_data))) {
        form <- as.formula(
          paste(metric, "~", component, "+ log_size + loan_quality + liquidity_ratio")
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
          message("Error with model: ", component, " -> ", metric, ": ", e$message)
        })
      }
    }
  }
  
  results <- results[order(results$p_value), ]
  return(results)
}


# Run component analysis
component_results <- test_individual_components(panel_data)
significant_results <- component_results[component_results$p_value < 0.05, ]

# Print the top 20 most significant results
cat("\n=== Top 20 Most Significant ESG Component Relationships ===\n")
print(head(component_results, 20))

# Export results
write.csv(component_results, "results/esg_component_financial_metric_results.csv", row.names = FALSE)

# Ensure we get exactly 10 results, even if we need to take less significant ones
top_results <- head(component_results, 10)

# Verify we have 10 rows
cat("Number of rows in top_results:", nrow(top_results), "\n")

# Make sure significance levels are properly coded
top_results$significance <- ifelse(top_results$p_value < 0.01, "p < 0.01", 
                                   ifelse(top_results$p_value < 0.05, "p < 0.05", "p < 0.10"))

# Create clearer labels without truncation
top_results$label <- paste0(
  gsub("_score", "", top_results$esg_component),  # Remove "_score" suffix for cleaner display
  " → ",
  gsub("_ratio", "", top_results$financial_metric)  # Remove "_ratio" suffix
)

# Debug: Print the prepared data
print(top_results)

# Create the plot with explicit height parameter for bars
p_top_components <- ggplot(top_results, 
                           aes(x = reorder(label, coefficient), 
                               y = coefficient, fill = significance)) +
  geom_bar(stat = "identity", width = 0.7) +  # Control bar width
  coord_flip() +
  labs(title = "Top 10 ESG Component Relationships with Financial Metrics",
       subtitle = "Ordered by coefficient magnitude",
       x = "ESG Component → Financial Metric",
       y = "Coefficient") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_text(size = 9),
        panel.grid.major.y = element_blank()) +  # Remove horizontal grid lines
  scale_fill_manual(values = c("p < 0.01" = "#1b9e77", 
                               "p < 0.05" = "#7570b3", 
                               "p < 0.10" = "#d95f02"))

# Save with explicit dimensions
ggsave("plots/regression/top_esg_component_relationships.png", 
       p_top_components, width = 12, height = 10, dpi = 300)

# As a backup, try another visualization approach
p_top_components_alt <- ggplot(top_results, 
                               aes(x = 1:10, 
                                   y = coefficient, 
                                   fill = significance)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(
    breaks = 1:10,
    labels = top_results$label,
    expand = c(0.01, 0.01)
  ) +
  coord_flip() +
  labs(title = "Top 10 ESG Component Relationships with Financial Metrics",
       subtitle = "Ordered by coefficient magnitude",
       y = "Coefficient",
       x = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_text(size = 9)) +
  scale_fill_manual(values = c("p < 0.01" = "#1b9e77", 
                               "p < 0.05" = "#7570b3", 
                               "p < 0.10" = "#d95f02"))

# Save alternative visualization
ggsave("plots/regression/top_esg_component_relationships_alt.png", 
       p_top_components_alt, width = 12, height = 10, dpi = 300)

# --- Quantile regression analysis --- 
# Prepare data for quantile regression
qr_data <- data_analysis %>% 
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
cat("\n=== Quantile Regression Results (Median) ===\n")
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
# Add squared terms to panel data
panel_data$esg_score_squared <- panel_data$esg_score^2
panel_data$env_score_squared <- panel_data$environmental_pillar_score^2
panel_data$soc_score_squared <- panel_data$social_pillar_score^2
panel_data$gov_score_squared <- panel_data$governance_pillar_score^2

# Test for non-linear relationship with overall ESG score
fe_nonlinear_esg <- run_fe_model(
  cet1_ratio ~ esg_score + esg_score_squared + log_size + rwa_ratio + year_factor,
  panel_data
)
cat("\n=== Non-linear ESG Model Results ===\n")
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
cat("\n=== Non-linear ESG Pillars Model Results ===\n")
print(fe_nonlinear_pillars$robust)

# Visualize the non-linear relationship if significant
if ("esg_score_squared" %in% rownames(coef(fe_nonlinear_esg$robust)) && 
    coef(fe_nonlinear_esg$robust)["esg_score_squared", "Pr(>|t|)"] < 0.05) {
  
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

# lagged effects
panel_data$esg_score_lag <- plm::lag(panel_data$esg_score, 1)
panel_data$env_score_lag <- plm::lag(panel_data$environmental_pillar_score, 1)
panel_data$soc_score_lag <- plm::lag(panel_data$social_pillar_score, 1)
panel_data$gov_score_lag <- plm::lag(panel_data$governance_pillar_score, 1)

# Function to test lagged effects
test_lagged_effects <- function(panel_data) {
  fin_metrics <- c(
    "cet1_ratio", "tier1_ratio", "total_capital_ratio",
    "leverage_ratio", "equity_to_assets", "loan_to_assets", "provisions_ratio"
  )
  
  lag_results <- list()
  lag_summary <- data.frame(
    financial_metric = character(),
    model_type = character(),
    variable = character(),
    coefficient = numeric(),
    std_error = numeric(),
    t_value = numeric(),
    p_value = numeric()
  )
  
  for (metric in fin_metrics) {
    # Overall ESG lag specification
    f1 <- as.formula(
      paste(metric, "~ esg_score_lag + log_size + loan_quality + liquidity_ratio")
    )
    model_overall <- plm(f1, data = panel_data, model = "within", effect = "twoways")
    robust_se_overall <- vcovHC(model_overall, method = "arellano", cluster = "group")
    robust_results_overall <- coeftest(model_overall, robust_se_overall)
    
    # Pillar-specific lag specification
    f2 <- as.formula(
      paste(metric, "~ env_score_lag + soc_score_lag + gov_score_lag + log_size + loan_quality + liquidity_ratio")
    )
    model_pillars <- plm(f2, data = panel_data, model = "within", effect = "twoways")
    robust_se_pillars <- vcovHC(model_pillars, method = "arellano", cluster = "group")
    robust_results_pillars <- coeftest(model_pillars, robust_se_pillars)
    
    lag_results[[paste(metric, "overall")]] <- model_overall
    lag_results[[paste(metric, "pillars")]] <- model_pillars
    
    # Add overall ESG lag to summary
    if ("esg_score_lag" %in% rownames(robust_results_overall)) {
      lag_summary <- rbind(lag_summary, data.frame(
        financial_metric = metric,
        model_type = "overall",
        variable = "esg_score_lag",
        coefficient = robust_results_overall["esg_score_lag", "Estimate"],
        std_error = robust_results_overall["esg_score_lag", "Std. Error"],
        t_value = robust_results_overall["esg_score_lag", "t value"],
        p_value = robust_results_overall["esg_score_lag", "Pr(>|t|)"]
      ))
    }
    
    # Add pillar lags to summary
    for (var in c("env_score_lag", "soc_score_lag", "gov_score_lag")) {
      if (var %in% rownames(robust_results_pillars)) {
        lag_summary <- rbind(lag_summary, data.frame(
          financial_metric = metric,
          model_type = "pillars",
          variable = var,
          coefficient = robust_results_pillars[var, "Estimate"],
          std_error = robust_results_pillars[var, "Std. Error"],
          t_value = robust_results_pillars[var, "t value"],
          p_value = robust_results_pillars[var, "Pr(>|t|)"]
        ))
      }
    }
  }
  
  return(list(models = lag_results, summary = lag_summary))
}

# Run lagged effects analysis
lag_results <- test_lagged_effects(panel_data)

# Print summary of significant lagged effects
significant_lag_results <- lag_results$summary[lag_results$summary$p_value < 0.1, ]
cat("\n=== Significant Lagged Effects (p<0.1) ===\n")
print(significant_lag_results[order(significant_lag_results$p_value), ])

# Visualize significant lagged effects
if(nrow(significant_lag_results) > 0) {
  p_lag <- ggplot(significant_lag_results, 
                  aes(x = reorder(paste(variable, financial_metric, sep = " → "), p_value), 
                      y = coefficient, fill = p_value)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Significant Lagged Effects of ESG on Financial Metrics",
         x = "ESG Variable → Financial Metric",
         y = "Coefficient") +
    theme_minimal() +
    scale_fill_gradient(low = "darkblue", high = "lightblue")
  
  ggsave("plots/regression/lagged_effects.png", p_lag, width = 10, height = 6, dpi = 300)
}

# PCA
esg_vars <- data_analysis %>%
  select(matches("score")) %>%
  # Keep only numeric variables
  select_if(is.numeric) %>%
  # Remove squared terms
  select(-contains("squared"))

# Check if we have enough ESG variables for PCA
if(ncol(esg_vars) >= 5) {
  # Run PCA with varimax rotation
  pca_result <- principal(esg_vars, nfactors = min(3, ncol(esg_vars) - 1), rotate = "varimax")
  print(pca_result$loadings)
  
  # Create a loading plot to visualize PCA components
  pca_loadings <- as.data.frame(unclass(pca_result$loadings))
  pca_loadings$variable <- rownames(pca_loadings)
  
  # Create a long format for ggplot
  pca_long <- reshape2::melt(pca_loadings, id.vars = "variable", 
                             variable.name = "component", value.name = "loading")
  
  # Plot the loadings
  p_pca <- ggplot(pca_long, aes(x = variable, y = loading, fill = component)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "PCA Component Loadings",
         x = "ESG Variables",
         y = "Loading Value") +
    theme_minimal()
  
  ggsave("plots/regression/pca_loadings.png", p_pca, width = 10, height = 6, dpi = 300)
  
  # Extract and add factor scores to data_analysis
  pca_scores <- as.data.frame(pca_result$scores)
  data_analysis$esg_factor1 <- pca_scores$RC1
  data_analysis$esg_factor2 <- pca_scores$RC2
  if(ncol(pca_scores) >= 3) data_analysis$esg_factor3 <- pca_scores$RC3
  
  # Update panel data
  panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))
  
  # Interpret the factors based on loadings
  cat("\n=== PCA Factor Interpretation ===\n")
  # Extract top loadings for each factor
  factor_interpretation <- list()
  for (i in 1:min(3, ncol(pca_scores))) {
    component_name <- paste0("RC", i)
    loadings <- pca_loadings[, c("variable", component_name)]
    loadings <- loadings[order(abs(loadings[[component_name]]), decreasing = TRUE), ]
    top_vars <- head(loadings, 5)
    
    cat(paste0("\nFactor ", i, " (", component_name, ") - Top contributors:\n"))
    for (j in 1:nrow(top_vars)) {
      cat(paste0("- ", top_vars$variable[j], ": ", round(top_vars[[component_name]][j], 3), "\n"))
    }
    
    # Assign a meaningful name to each factor based on top loadings
    if (i == 1) {
      factor_name <- "Social and Community Factor"
    } else if (i == 2) {
      factor_name <- "Environmental Factor"
    } else {
      factor_name <- "Governance Factor"
    }
    
    cat(paste0("Interpretation: ", factor_name, "\n"))
    factor_interpretation[[i]] <- factor_name
  }
  
  # Run panel regression using the PCA factors
  run_pca_regression <- function(panel_data) {
    fin_metrics <- c(
      "cet1_ratio", "tier1_ratio", "total_capital_ratio",
      "leverage_ratio", "equity_to_assets", "loan_to_assets", "provisions_ratio"
    )
    
    pca_models <- list()
    pca_summary <- data.frame(
      financial_metric = character(),
      factor = character(),
      factor_name = character(),
      coefficient = numeric(),
      std_error = numeric(),
      t_value = numeric(),
      p_value = numeric()
    )
    
    for (metric in fin_metrics) {
      # Create formula based on available factors
      factors <- names(panel_data)[grepl("esg_factor", names(panel_data))]
      if(length(factors) > 0) {
        formula_str <- paste(metric, "~", paste(factors, collapse = " + "), 
                             "+ log_size + loan_quality + liquidity_ratio")
        formula <- as.formula(formula_str)
        
        model <- plm(formula, data = panel_data, model = "within", effect = "twoways")
        robust_se <- vcovHC(model, method = "arellano", cluster = "group")
        robust_results <- coeftest(model, robust_se)
        
        pca_models[[metric]] <- list(model = model, robust_results = robust_results)
        
        # Extract factor coefficients
        for (factor in factors) {
          if (factor %in% rownames(robust_results)) {
            factor_num <- as.numeric(gsub("esg_factor", "", factor))
            pca_summary <- rbind(pca_summary, data.frame(
              financial_metric = metric,
              factor = factor,
              factor_name = factor_interpretation[[factor_num]],
              coefficient = robust_results[factor, "Estimate"],
              std_error = robust_results[factor, "Std. Error"],
              t_value = robust_results[factor, "t value"],
              p_value = robust_results[factor, "Pr(>|t|)"]
            ))
          }
        }
      }
    }
    
    return(list(models = pca_models, summary = pca_summary))
  }
  
  # Run PCA regression
  pca_reg_results <- run_pca_regression(panel_data)
  
  # Print significant PCA regression results
  significant_pca <- pca_reg_results$summary[pca_reg_results$summary$p_value < 0.1, ]
  cat("\n=== Significant PCA Regression Results (p<0.1) ===\n")
  print(significant_pca[order(significant_pca$p_value), ])
  
  # Visualize the factor impacts
  if(nrow(significant_pca) > 0) {
    p_pca_reg <- ggplot(significant_pca, 
                        aes(x = reorder(paste(factor_name, financial_metric, sep = " → "), p_value), 
                            y = coefficient, fill = p_value)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Significant ESG Factor Effects on Financial Metrics",
           x = "ESG Factor → Financial Metric",
           y = "Coefficient") +
      theme_minimal() +
      scale_fill_gradient(low = "darkgreen", high = "lightgreen")
    
    ggsave("plots/regression/pca_regression.png", p_pca_reg, width = 10, height = 6, dpi = 300)
  }
}

# Sensitivity analysis 
# 9.1 Complete Banks vs All Banks
# Identify banks with complete data (e.g., data available in all 4 years)
complete_banks <- data_analysis %>%
  group_by(lei_code) %>%
  summarize(n_years = n_distinct(year)) %>%
  filter(n_years == max(n_years)) %>%
  pull(lei_code)

data_complete <- data_analysis %>% filter(lei_code %in% complete_banks)
panel_data_complete <- pdata.frame(data_complete, index = c("lei_code", "year"))

# Function to compare models for complete vs all banks
compare_complete_vs_all <- function(formula_str, complete_data, all_data) {
  formula <- as.formula(formula_str)
  
  # Model for complete banks
  model_complete <- plm(formula, data = complete_data, model = "within", effect = "twoways")
  complete_robust_se <- vcovHC(model_complete, method = "arellano", cluster = "group")
  complete_results <- coeftest(model_complete, complete_robust_se)
  
  # Model for all banks
  model_all <- plm(formula, data = all_data, model = "within", effect = "twoways")
  all_robust_se <- vcovHC(model_all, method = "arellano", cluster = "group")
  all_results <- coeftest(model_all, all_robust_se)
  
  # Model with Driscoll-Kraay standard errors
  dk_robust_se_complete <- vcovSCC(model_complete, type = "HC0", maxlag = 2)
  dk_results_complete <- coeftest(model_complete, dk_robust_se_complete)
  
  dk_robust_se_all <- vcovSCC(model_all, type = "HC0", maxlag = 2)
  dk_results_all <- coeftest(model_all, dk_robust_se_all)
  
  return(list(
    complete = list(model = model_complete, robust = complete_results, dk = dk_results_complete),
    all = list(model = model_all, robust = all_results, dk = dk_results_all)
  ))
}

# Compare models for overall ESG score
cat("\n=== Sensitivity Analysis: Complete Banks vs All Banks ===\n")
main_formula <- "cet1_ratio ~ esg_score + log_size + rwa_ratio + year_factor"
comparison_main <- compare_complete_vs_all(main_formula, panel_data_complete, panel_data)

cat("\nComplete Banks - Standard Robust SEs:\n")
print(comparison_main$complete$robust)
cat("\nComplete Banks - Driscoll-Kraay SEs:\n")
print(comparison_main$complete$dk)
cat("\nAll Banks - Standard Robust SEs:\n")
print(comparison_main$all$robust)
cat("\nAll Banks - Driscoll-Kraay SEs:\n")
print(comparison_main$all$dk)

# Compare models for ESG pillars
pillars_formula <- "cet1_ratio ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_size + rwa_ratio + year_factor"
comparison_pillars <- compare_complete_vs_all(pillars_formula, panel_data_complete, panel_data)

cat("\nComplete Banks - Pillars Model:\n")
print(comparison_pillars$complete$robust)
cat("\nAll Banks - Pillars Model:\n")
print(comparison_pillars$all$robust)

# 9.2 Predicted Effect of ESG Score -----------

# Create a prediction grid over the range of ESG scores
esg_seq <- seq(min(panel_data$esg_score, na.rm = TRUE),
               max(panel_data$esg_score, na.rm = TRUE), length.out = 100)

# Calculate mean values for other covariates
mean_log_size <- mean(panel_data$log_size, na.rm = TRUE)
mean_rwa_ratio <- mean(panel_data$rwa_ratio, na.rm = TRUE)
mean_loan_quality <- mean(panel_data$loan_quality, na.rm = TRUE)
mean_liquidity <- mean(panel_data$liquidity_ratio, na.rm = TRUE)

# Prepare new data frame for prediction
pred_data <- data.frame(
  esg_score = esg_seq,
  log_size = mean_log_size,
  rwa_ratio = mean_rwa_ratio,
  loan_quality = mean_loan_quality,
  liquidity_ratio = mean_liquidity
)

# Since fixed effects models don't include an overall intercept, we compute predictions directly
coef_fe <- coef(comparison_main$all$model)
pred_data$pred_all <- coef_fe["esg_score"] * pred_data$esg_score +
  coef_fe["log_size"] * pred_data$log_size +
  coef_fe["rwa_ratio"] * pred_data$rwa_ratio

coef_fe_complete <- coef(comparison_main$complete$model)
pred_data$pred_complete <- coef_fe_complete["esg_score"] * pred_data$esg_score +
  coef_fe_complete["log_size"] * pred_data$log_size +
  coef_fe_complete["rwa_ratio"] * pred_data$rwa_ratio

# Plot the predicted effect of ESG Score on CET1 ratio
p_pred <- ggplot(pred_data) +
  geom_line(aes(x = esg_score, y = pred_all, color = "All Banks"), size = 1.2) +
  geom_line(aes(x = esg_score, y = pred_complete, color = "Complete Banks"), size = 1.2, linetype = "dashed") +
  labs(title = "Predicted Effect of ESG Score on CET1 Ratio",
       subtitle = "Comparison between All Banks and Banks with Complete Data",
       x = "ESG Score",
       y = "Predicted CET1 Ratio",
       color = "Sample") +
  theme_minimal() +
  scale_color_manual(values = c("All Banks" = "blue", "Complete Banks" = "red"))

ggsave("plots/regression/predicted_esg_effect.png", p_pred, width = 8, height = 6, dpi = 300)