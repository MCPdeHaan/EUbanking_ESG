library(plm); library(lmtest); library(sandwich); library(dplyr)
library(psych); library(ggplot2); library(corrplot)

# Convert data_analysis to a panel data frame
panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

# Correlation analysis 
corr_vars <- data_analysis %>%
  select(esg_score, environmental_pillar_score, social_pillar_score, governance_pillar_score,
         common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         leverage_ratio_using_a_transitional_definition_of_tier_1_capital,
         equity_to_assets, loan_to_assets, provisions_ratio)

correlation_matrix <- cor(corr_vars, use = "pairwise.complete.obs")

# Visualize the correlation matrix
png("correlation_plot.png", width = 12, height = 10, units = "in", res = 300)
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7,
         title = "Correlation between ESG Scores and Financial Metrics")
dev.off()

# Testing Individual ESG Components
test_individual_components <- function(panel_data) {
  # Define ESG components and selected financial metrics
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
  
  # Prepare an empty results data frame to collect coefficient estimates and diagnostics
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
  
  # Loop over ESG components and financial metrics, running fixed effects regressions
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
significant_results <- component_results[component_results$p_value < 0.05, ]

# Print the top 20 most significant results
print(head(component_results, 20))
write.csv(component_results, "esg_component_financial_metric_results.csv", row.names = FALSE)



# Visualize top significant relationships
top_results <- head(component_results, 10)
top_results$significance <- ifelse(top_results$p_value < 0.01, "p < 0.01", 
                                   ifelse(top_results$p_value < 0.05, "p < 0.05", "p < 0.10"))

ggplot(top_results, aes(x = reorder(paste(esg_component, financial_metric, sep = " → "), coefficient), 
                        y = coefficient, fill = significance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top ESG Component Relationships with Financial Metrics",
       x = "ESG Component → Financial Metric",
       y = "Coefficient") +
  theme_minimal() +
  theme(legend.title = element_blank())

# export
ggsave("top_esg_component_relationships.png", width = 10, height = 6, units = "in", dpi = 300)

# 4. Analyze key ESG components with strongest effects
# Select top significant components for further analysis
top_components <- unique(head(significant_results, 10)$esg_component)

# Run more detailed models for each top component
detailed_models <- list()

for (component in top_components) {
  # Find the financial metric most strongly related to this component
  best_metric <- significant_results %>%
    filter(esg_component == component) %>%
    arrange(p_value) %>%
    pull(financial_metric) %>%
    head(1)
  
  if (length(best_metric) > 0) {
    form <- as.formula(paste(best_metric, "~", component, 
                             "+ log_assets + loan_quality + liquidity_ratio"))
    
    # Run fixed effects model with robust standard errors
    model <- plm(form, data = panel_data, model = "within", effect = "twoways")
    robust_se <- vcovHC(model, method = "arellano", cluster = "group")
    robust_results <- coeftest(model, robust_se)
    
    detailed_models[[component]] <- list(
      component = component,
      metric = best_metric,
      model = model,
      robust_results = robust_results
    )
  }
}

# Print detailed results for each top component
for (component in names(detailed_models)) {
  cat("Component:", component, "\n")
  cat("Financial Metric:", detailed_models[[component]]$metric, "\n")
  print(detailed_models[[component]]$robust_results)
}

# 5. Lagged Effects of ESG
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
    "leverage_ratio_using_a_transitional_definition_of_tier_1_capital",
    "equity_to_assets", "loan_to_assets", "provisions_ratio"
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
      paste(metric, "~ lag1_esg_score + log_assets + loan_quality + liquidity_ratio")
    )
    model_overall <- plm(f1, data = panel_data, model = "within", effect = "twoways")
    robust_se_overall <- vcovHC(model_overall, method = "arellano", cluster = "group")
    robust_results_overall <- coeftest(model_overall, robust_se_overall)
    
    # Pillar-specific lag specification
    f2 <- as.formula(
      paste(metric, "~ lag1_env_score + lag1_soc_score + lag1_gov_score + log_assets + loan_quality + liquidity_ratio")
    )
    model_pillars <- plm(f2, data = panel_data, model = "within", effect = "twoways")
    robust_se_pillars <- vcovHC(model_pillars, method = "arellano", cluster = "group")
    robust_results_pillars <- coeftest(model_pillars, robust_se_pillars)
    
    lag_results[[paste(metric, "overall")]] <- model_overall
    lag_results[[paste(metric, "pillars")]] <- model_pillars
    
    # Add to summary dataframe - FIXED SECTION
    overall_rows <- rownames(robust_results_overall)
    for (i in 1:nrow(robust_results_overall)) {
      if (!is.na(overall_rows[i]) && overall_rows[i] == "lag1_esg_score") {
        lag_summary <- rbind(lag_summary, data.frame(
          financial_metric = metric,
          model_type = "overall",
          variable = "lag1_esg_score",
          coefficient = robust_results_overall[i, "Estimate"],
          std_error = robust_results_overall[i, "Std. Error"],
          t_value = robust_results_overall[i, "t value"],
          p_value = robust_results_overall[i, "Pr(>|t|)"]
        ))
      }
    }
    
    pillar_rows <- rownames(robust_results_pillars)
    for (i in 1:nrow(robust_results_pillars)) {
      if (!is.na(pillar_rows[i]) && pillar_rows[i] %in% c("lag1_env_score", "lag1_soc_score", "lag1_gov_score")) {
        lag_summary <- rbind(lag_summary, data.frame(
          financial_metric = metric,
          model_type = "pillars",
          variable = pillar_rows[i],
          coefficient = robust_results_pillars[i, "Estimate"],
          std_error = robust_results_pillars[i, "Std. Error"],
          t_value = robust_results_pillars[i, "t value"],
          p_value = robust_results_pillars[i, "Pr(>|t|)"]
        ))
      }
    }
  }
  
  return(list(models = lag_results, summary = lag_summary))
}

lag_results <- test_lagged_effects(panel_data)

# Print summary of lagged effects
significant_lag_results <- lag_results$summary[lag_results$summary$p_value < 0.1, ]
print(significant_lag_results[order(significant_lag_results$p_value), ])

# Create a visualization of significant lagged effects
ggplot(significant_lag_results, 
       aes(x = reorder(paste(variable, financial_metric, sep = " → "), p_value), 
           y = coefficient, fill = p_value)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Significant Lagged Effects of ESG on Financial Metrics",
       x = "ESG Variable → Financial Metric",
       y = "Coefficient") +
  theme_minimal() +
  scale_fill_gradient(low = "darkblue", high = "lightblue")

# Test if there are differences in lag effects between different financial metrics
# using ANOVA
lag_anova <- aov(coefficient ~ financial_metric, data = lag_results$summary)
print(summary(lag_anova))

# IV. Non-linear (Quadratic) Specifications
# Add quadratic terms for overall ESG and ESG pillars

# Add squared terms to data_analysis and update panel_data accordingly
data_analysis <- data_analysis %>%
  mutate(
    esg_score_squared = esg_score^2,
    env_score_squared = environmental_pillar_score^2,
    soc_score_squared = social_pillar_score^2,
    gov_score_squared = governance_pillar_score^2
  )
panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

# Create a function to test non-linear relationships for all financial metrics
test_nonlinear_relationships <- function(panel_data) {
  fin_metrics <- c(
    "common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
    "tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
    "total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition",
    "leverage_ratio_using_a_transitional_definition_of_tier_1_capital",
    "equity_to_assets", "loan_to_assets", "provisions_ratio"
  )
  
  nl_results <- list()
  nl_summary <- data.frame(
    financial_metric = character(),
    model_type = character(),
    has_nonlinearity = logical(),
    linear_term_coef = numeric(),
    linear_term_p = numeric(),
    squared_term_coef = numeric(),
    squared_term_p = numeric(),
    r_squared = numeric()
  )
  
  for (metric in fin_metrics) {
    # Overall ESG quadratic specification
    f1 <- as.formula(paste(metric, "~ esg_score + esg_score_squared + log_assets + loan_quality + liquidity_ratio"))
    model_nl1 <- plm(f1, data = panel_data, model = "within", effect = "twoways")
    
    # Extract key information
    sm1 <- summary(model_nl1)
    has_nl_overall <- FALSE
    if ("esg_score_squared" %in% rownames(sm1$coefficients)) {
      has_nl_overall <- sm1$coefficients["esg_score_squared", "Pr(>|t|)"] < 0.1
    }
    
    nl_summary <- rbind(nl_summary, data.frame(
      financial_metric = metric,
      model_type = "overall_esg",
      has_nonlinearity = has_nl_overall,
      linear_term_coef = sm1$coefficients["esg_score", "Estimate"],
      linear_term_p = sm1$coefficients["esg_score", "Pr(>|t|)"],
      squared_term_coef = sm1$coefficients["esg_score_squared", "Estimate"],
      squared_term_p = sm1$coefficients["esg_score_squared", "Pr(>|t|)"],
      r_squared = sm1$r.squared
    ))
    
    # ESG Pillars quadratic specification
    f2 <- as.formula(paste(metric, "~ environmental_pillar_score + env_score_squared + social_pillar_score + soc_score_squared + governance_pillar_score + gov_score_squared + log_assets + loan_quality + liquidity_ratio"))
    model_nl2 <- plm(f2, data = panel_data, model = "within", effect = "twoways")
    sm2 <- summary(model_nl2)
    
    # Check for non-linearity in each pillar
    env_nl <- FALSE
    soc_nl <- FALSE
    gov_nl <- FALSE
    
    if ("env_score_squared" %in% rownames(sm2$coefficients)) {
      env_nl <- sm2$coefficients["env_score_squared", "Pr(>|t|)"] < 0.1
    }
    if ("soc_score_squared" %in% rownames(sm2$coefficients)) {
      soc_nl <- sm2$coefficients["soc_score_squared", "Pr(>|t|)"] < 0.1
    }
    if ("gov_score_squared" %in% rownames(sm2$coefficients)) {
      gov_nl <- sm2$coefficients["gov_score_squared", "Pr(>|t|)"] < 0.1
    }
    
    # Add environmental pillar results
    nl_summary <- rbind(nl_summary, data.frame(
      financial_metric = metric,
      model_type = "environmental",
      has_nonlinearity = env_nl,
      linear_term_coef = sm2$coefficients["environmental_pillar_score", "Estimate"],
      linear_term_p = sm2$coefficients["environmental_pillar_score", "Pr(>|t|)"],
      squared_term_coef = sm2$coefficients["env_score_squared", "Estimate"],
      squared_term_p = sm2$coefficients["env_score_squared", "Pr(>|t|)"],
      r_squared = sm2$r.squared
    ))
    
    # Add social pillar results
    nl_summary <- rbind(nl_summary, data.frame(
      financial_metric = metric,
      model_type = "social",
      has_nonlinearity = soc_nl,
      linear_term_coef = sm2$coefficients["social_pillar_score", "Estimate"],
      linear_term_p = sm2$coefficients["social_pillar_score", "Pr(>|t|)"],
      squared_term_coef = sm2$coefficients["soc_score_squared", "Estimate"],
      squared_term_p = sm2$coefficients["soc_score_squared", "Pr(>|t|)"],
      r_squared = sm2$r.squared
    ))
    
    # Add governance pillar results
    nl_summary <- rbind(nl_summary, data.frame(
      financial_metric = metric,
      model_type = "governance",
      has_nonlinearity = gov_nl,
      linear_term_coef = sm2$coefficients["governance_pillar_score", "Estimate"],
      linear_term_p = sm2$coefficients["governance_pillar_score", "Pr(>|t|)"],
      squared_term_coef = sm2$coefficients["gov_score_squared", "Estimate"],
      squared_term_p = sm2$coefficients["gov_score_squared", "Pr(>|t|)"],
      r_squared = sm2$r.squared
    ))
    
    nl_results[[paste(metric, "overall")]] <- model_nl1
    nl_results[[paste(metric, "pillars")]] <- model_nl2
  }
  
  return(list(models = nl_results, summary = nl_summary))
}

nonlinear_results <- test_nonlinear_relationships(panel_data)

# Find cases with significant non-linearity
significant_nonlinear <- nonlinear_results$summary[nonlinear_results$summary$squared_term_p < 0.1, ]
print(significant_nonlinear[order(significant_nonlinear$squared_term_p), ])

# Visualize the non-linear relationships for significant cases
if (nrow(significant_nonlinear) > 0) {
  for (i in 1:min(nrow(significant_nonlinear), 3)) {  # Visualize up to 3 cases
    row <- significant_nonlinear[i, ]
    metric <- row$financial_metric
    model_type <- row$model_type
    
    # Create prediction data
    if (model_type == "overall_esg") {
      var_name <- "esg_score"
    } else if (model_type == "environmental") {
      var_name <- "environmental_pillar_score"
    } else if (model_type == "social") {
      var_name <- "social_pillar_score"
    } else {
      var_name <- "governance_pillar_score"
    }
    
    var_range <- seq(min(data_analysis[[var_name]], na.rm = TRUE), 
                     max(data_analysis[[var_name]], na.rm = TRUE), length.out = 100)
    
    # Calculate predicted values
    pred_df <- data.frame(x = var_range)
    pred_df$y <- row$linear_term_coef * var_range + row$squared_term_coef * var_range^2
    
    # Normalize for plotting
    pred_df$y <- pred_df$y - min(pred_df$y) + min(data_analysis[[metric]], na.rm = TRUE)
    
    # Create the plot
    plot_title <- paste("Non-linear Relationship:", model_type, "→", metric)
    p <- ggplot() +
      geom_line(data = pred_df, aes(x = x, y = y), size = 1.2, color = "blue") +
      labs(title = plot_title,
           x = var_name,
           y = metric) +
      theme_minimal()
    
    print(p)
    ggsave(paste0("nonlinear_", model_type, "_", i, ".png"), p, width = 8, height = 6)
  }
}

# ------------------------------
# V. Principal Component Analysis (PCA)
# ------------------------------
# Select ESG-related variables for PCA
esg_vars <- data_analysis %>%
  select(
    esg_score, environmental_pillar_score, social_pillar_score, governance_pillar_score,
    resource_use_score, emissions_score, environmental_innovation_score,
    workforce_score, human_rights_score, community_score, product_responsibility_score,
    management_score, shareholders_score, csr_strategy_score
  )

# Run PCA with varimax rotation; here we extract 3 factors as an example.
pca_result <- principal(esg_vars, nfactors = 3, rotate = "varimax")
print(pca_result$loadings)

# Create a loading plot to visualize PCA components
pca_loadings <- as.data.frame(unclass(pca_result$loadings))
pca_loadings$variable <- rownames(pca_loadings)

# Create a long format for ggplot
pca_long <- reshape2::melt(pca_loadings, id.vars = "variable", 
                           variable.name = "component", value.name = "loading")

# Plot the loadings
ggplot(pca_long, aes(x = variable, y = loading, fill = component)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "PCA Component Loadings",
       x = "ESG Variables",
       y = "Loading Value") +
  theme_minimal()

# Extract and add factor scores to data_analysis
pca_scores <- as.data.frame(pca_result$scores)
data_analysis$esg_factor1 <- pca_scores$RC1
data_analysis$esg_factor2 <- pca_scores$RC2
data_analysis$esg_factor3 <- pca_scores$RC3
panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

# Interpret the factors based on loadings
cat("\n=== PCA Factor Interpretation ===\n")
# Extract top loadings for each factor
factor_interpretation <- list()
for (i in 1:3) {
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

# Run panel regression using the PCA factors for each financial metric
pca_regression_results <- function(panel_data) {
  fin_metrics <- c(
    "common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
    "tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition",
    "total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition",
    "leverage_ratio_using_a_transitional_definition_of_tier_1_capital",
    "equity_to_assets", "loan_to_assets", "provisions_ratio"
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
    formula <- as.formula(paste(metric, "~ esg_factor1 + esg_factor2 + esg_factor3 + log_assets + loan_quality + liquidity_ratio"))
    model <- plm(formula, data = panel_data, model = "within", effect = "twoways")
    robust_se <- vcovHC(model, method = "arellano", cluster = "group")
    robust_results <- coeftest(model, robust_se)
    
    pca_models[[metric]] <- list(model = model, robust_results = robust_results)
    
    # Extract factor coefficients
    for (factor in c("esg_factor1", "esg_factor2", "esg_factor3")) {
      factor_idx <- which(rownames(robust_results) == factor)
      if (length(factor_idx) > 0) {
        factor_num <- as.numeric(gsub("esg_factor", "", factor))
        pca_summary <- rbind(pca_summary, data.frame(
          financial_metric = metric,
          factor = factor,
          factor_name = factor_interpretation[[factor_num]],
          coefficient = robust_results[factor_idx, "Estimate"],
          std_error = robust_results[factor_idx, "Std. Error"],
          t_value = robust_results[factor_idx, "t value"],
          p_value = robust_results[factor_idx, "Pr(>|t|)"]
        ))
      }
    }
  }
  
  return(list(models = pca_models, summary = pca_summary))
}

pca_reg_results <- pca_regression_results(panel_data)

# Print significant PCA regression results
significant_pca <- pca_reg_results$summary[pca_reg_results$summary$p_value < 0.1, ]
print(significant_pca[order(significant_pca$p_value), ])

# Visualize the factor impacts
ggplot(significant_pca, aes(x = reorder(paste(factor_name, financial_metric, sep = " → "), p_value), 
                            y = coefficient, fill = p_value)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Significant ESG Factor Effects on Financial Metrics",
       x = "ESG Factor → Financial Metric",
       y = "Coefficient") +
  theme_minimal() +
  scale_fill_gradient(low = "darkgreen", high = "lightgreen")



# ------------------------------
# IX. Predicted Effect of ESG Score and Visualization
# ------------------------------
# Create a prediction grid over the range of ESG scores in the complete sample
esg_seq <- seq(min(panel_data_complete$esg_score, na.rm = TRUE),
               max(panel_data_complete$esg_score, na.rm = TRUE), length.out = 100)

# Calculate mean values for other covariates
mean_log_assets   <- mean(panel_data_complete$log_assets, na.rm = TRUE)
mean_loan_quality <- mean(panel_data_complete$loan_quality, na.rm = TRUE)
mean_liquidity    <- mean(panel_data_complete$liquidity_ratio, na.rm = TRUE)

# Prepare new data frame for prediction
pred_data <- data.frame(
  esg_score       = esg_seq,
  log_assets      = mean_log_assets,
  loan_quality    = mean_loan_quality,
  liquidity_ratio = mean_liquidity
)

# Since fixed effects models do not include an overall intercept, predictions are computed directly
coef_fe <- coef(model_fe_complete)
pred_data$pred <- coef_fe["esg_score"] * pred_data$esg_score +
  coef_fe["log_assets"] * pred_data$log_assets +
  coef_fe["loan_quality"] * pred_data$loan_quality +
  coef_fe["liquidity_ratio"] * pred_data$liquidity_ratio

# Plot the predicted effect of ESG Score on the common equity ratio
pred_plot <- ggplot(pred_data, aes(x = esg_score, y = pred)) +
  geom_line(color = "blue", size = 1.2) +
  labs(title = "Predicted Effect of ESG Score on Common Equity Ratio",
       x = "ESG Score",
       y = "Predicted Common Equity Ratio") +
  theme_minimal()
print(pred_plot)

ggsave("predicted_esg_effect.png", plot = pred_plot, width = 8, height = 6)