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


# 1. Check for Serial Correlation and Heteroskedasticity

# Load required packages (if not already loaded)
library(plm)
library(lmtest)

# (a) Serial Correlation: Wooldridge test for panel data
serial_test <- pwartest(model_fe)
print("Serial Correlation Test (Wooldridge):")
print(serial_test)

# (b) Heteroskedasticity: Breusch-Pagan test
hetero_test <- bptest(model_fe)
print("Heteroskedasticity Test (Breusch-Pagan):")
print(hetero_test)


# 2. Sensitivity Analysis: Restrict the Sample to Banks with Complete Data

# Identify banks with data in all 4 years using the 'bank_distribution' data
# (Assuming bank_distribution is already available from your previous summary)
library(dplyr)
complete_banks <- bank_distribution %>% 
  filter(n_years == 4) %>% 
  pull(name)

# Subset the full data_analysis to include only these banks
data_complete <- data_analysis %>% 
  filter(name %in% complete_banks)

# Convert the subset to a panel data frame
panel_data_complete <- pdata.frame(data_complete, index = c("lei_code", "year"))

# Run the fixed effects model on the complete sample
model_fe_complete <- plm(common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
                           esg_score + log_assets + loan_quality + liquidity_ratio,
                         data = panel_data_complete, model = "within", effect = "twoways")

# Compute robust standard errors clustered by bank
fe_complete_robust_se <- vcovHC(model_fe_complete, method = "arellano", cluster = "group")

# Display the fixed effects model results with robust SEs for the complete sample
fe_complete_results <- coeftest(model_fe_complete, fe_complete_robust_se)
print("Fixed Effects Model on Complete Banks with Robust SEs:")
print(fe_complete_results)


# Load required packages
library(plm)
library(lmtest)

# Load required packages
library(plm)
library(lmtest)
library(sandwich)

library(dplyr)

# 1. Summary of the inner-joined dataset (banks with both ESG and financial data)
summary_inner <- data_analysis %>%
  summarise(
    total_banks = n_distinct(lei_code),
    total_years = n_distinct(year),
    total_obs   = n()
  )
print(summary_inner)

# 2. Distribution of observations by year
year_distribution <- data_analysis %>%
  group_by(year) %>%
  summarise(
    n_obs   = n(),
    n_banks = n_distinct(lei_code)
  ) %>%
  arrange(year)
print(year_distribution)

# 3. Count of years available per bank (to see if some banks have data for only a few years)
bank_distribution <- data_analysis %>%
  group_by(name) %>%
  summarise(n_years = n_distinct(year)) %>%
  arrange(desc(n_years))
print(bank_distribution)


# 1. Fixed Effects (Two-Way) Model with Clustered Robust SEs
model_fe <- plm(common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
                  esg_score + log_assets + loan_quality + liquidity_ratio,
                data = panel_data, model = "within", effect = "twoways")

# Compute robust standard errors clustered by bank (group)
fe_robust_se <- vcovHC(model_fe, method = "arellano", cluster = "group")

# Display the fixed effects model coefficients with robust SEs
fe_results <- coeftest(model_fe, fe_robust_se)
print("Fixed Effects Model with Robust SEs:")
print(fe_results)

# 2. Pooled OLS Model with Clustered Robust SEs
model_pool <- plm(common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition ~ 
                    esg_score + log_assets + loan_quality + liquidity_ratio,
                  data = panel_data, model = "pooling")

# Compute robust standard errors clustered by bank for pooled OLS
pool_robust_se <- vcovHC(model_pool, method = "arellano", cluster = "group")

# Display the pooled OLS model coefficients with robust SEs
pool_results <- coeftest(model_pool, pool_robust_se)
print("Pooled OLS Model with Robust SEs:")
print(pool_results)

# You might compare the magnitude and significance of the coefficients
# from both approaches to assess robustness.


# Compute Driscoll-Kraay robust SEs for the complete-sample FE model
dk_robust_se <- vcovSCC(model_fe_complete, type = "HC0", maxlag = 2)

# Re-run the coefficient tests with these SEs
dk_results <- coeftest(model_fe_complete, dk_robust_se)

# Print the results
print("Fixed Effects Model on Complete Banks with Driscoll-Kraay Robust SEs:")
print(dk_results)


# Create a sequence of ESG scores over its observed range
esg_seq <- seq(min(panel_data_complete$esg_score, na.rm = TRUE),
               max(panel_data_complete$esg_score, na.rm = TRUE), length.out = 100)

# Compute means of the other covariates
mean_log_assets    <- mean(panel_data_complete$log_assets, na.rm = TRUE)
mean_loan_quality  <- mean(panel_data_complete$loan_quality, na.rm = TRUE)
mean_liquidity     <- mean(panel_data_complete$liquidity_ratio, na.rm = TRUE)

# Create a new data frame for predictions
pred_data <- data.frame(
  esg_score      = esg_seq,
  log_assets     = mean_log_assets,
  loan_quality   = mean_loan_quality,
  liquidity_ratio= mean_liquidity
)

# Extract coefficients from the fixed effects model (note: FE models don’t have an intercept)
coef_fe <- coef(model_fe_complete)

# Compute predicted values from the FE model:
pred_data$pred <- coef_fe["esg_score"] * pred_data$esg_score +
  coef_fe["log_assets"] * pred_data$log_assets +
  coef_fe["loan_quality"] * pred_data$loan_quality +
  coef_fe["liquidity_ratio"] * pred_data$liquidity_ratio

# Load ggplot2 for plotting
library(ggplot2)

# Plot the predicted relationship between ESG score and the dependent variable
ggplot(pred_data, aes(x = esg_score, y = pred)) +
  geom_line(color = "blue", size = 1.2) +
  labs(title = "Predicted Effect of ESG Score on Common Equity Ratio",
       x = "ESG Score",
       y = "Predicted Common Equity Ratio") +
  theme_minimal()


