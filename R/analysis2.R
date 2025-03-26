library(plm); library(dplyr)

# Fixed effects of banks and years
panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

summary(plm(credit_risk ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_assets + equity_to_assets, 
              data = panel_data,
              model = "within",
              effect = "twoways"))

summary(plm(cet1_risk_exposure ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_assets + equity_to_assets, 
            data = panel_data,
            model = "within",
            effect = "twoways"))

summary(plm(tier1_risk_exposure ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_assets + equity_to_assets, 
            data = panel_data,
            model = "within",
            effect = "twoways"))

summary(plm(totalcap_risk_exposure ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_assets + equity_to_assets, 
            data = panel_data,
            model = "within",
            effect = "twoways"))

summary(plm(leverage_ratio ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_assets + equity_to_assets, 
            data = panel_data,
            model = "within",
            effect = "twoways"))

summary(plm(provisions ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_assets + equity_to_assets, 
            data = panel_data,
            model = "within",
            effect = "twoways"))

summary(plm(provisions_ratio ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_assets + equity_to_assets, 
            data = panel_data,
            model = "within",
            effect = "twoways"))

summary(plm(liquidity_ratio ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_assets + equity_to_assets, 
            data = panel_data,
            model = "within",
            effect = "twoways"))

summary(plm(rwa_ratio ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_assets + equity_to_assets, 
            data = panel_data,
            model = "within",
            effect = "twoways"))

# List of dependent variables to analyze
dependent_vars <- c("credit_risk", "cet1_risk_exposure", "tier1_risk_exposure", 
                    "totalcap_risk_exposure", "leverage_ratio", "provisions", 
                    "provisions_ratio", "liquidity_ratio", "rwa_ratio")

# Function to run quadratic models and store results
run_quadratic_models <- function(dep_var) {
  # Create lagged dependent variable name
  lag_var <- paste0("lag_", dep_var)
  
  # Add lagged variable to the dataset
  panel_data[[lag_var]] <- lag(panel_data[[dep_var]])
  
  # Rename the lagged variable to a common name
  panel_data$lag_dep <- panel_data[[lag_var]]
  
  # 1. Model with overall ESG score
  formula_esg <- as.formula(paste0(dep_var, " ~ lag_dep + esg_score + esg_score_squared + ",
                                   "log_assets + equity_to_assets"))
  
  model_esg <- plm(formula_esg, data = panel_data, model = "within", effect = "twoways")
  
  # 2. Model with separate pillar scores
  formula_pillars <- as.formula(paste0(dep_var, " ~ lag_dep + ",
                                       "environmental_pillar_score + environmental_pillar_score_squared + ",
                                       "social_pillar_score + social_pillar_score_squared + ",
                                       "governance_pillar_score + governance_pillar_score_squared + ",
                                       "log_assets + equity_to_assets"))
  
  model_pillars <- plm(formula_pillars, data = panel_data, model = "within", effect = "twoways")
  
  # Return models as a list
  return(list(
    model_esg = model_esg,
    model_pillars = model_pillars
  ))
}

# Run models for each dependent variable
results_list <- lapply(dependent_vars, run_quadratic_models)

# Name the results list with dependent variable names
names(results_list) <- dependent_vars

# Print summary of one set of models as an example
cat("Results for", dependent_vars[1], "models:\n\n")
cat("Model with overall ESG score:\n")
print(summary(results_list[[1]]$model_esg))

cat("\nModel with separate pillar scores:\n")
print(summary(results_list[[1]]$model_pillars))


# Load required packages
library(stargazer)    # For LaTeX/HTML tables (academic standard)
library(kableExtra)   # For enhanced RMarkdown tables
library(modelsummary) # For beautiful modern tables
library(dplyr)        # For data manipulation
library(broom)        # For tidying model outputs

# Define more descriptive names for variables
descriptive_labels <- c(
  "Credit\nRisk", "CET1 Risk\nExposure", "Tier1 Risk\nExposure", 
  "Total Capital\nRisk Exposure", "Leverage\nRatio", "Provisions", 
  "Provisions\nRatio", "Liquidity\nRatio", "RWA\nRatio"
)

# Extract models by type
esg_models <- lapply(results_list, function(x) x$model_esg)
names(esg_models) <- dependent_vars

pillar_models <- lapply(results_list, function(x) x$model_pillars)
names(pillar_models) <- dependent_vars

#-----------------------------------------------------------------
# MODELSUMMARY TABLES (MODERN, FLEXIBLE, BEAUTIFUL)
#-----------------------------------------------------------------

# Custom coefficient map for better variable names
coef_map_esg <- c(
  "lag_.*" = "Lagged Dependent Variable",
  "esg_score" = "ESG Score",
  "esg_score_squared" = "ESG Score²",
  "log_assets" = "Log Assets",
  "equity_to_assets" = "Equity to Assets"
)

coef_map_pillars <- c(
  "lag_.*" = "Lagged Dependent Variable",
  "environmental_pillar_score" = "Environmental Score",
  "environmental_pillar_score_squared" = "Environmental Score²",
  "social_pillar_score" = "Social Score",
  "social_pillar_score_squared" = "Social Score²",
  "governance_pillar_score" = "Governance Score",
  "governance_pillar_score_squared" = "Governance Score²",
  "log_assets" = "Log Assets",
  "equity_to_assets" = "Equity to Assets"
)

# Goodness-of-fit statistics to include
gof_map <- tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "Observations",  0,
  "r.squared", "R²",            3
)

# Create the ESG score model table
ms_esg_table <- modelsummary(
  esg_models,
  title = "Table 1: Effects of Overall ESG Score on Bank Risk Measures",
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  gof_map = gof_map,
  coef_map = coef_map_esg,
  fmt = 3,                # 3 decimal places
  output = "kableExtra",  # For better RMarkdown integration
  note = "Bank and year fixed effects included in all models. *p<0.05; **p<0.01; ***p<0.001",
  add_rows = tibble(
    term = c("Bank FE", "Year FE"),
    `Credit Risk` = c("Yes", "Yes"),
    `CET1 Risk Exposure` = c("Yes", "Yes"),
    `Tier1 Risk Exposure` = c("Yes", "Yes"),
    `Total Capital Risk Exposure` = c("Yes", "Yes"),
    `Leverage Ratio` = c("Yes", "Yes"),
    `Provisions` = c("Yes", "Yes"),
    `Provisions Ratio` = c("Yes", "Yes"),
    `Liquidity Ratio` = c("Yes", "Yes"),
    `RWA Ratio` = c("Yes", "Yes")
  )
) %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position"),
                full_width = FALSE,
                font_size = 8)

# Create the ESG pillar score model table
ms_pillar_table <- modelsummary(
  pillar_models,
  title = "Table 2: Effects of ESG Pillar Scores on Bank Risk Measures",
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  gof_map = gof_map,
  coef_map = coef_map_pillars,
  fmt = 3,                # 3 decimal places
  output = "kableExtra",  # For better RMarkdown integration
  note = "Bank and year fixed effects included in all models. *p<0.05; **p<0.01; ***p<0.001",
  add_rows = tibble(
    term = c("Bank FE", "Year FE"),
    `Credit Risk` = c("Yes", "Yes"),
    `CET1 Risk Exposure` = c("Yes", "Yes"),
    `Tier1 Risk Exposure` = c("Yes", "Yes"),
    `Total Capital Risk Exposure` = c("Yes", "Yes"),
    `Leverage Ratio` = c("Yes", "Yes"),
    `Provisions` = c("Yes", "Yes"),
    `Provisions Ratio` = c("Yes", "Yes"),
    `Liquidity Ratio` = c("Yes", "Yes"),
    `RWA Ratio` = c("Yes", "Yes")
  )
) %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position"),
                full_width = FALSE,
                font_size = 8)

# Save to files (HTML and LaTeX)
save_kable(ms_esg_table, "table1_esg_models_kable.tex")
save_kable(ms_pillar_table, "table2_pillar_models_kable.tex")







