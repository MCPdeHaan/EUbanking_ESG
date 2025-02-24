
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


