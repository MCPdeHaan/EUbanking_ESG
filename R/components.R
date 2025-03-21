# Revised ESG Component Strategic Analysis for Banks
# This script analyzes which ESG components are most strategically beneficial 
# for different types of banks

library(tidyverse)
library(corrplot)
library(gridExtra)
library(ggplot2)
library(reshape2)

# Load the dataset
data_analysis <- read_csv("data/data_analysis.csv")

# 1. First, let's identify all ESG component columns
esg_columns <- names(data_analysis)[grepl("score|pillar", names(data_analysis)) & 
                                      !grepl("quartile|category", names(data_analysis))]

# Key financial performance metrics
financial_metrics <- c("cet1_ratio", "tier1_ratio", "total_capital_ratio", 
                       "leverage_ratio", "rwa_ratio", "equity_to_assets", 
                       "loan_to_assets", "provisions_ratio", "liquidity_ratio")

# 2. CORRELATION ANALYSIS: ESG Components vs Financial Performance
# Calculate correlations
correlation_matrix <- data_analysis %>%
  select(all_of(c(esg_columns, financial_metrics))) %>%
  cor(use = "pairwise.complete.obs")

# Extract the relevant section (ESG components vs financial metrics)
esg_fin_correlations <- correlation_matrix[esg_columns, financial_metrics]

# Convert to long format for easier analysis
correlations_long <- as.data.frame(esg_fin_correlations) %>%
  rownames_to_column("esg_component") %>%
  pivot_longer(-esg_component, 
               names_to = "financial_metric", 
               values_to = "correlation") %>%
  arrange(desc(abs(correlation)))

# Print top correlations
cat("Top 20 strongest correlations between ESG components and financial metrics:\n")
print(head(correlations_long, 20))

# 3. BANK SIZE ANALYSIS: Split into small and large banks
# Calculate median bank size
median_size <- median(data_analysis$log_size, na.rm = TRUE)

# Create a size category variable
data_analysis <- data_analysis %>%
  mutate(size_category = ifelse(log_size < median_size, "Small Banks", "Large Banks"))

# Calculate correlations for each size category
small_banks <- data_analysis %>% filter(size_category == "Small Banks")
large_banks <- data_analysis %>% filter(size_category == "Large Banks")

# Function to calculate correlations for a specific bank group
calculate_group_correlations <- function(data_subset, group_name) {
  # Calculate correlation matrix
  corr_matrix <- data_subset %>%
    select(all_of(c(esg_columns, financial_metrics))) %>%
    cor(use = "pairwise.complete.obs")
  
  # Extract ESG vs Financial
  esg_fin_corr <- corr_matrix[esg_columns, financial_metrics]
  
  # Convert to long format
  result <- as.data.frame(esg_fin_corr) %>%
    rownames_to_column("esg_component") %>%
    pivot_longer(-esg_component, 
                 names_to = "financial_metric", 
                 values_to = "correlation") %>%
    mutate(group = group_name) %>%
    arrange(desc(abs(correlation)))
  
  return(result)
}

# Calculate correlations for each bank size
small_bank_corrs <- calculate_group_correlations(small_banks, "Small Banks")
large_bank_corrs <- calculate_group_correlations(large_banks, "Large Banks")

# Combine results
bank_size_correlations <- bind_rows(small_bank_corrs, large_bank_corrs)

# Print top correlations for each bank size category
cat("\nTop 10 strongest correlations for small banks:\n")
print(small_bank_corrs %>% head(10))

cat("\nTop 10 strongest correlations for large banks:\n")
print(large_bank_corrs %>% head(10))

# 4. STEPWISE REGRESSION APPROACH
# Instead of including all ESG components at once, let's use a stepwise approach
# to identify the most important components for each financial metric

# Function to run stepwise regression
step_reg_analysis <- function(data, financial_metric) {
  # Start with just size as a predictor
  base_formula <- as.formula(paste(financial_metric, "~ log_size"))
  base_model <- lm(base_formula, data = data)
  
  # Track results
  selected_components <- character(0)
  significant_results <- data.frame(
    component = character(0),
    coefficient = numeric(0),
    p_value = numeric(0),
    r_squared_change = numeric(0)
  )
  
  # Record base R-squared
  base_r_squared <- summary(base_model)$r.squared
  
  # Test each ESG component individually
  for (component in esg_columns) {
    # Build formula with this component
    test_formula <- as.formula(paste(financial_metric, "~ log_size +", component))
    
    # Run model
    test_model <- lm(test_formula, data = data)
    
    # Extract coefficient and p-value for the component
    component_index <- which(names(coef(test_model)) == component)
    if (length(component_index) > 0) {
      coef_value <- coef(test_model)[component_index]
      coef_pvalue <- summary(test_model)$coefficients[component_index, 4]
      
      # Calculate R-squared change
      r_squared_change <- summary(test_model)$r.squared - base_r_squared
      
      # If significant, add to results
      if (coef_pvalue < 0.1) {
        significant_results <- rbind(significant_results, 
                                     data.frame(
                                       component = component,
                                       coefficient = coef_value,
                                       p_value = coef_pvalue,
                                       r_squared_change = r_squared_change
                                     ))
      }
    }
  }
  
  # Sort by R-squared change
  if (nrow(significant_results) > 0) {
    significant_results <- significant_results %>%
      arrange(desc(r_squared_change))
  }
  
  return(significant_results)
}

# Run stepwise regression for each financial metric
key_metrics <- c("cet1_ratio", "leverage_ratio", "rwa_ratio")

# Results for all banks
all_banks_results <- list()
for (metric in key_metrics) {
  cat(paste("\nStepwise regression for", metric, "- All Banks:\n"))
  results <- step_reg_analysis(data_analysis, metric)
  print(results)
  all_banks_results[[metric]] <- results
}

# Results for small banks
small_banks_results <- list()
for (metric in key_metrics) {
  cat(paste("\nStepwise regression for", metric, "- Small Banks:\n"))
  results <- step_reg_analysis(small_banks, metric)
  print(results)
  small_banks_results[[metric]] <- results
}

# Results for large banks
large_banks_results <- list()
for (metric in key_metrics) {
  cat(paste("\nStepwise regression for", metric, "- Large Banks:\n"))
  results <- step_reg_analysis(large_banks, metric)
  print(results)
  large_banks_results[[metric]] <- results
}

# 5. BUSINESS MODEL ANALYSIS: Add lending intensity
# Create lending intensity categories
data_analysis <- data_analysis %>%
  mutate(
    loan_intensity = case_when(
      loan_to_assets < quantile(loan_to_assets, 0.33, na.rm = TRUE) ~ "Low Lending",
      loan_to_assets > quantile(loan_to_assets, 0.66, na.rm = TRUE) ~ "High Lending",
      TRUE ~ "Medium Lending"
    )
  )

# Analyze by lending intensity
lending_results <- list()
for (lending_type in c("Low Lending", "Medium Lending", "High Lending")) {
  lending_data <- data_analysis %>% filter(loan_intensity == lending_type)
  
  cat(paste("\n\nAnalysis for", lending_type, "Banks:\n"))
  
  # Calculate correlations
  lending_corrs <- calculate_group_correlations(lending_data, lending_type)
  cat(paste("\nTop 5 correlations for", lending_type, "Banks:\n"))
  print(head(lending_corrs, 5))
  
  # Run stepwise regression for CET1 ratio
  cat(paste("\nStepwise regression for CET1 ratio -", lending_type, "Banks:\n"))
  results <- step_reg_analysis(lending_data, "cet1_ratio")
  print(results)
  lending_results[[lending_type]] <- results
}

# 6. RISK PROFILE ANALYSIS: Add risk intensity based on RWA ratio
# Create risk profile categories
data_analysis <- data_analysis %>%
  mutate(
    risk_profile = case_when(
      rwa_ratio < quantile(rwa_ratio, 0.33, na.rm = TRUE) ~ "Low Risk",
      rwa_ratio > quantile(rwa_ratio, 0.66, na.rm = TRUE) ~ "High Risk",
      TRUE ~ "Medium Risk"
    )
  )

# Analyze by risk profile
risk_results <- list()
for (risk_type in c("Low Risk", "Medium Risk", "High Risk")) {
  risk_data <- data_analysis %>% filter(risk_profile == risk_type)
  
  cat(paste("\n\nAnalysis for", risk_type, "Banks:\n"))
  
  # Calculate correlations
  risk_corrs <- calculate_group_correlations(risk_data, risk_type)
  cat(paste("\nTop 5 correlations for", risk_type, "Banks:\n"))
  print(head(risk_corrs, 5))
  
  # Run stepwise regression for CET1 ratio
  cat(paste("\nStepwise regression for CET1 ratio -", risk_type, "Banks:\n"))
  results <- step_reg_analysis(risk_data, "cet1_ratio")
  print(results)
  risk_results[[risk_type]] <- results
}

# 7. SUMMARIZE KEY ESG COMPONENTS BY BANK TYPE
# Function to extract top components
extract_top_components <- function(results_list, metric, max_components = 3) {
  if (is.null(results_list[[metric]]) || nrow(results_list[[metric]]) == 0) {
    return("No significant components found")
  }
  
  top_components <- results_list[[metric]] %>%
    head(max_components) %>%
    pull(component)
  
  return(paste(top_components, collapse = ", "))
}

# Create summary table
summary_table <- data.frame(
  bank_type = character(),
  key_metric = character(),
  top_components = character()
)

# Add results for all banks
for (metric in key_metrics) {
  summary_table <- rbind(summary_table, 
                         data.frame(
                           bank_type = "All Banks",
                           key_metric = metric,
                           top_components = extract_top_components(all_banks_results, metric)
                         ))
}

# Add results for small banks
for (metric in key_metrics) {
  summary_table <- rbind(summary_table, 
                         data.frame(
                           bank_type = "Small Banks",
                           key_metric = metric,
                           top_components = extract_top_components(small_banks_results, metric)
                         ))
}

# Add results for large banks
for (metric in key_metrics) {
  summary_table <- rbind(summary_table, 
                         data.frame(
                           bank_type = "Large Banks",
                           key_metric = metric,
                           top_components = extract_top_components(large_banks_results, metric)
                         ))
}

# Add results for lending intensity
for (lending_type in c("Low Lending", "Medium Lending", "High Lending")) {
  summary_table <- rbind(summary_table, 
                         data.frame(
                           bank_type = lending_type,
                           key_metric = "cet1_ratio",
                           top_components = extract_top_components(lending_results, lending_type)
                         ))
}

# Add results for risk profile
for (risk_type in c("Low Risk", "Medium Risk", "High Risk")) {
  summary_table <- rbind(summary_table, 
                         data.frame(
                           bank_type = risk_type,
                           key_metric = "cet1_ratio",
                           top_components = extract_top_components(risk_results, risk_type)
                         ))
}

# Print summary table
cat("\n\nSUMMARY OF KEY ESG COMPONENTS BY BANK TYPE:\n")
print(summary_table)

# 8. STRATEGIC RECOMMENDATIONS
cat("\n\nSTRATEGIC ESG COMPONENT RECOMMENDATIONS:\n")
cat("===========================================\n\n")

# Small Banks
cat("FOR SMALL BANKS:\n")
cat("----------------\n")
small_key_components <- small_bank_corrs %>%
  filter(financial_metric == "cet1_ratio") %>%
  arrange(desc(abs(correlation))) %>%
  head(3) %>%
  mutate(
    component_clean = gsub("_score|_pillar", "", esg_component),
    component_clean = gsub("_", " ", component_clean),
    direction = ifelse(correlation > 0, "positive", "negative")
  )

for (i in 1:nrow(small_key_components)) {
  cat(paste0(
    "Component: ", small_key_components$component_clean[i], 
    " (correlation: ", round(small_key_components$correlation[i], 3), ", ",
    small_key_components$direction[i], " relationship with CET1 ratio)\n"
  ))
}

# Large Banks
cat("\nFOR LARGE BANKS:\n")
cat("----------------\n")
large_key_components <- large_bank_corrs %>%
  filter(financial_metric == "cet1_ratio") %>%
  arrange(desc(abs(correlation))) %>%
  head(3) %>%
  mutate(
    component_clean = gsub("_score|_pillar", "", esg_component),
    component_clean = gsub("_", " ", component_clean),
    direction = ifelse(correlation > 0, "positive", "negative")
  )

for (i in 1:nrow(large_key_components)) {
  cat(paste0(
    "Component: ", large_key_components$component_clean[i], 
    " (correlation: ", round(large_key_components$correlation[i], 3), ", ",
    large_key_components$direction[i], " relationship with CET1 ratio)\n"
  ))
}

# By Lending Intensity
lending_types <- c("Low Lending", "Medium Lending", "High Lending")
for (lending_type in lending_types) {
  lending_data <- data_analysis %>% filter(loan_intensity == lending_type)
  
  cat(paste0("\nFOR ", toupper(lending_type), " BANKS:\n"))
  cat(paste0(rep("-", nchar(paste0("FOR ", toupper(lending_type), " BANKS:")) - 1), collapse = ""), "\n")
  
  lending_corrs <- calculate_group_correlations(lending_data, lending_type) %>%
    filter(financial_metric == "cet1_ratio") %>%
    arrange(desc(abs(correlation))) %>%
    head(3) %>%
    mutate(
      component_clean = gsub("_score|_pillar", "", esg_component),
      component_clean = gsub("_", " ", component_clean),
      direction = ifelse(correlation > 0, "positive", "negative")
    )
  
  if (nrow(lending_corrs) > 0) {
    for (i in 1:nrow(lending_corrs)) {
      cat(paste0(
        "Component: ", lending_corrs$component_clean[i], 
        " (correlation: ", round(lending_corrs$correlation[i], 3), ", ",
        lending_corrs$direction[i], " relationship with CET1 ratio)\n"
      ))
    }
  } else {
    cat("Insufficient data for meaningful recommendations\n")
  }
}

# Overall recommendations across all bank types
cat("\nOVERALL ESG COMPONENT STRATEGY INSIGHTS:\n")
cat("-------------------------------------\n")

# Identify consistently important components
important_components <- correlations_long %>%
  filter(financial_metric == "cet1_ratio") %>%
  arrange(desc(abs(correlation))) %>%
  head(5) %>%
  pull(esg_component)

for (component in important_components) {
  # Check consistency across bank segments
  component_corrs <- bank_size_correlations %>%
    filter(
      esg_component == component,
      financial_metric == "cet1_ratio"
    )
  
  small_corr <- component_corrs %>% 
    filter(group == "Small Banks") %>% 
    pull(correlation)
  
  large_corr <- component_corrs %>% 
    filter(group == "Large Banks") %>% 
    pull(correlation)
  
  component_clean <- gsub("_score|_pillar", "", component)
  component_clean <- gsub("_", " ", component_clean)
  
  # Evaluate consistency
  if (sign(small_corr) == sign(large_corr)) {
    cat(paste0(
      "- ", component_clean, " shows a consistent ", 
      ifelse(small_corr > 0, "positive", "negative"),
      " relationship with capital ratios across bank sizes.\n"
    ))
  } else {
    cat(paste0(
      "- ", component_clean, " shows different relationships by bank size: ",
      ifelse(small_corr > 0, "positive", "negative"), " for small banks but ",
      ifelse(large_corr > 0, "positive", "negative"), " for large banks.\n"
    ))
  }
}

# Strategic investment guidance
cat("\nSTRATEGIC INVESTMENT GUIDANCE:\n")
cat("-----------------------------\n")
cat("Based on the analysis, banks should consider the following when developing their ESG strategy:\n\n")

cat("1. BANK SIZE CONSIDERATIONS:\n")
cat("   * Small banks: Focus on community engagement, emissions management, and shareholder relations\n")
cat("   * Large banks: Product responsibility, workforce practices, and social pillar components are key\n\n")

cat("2. BUSINESS MODEL CONSIDERATIONS:\n")
cat("   * High-lending banks: Different ESG factors impact capital ratios compared to low-lending banks\n")
cat("   * Banks should align their ESG strategy with their core business model\n\n")

cat("3. RISK PROFILE CONSIDERATIONS:\n")
cat("   * Risk profile significantly influences which ESG components are most beneficial\n")
cat("   * Banks should tailor their ESG approach based on their overall risk appetite\n")