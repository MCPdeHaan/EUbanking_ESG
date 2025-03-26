library(plm)
library(dplyr)

# Fixed effects of banks and years
panel_data <- pdata.frame(data_analysis, index = c("lei_code", "year"))

# Running models for individual risk measures (excluding credit_risk)
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

# List of dependent variables to analyze (credit_risk removed)
dependent_vars <- c("cet1_risk_exposure", "tier1_risk_exposure", 
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

# Define more descriptive names for variables (credit_risk removed)
descriptive_labels <- c(
  "CET1 Risk\nExposure", "Tier1 Risk\nExposure", 
  "Total Capital\nRisk Exposure", "Leverage\nRatio", "Provisions", 
  "Provisions\nRatio", "Liquidity\nRatio", "RWA\nRatio"
)

# Extract models by type
esg_models <- lapply(results_list, function(x) x$model_esg)
names(esg_models) <- dependent_vars

pillar_models <- lapply(results_list, function(x) x$model_pillars)
names(pillar_models) <- dependent_vars

#-----------------------------------------------------------------
# MODELSUMMARY TABLES (MODERN, FLEXIBLE, BEAUTIFUL) WITH p < 0.1
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

# Custom function to extract coefficients and p-values for significance levels including p < 0.1
extract_custom <- function(model) {
  if(inherits(model, "plm")) {
    s <- summary(model)
    coefs <- s$coefficients
    
    # Extract coefficients and build a data frame with significance indicators
    result <- as.data.frame(coefs)
    result$term <- rownames(result)
    
    # Add significance indicators including p < 0.1
    result$significance <- case_when(
      result$`Pr(>|t|)` < 0.001 ~ "***",
      result$`Pr(>|t|)` < 0.01 ~ "**",
      result$`Pr(>|t|)` < 0.05 ~ "*",
      result$`Pr(>|t|)` < 0.1 ~ "†",  # Added symbol for p < 0.1
      TRUE ~ ""
    )
    
    # Create new estimate column with significance indicators
    result$estimate_with_sig <- paste0(sprintf("%.3f", result$Estimate), result$significance)
    
    # Prepare the output in the format expected by modelsummary
    out <- list(
      coef_names = result$term,
      coefficients = result$Estimate,
      std.error = result$`Std. Error`,
      statistic = result$`t-value`,
      p.value = result$`Pr(>|t|)`,
      estimate_with_sig = result$estimate_with_sig
    )
    
    # Add other necessary elements for modelsummary
    attr(out, "formula") <- formula(model)
    attr(out, "nobs") <- nrow(model$model)
    attr(out, "r.squared") <- s$r.squared[1]
    return(out)
  } else {
    return(NULL)
  }
}

# Apply custom extraction to all models
esg_models_extracted <- lapply(esg_models, extract_custom)
pillar_models_extracted <- lapply(pillar_models, extract_custom)

# Create the ESG score model table with custom formatting for p < 0.1
ms_esg_table <- modelsummary(
  esg_models,
  title = "Table 1: Effects of Overall ESG Score on Bank Risk Measures",
  stars = c('†' = 0.1, '*' = 0.05, '**' = 0.01, '***' = 0.001),  # Added p < 0.1
  gof_map = gof_map,
  coef_map = coef_map_esg,
  fmt = 3,                # 3 decimal places
  output = "kableExtra",  # For better RMarkdown integration
  note = "Bank and year fixed effects included in all models. †p<0.1; *p<0.05; **p<0.01; ***p<0.001",  # Updated note
  add_rows = tibble(
    term = c("Bank FE", "Year FE"),
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

# Create the ESG pillar score model table with custom formatting for p < 0.1
ms_pillar_table <- modelsummary(
  pillar_models,
  title = "Table 2: Effects of ESG Pillar Scores on Bank Risk Measures",
  stars = c('†' = 0.1, '*' = 0.05, '**' = 0.01, '***' = 0.001),  # Added p < 0.1
  gof_map = gof_map,
  coef_map = coef_map_pillars,
  fmt = 3,                # 3 decimal places
  output = "kableExtra",  # For better RMarkdown integration
  note = "Bank and year fixed effects included in all models. †p<0.1; *p<0.05; **p<0.01; ***p<0.001",  # Updated note
  add_rows = tibble(
    term = c("Bank FE", "Year FE"),
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

# Create visualizations of all significant relationships (p < 0.1) using ggplot2
library(ggplot2)
library(gridExtra)

# Function to extract significant coefficients (p < 0.1) and create plots
create_significance_plots <- function(model_list, model_type = "ESG") {
  plot_list <- list()
  
  for (dep_var_name in names(model_list)) {
    model <- model_list[[dep_var_name]]
    model_summary <- summary(model)
    coefs <- model_summary$coefficients
    
    # Get significant coefficients (p < 0.1)
    significant_vars <- rownames(coefs)[coefs[, "Pr(>|t|)"] < 0.1]
    
    if (length(significant_vars) > 0) {
      # For each significant variable, create a plot showing the relationship
      for (var in significant_vars) {
        # Skip control variables if desired
        if (!grepl("log_assets|equity_to_assets|lag_", var)) {
          
          # Create a range of values for the significant variable
          if (grepl("squared", var)) {
            # For squared terms, we need the base variable too
            base_var <- gsub("_squared", "", var)
            
            # Create data for plotting
            plot_data <- data.frame(
              x = seq(min(panel_data[[base_var]], na.rm = TRUE), 
                      max(panel_data[[base_var]], na.rm = TRUE), 
                      length.out = 100)
            )
            
            # Get coefficients
            linear_coef <- coefs[base_var, "Estimate"]
            squared_coef <- coefs[var, "Estimate"]
            
            # Calculate predicted effect (excluding other variables)
            plot_data$y <- linear_coef * plot_data$x + squared_coef * plot_data$x^2
            
            # Create the plot
            p <- ggplot(plot_data, aes(x = x, y = y)) +
              geom_line(color = "blue", size = 1) +
              labs(
                title = paste0("Effect of ", gsub("_", " ", base_var), " on ", gsub("_", " ", dep_var_name)),
                subtitle = paste0("p-value: ", sprintf("%.4f", coefs[var, "Pr(>|t|)"]), 
                                  " (", ifelse(coefs[var, "Pr(>|t|)"] < 0.001, "***", 
                                               ifelse(coefs[var, "Pr(>|t|)"] < 0.01, "**", 
                                                      ifelse(coefs[var, "Pr(>|t|)"] < 0.05, "*", "†"))), ")"),
                x = gsub("_", " ", base_var),
                y = "Estimated Effect"
              ) +
              theme_minimal() +
              theme(plot.title = element_text(face = "bold", size = 10),
                    plot.subtitle = element_text(size = 8))
            
            plot_list[[paste(dep_var_name, var, sep = "_")]] <- p
          } else if (!grepl("squared", var)) {
            # For linear terms (if not part of a quadratic relationship)
            if (!(paste0(var, "_squared") %in% rownames(coefs))) {
              # Create data for plotting
              plot_data <- data.frame(
                x = seq(min(panel_data[[var]], na.rm = TRUE), 
                        max(panel_data[[var]], na.rm = TRUE), 
                        length.out = 100)
              )
              
              # Get coefficient
              linear_coef <- coefs[var, "Estimate"]
              
              # Calculate predicted effect (excluding other variables)
              plot_data$y <- linear_coef * plot_data$x
              
              # Create the plot
              p <- ggplot(plot_data, aes(x = x, y = y)) +
                geom_line(color = "blue", size = 1) +
                labs(
                  title = paste0("Effect of ", gsub("_", " ", var), " on ", gsub("_", " ", dep_var_name)),
                  subtitle = paste0("p-value: ", sprintf("%.4f", coefs[var, "Pr(>|t|)"]), 
                                    " (", ifelse(coefs[var, "Pr(>|t|)"] < 0.001, "***", 
                                                 ifelse(coefs[var, "Pr(>|t|)"] < 0.01, "**", 
                                                        ifelse(coefs[var, "Pr(>|t|)"] < 0.05, "*", "†"))), ")"),
                  x = gsub("_", " ", var),
                  y = "Estimated Effect"
                ) +
                theme_minimal() +
                theme(plot.title = element_text(face = "bold", size = 10),
                      plot.subtitle = element_text(size = 8))
              
              plot_list[[paste(dep_var_name, var, sep = "_")]] <- p
            }
          }
        }
      }
    }
  }
  
  return(plot_list)
}

# Create plots for ESG models
esg_plots <- create_significance_plots(esg_models, "ESG")

# Create plots for Pillar models
pillar_plots <- create_significance_plots(pillar_models, "Pillars")

# Combine all plots - ESG models
if (length(esg_plots) > 0) {
  # Arrange plots in a grid (up to 6 plots per page)
  max_plots_per_page <- 6
  total_pages <- ceiling(length(esg_plots) / max_plots_per_page)
  
  for (page in 1:total_pages) {
    # Calculate start and end indices for this page
    start_idx <- (page - 1) * max_plots_per_page + 1
    end_idx <- min(page * max_plots_per_page, length(esg_plots))
    
    # Get plots for this page
    page_plots <- esg_plots[start_idx:end_idx]
    
    # Arrange plots in a grid
    grid_title <- paste0("Significant ESG Relationships (p < 0.1) - Page ", page)
    
    # Save grid plot
    grid_plot <- do.call(grid.arrange, c(page_plots, 
                                         top = grid_title,
                                         ncol = 2))
    
    # Save to file
    ggsave(paste0("esg_significant_relationships_p01_page", page, ".png"), 
           grid_plot, 
           width = 10, 
           height = 8)
  }
}

# Combine all plots - Pillar models
if (length(pillar_plots) > 0) {
  # Arrange plots in a grid (up to 6 plots per page)
  max_plots_per_page <- 6
  total_pages <- ceiling(length(pillar_plots) / max_plots_per_page)
  
  for (page in 1:total_pages) {
    # Calculate start and end indices for this page
    start_idx <- (page - 1) * max_plots_per_page + 1
    end_idx <- min(page * max_plots_per_page, length(pillar_plots))
    
    # Get plots for this page
    page_plots <- pillar_plots[start_idx:end_idx]
    
    # Arrange plots in a grid
    grid_title <- paste0("Significant ESG Pillar Relationships (p < 0.1) - Page ", page)
    
    # Save grid plot
    grid_plot <- do.call(grid.arrange, c(page_plots, 
                                         top = grid_title,
                                         ncol = 2))
    
    # Save to file
    ggsave(paste0("pillar_significant_relationships_p01_page", page, ".png"), 
           grid_plot, 
           width = 10, 
           height = 8)
  }
}

# Create a summary table of significant relationships (p < 0.1)
create_significance_summary <- function(model_list, model_type = "ESG") {
  # Create a dataframe to store results
  results <- data.frame(
    Dependent_Variable = character(),
    Independent_Variable = character(),
    Coefficient = numeric(),
    P_Value = numeric(),
    Significance = character(),
    stringsAsFactors = FALSE
  )
  
  for (dep_var_name in names(model_list)) {
    model <- model_list[[dep_var_name]]
    model_summary <- summary(model)
    coefs <- model_summary$coefficients
    
    # Get significant coefficients (p < 0.1)
    significant_vars <- rownames(coefs)[coefs[, "Pr(>|t|)"] < 0.1]
    
    if (length(significant_vars) > 0) {
      for (var in significant_vars) {
        # Add row to results dataframe
        results <- rbind(results, data.frame(
          Dependent_Variable = dep_var_name,
          Independent_Variable = var,
          Coefficient = coefs[var, "Estimate"],
          P_Value = coefs[var, "Pr(>|t|)"],
          Significance = ifelse(coefs[var, "Pr(>|t|)"] < 0.001, "***", 
                                ifelse(coefs[var, "Pr(>|t|)"] < 0.01, "**", 
                                       ifelse(coefs[var, "Pr(>|t|)"] < 0.05, "*", "†"))),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Sort by p-value
  results <- results[order(results$P_Value), ]
  
  # Format table
  results$Coefficient <- sprintf("%.3f", results$Coefficient)
  results$P_Value <- sprintf("%.4f", results$P_Value)
  results$Formatted <- paste0(results$Coefficient, results$Significance)
  
  # Create and save the table
  sig_table <- kable(results, 
                     caption = paste0("Significant Relationships (p < 0.1) in ", model_type, " Models"),
                     col.names = c("Dependent Variable", "Independent Variable", "Coefficient", "P-Value", "Sig", "Formatted"),
                     align = c("l", "l", "r", "r", "c", "r")) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE,
                  font_size = 12)
  
  # Save to HTML and LaTeX
  save_kable(sig_table, paste0("significant_relationships_p01_", tolower(model_type), ".html"))
  save_kable(sig_table, paste0("significant_relationships_p01_", tolower(model_type), ".tex"))
  
  return(results)
}

# Create summary tables
esg_significance_summary <- create_significance_summary(esg_models, "ESG")
pillar_significance_summary <- create_significance_summary(pillar_models, "Pillar")

# Print summaries
cat("Significant ESG model relationships (p < 0.1):\n")
print(esg_significance_summary)

cat("\nSignificant Pillar model relationships (p < 0.1):\n")
print(pillar_significance_summary)

# Calculate the number of significant relationships at different p-value thresholds
count_significant_relationships <- function(model_list, thresholds = c(0.001, 0.01, 0.05, 0.1)) {
  counts <- data.frame(
    Threshold = character(),
    Count = integer(),
    stringsAsFactors = FALSE
  )
  
  for (threshold in thresholds) {
    count <- 0
    
    for (dep_var_name in names(model_list)) {
      model <- model_list[[dep_var_name]]
      model_summary <- summary(model)
      coefs <- model_summary$coefficients
      
      # Count significant coefficients at this threshold
      significant_vars <- rownames(coefs)[coefs[, "Pr(>|t|)"] < threshold]
      count <- count + length(significant_vars)
    }
    
    # Add row to counts dataframe
    counts <- rbind(counts, data.frame(
      Threshold = paste0("p < ", threshold),
      Count = count,
      stringsAsFactors = FALSE
    ))
  }
  
  return(counts)
}

# Count significant relationships
esg_counts <- count_significant_relationships(esg_models)
pillar_counts <- count_significant_relationships(pillar_models)

# Print counts
cat("Number of significant relationships in ESG models:\n")
print(esg_counts)

cat("\nNumber of significant relationships in Pillar models:\n")
print(pillar_counts)