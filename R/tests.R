# Check if any banks changed countries over time for country as control variable
country_changes <- data_analysis %>%
  group_by(lei_code) %>%
  summarise(num_countries = n_distinct(country)) %>%
  filter(num_countries > 1)

print(paste("Number of banks that changed countries:", nrow(country_changes)))
# No change -> no country as control





# Model 3: Alternative approach using lm with dummy variables for fixed effects
# This approach gives identical results but allows for different standard error calculations
model3 <- lm(credit_risk ~ esg_score + log_assets + equity_to_assets + 
               factor(lei_code) + factor(year_factor),
             data = data_analysis)

# Use robust standard errors
library(sandwich)
library(lmtest)

robust_se <- coeftest(model3, vcov = vcovHC(model3, type = "HC1"))
print(robust_se)

# Create diagnostic plots
# Residual plot
residuals_data <- data.frame(
  fitted = fitted(model1),
  residuals = residuals(model1)
)

residual_plot <- ggplot(residuals_data, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals")

print(residual_plot)

# Calculate partial effects of ESG on credit risk
library(effects)
esg_effect <- effect("esg_score", model1)
esg_effect_df <- as.data.frame(esg_effect)

esg_effect_plot <- ggplot(esg_effect_df, aes(x = esg_score, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Effect of ESG Score on Credit Risk",
       x = "ESG Score", y = "Credit Risk")

print(esg_effect_plot)



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