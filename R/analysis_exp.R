# --- Load Necessary Libraries ---
# Ensure these are loaded, potentially in your Rmd setup chunk
library(dplyr)
library(tidyr)      # For pivot_longer/wider
library(kableExtra)
library(ggplot2)
library(patchwork)  # For combining plots (install if needed: install.packages("patchwork"))

# --- Function 1: Generate Descriptive Summary Statistics Table ---

#' Generates a descriptive statistics table for selected variables.
#'
#' @param data The input dataframe (e.g., data_analysis).
#' @param vars A character vector of variable names to summarize.
#' @param title The title caption for the table.
#' @param digits Integer; number of decimal places for numeric stats.
#' @param output_format Character; "kableExtra" for direct Rmd output, "dataframe" for raw data.
#' @param kable_options List; additional options for kable_styling (e.g., font_size).
#' @return A kable object for Rmd or a dataframe.
#'
#' @examples
#' # key_vars <- c("esg_score", "environmental_pillar_score", "social_pillar_score",
#' #               "governance_pillar_score", "cet1_risk_exposure", "leverage_ratio",
#' #               "log_assets", "equity_to_assets")
#' # summary_table <- generate_summary_table(data_analysis, key_vars)
#' # print(summary_table) # To display in R console or Rmd chunk
#' # save_kable(summary_table, "table_summary_stats.tex") # To save as .tex
generate_summary_table <- function(data, vars,
                                   title = "Descriptive Statistics",
                                   digits = 3,
                                   output_format = "kableExtra",
                                   kable_options = list(latex_options = c("scale_down", "HOLD_position"),
                                                        full_width = FALSE,
                                                        font_size = 8)) {
  
  # Check if variables exist
  missing_vars <- setdiff(vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop("The following variables were not found in the data: ", paste(missing_vars, collapse = ", "))
  }
  
  summary_data <- data %>%
    select(all_of(vars)) %>%
    # Ensure selected columns are numeric before summarizing
    select(where(is.numeric)) %>%
    summarise(across(everything(),
                     list(N = ~sum(!is.na(.)),
                          Mean = ~mean(., na.rm = TRUE),
                          SD = ~sd(., na.rm = TRUE),
                          Min = ~min(., na.rm = TRUE),
                          Max = ~max(., na.rm = TRUE)),
                     .names = "{.col}::{.fn}")) %>%
    pivot_longer(cols = everything(),
                 names_to = c("Variable", "Statistic"),
                 names_sep = "::",
                 values_to = "Value") %>%
    # Tidy variable names (replace underscores if desired)
    mutate(Variable = gsub("_", " ", Variable),
           Variable = tools::toTitleCase(Variable)) %>%
    pivot_wider(names_from = Statistic, values_from = Value) %>%
    # Order columns logically
    select(Variable, N, Mean, SD, Min, Max) %>%
    # Order rows by variable name provided
    arrange(match(Variable, tools::toTitleCase(gsub("_", " ", vars))))
  
  
  if (output_format == "dataframe") {
    return(summary_data)
  } else if (output_format == "kableExtra") {
    # Format using kableExtra
    table_out <- kbl(summary_data,
                     caption = title,
                     digits = digits,
                     format = "latex", # Defaulting to latex for academic papers
                     booktabs = TRUE,
                     linesep = "",
                     col.names = c("Variable", "N", "Mean", "Std. Dev.", "Min", "Max")) %>%
      kable_styling(latex_options = kable_options$latex_options %||% c("scale_down", "HOLD_position"),
                    full_width = kable_options$full_width %||% FALSE,
                    font_size = kable_options$font_size %||% 8) # Use %||% for default if NULL
    return(table_out)
  } else {
    stop("Invalid output_format. Choose 'kableExtra' or 'dataframe'.")
  }
}

# Helper function for kable_options defaults
`%||%` <- function(a, b) if (is.null(a)) b else a


# --- Function 2: Generate Correlation Matrix Table ---

#' Generates a correlation matrix table for selected variables.
#'
#' @param data The input dataframe (e.g., data_analysis).
#' @param vars A character vector of variable names for correlation.
#' @param title The title caption for the table.
#' @param digits Integer; number of decimal places for correlations.
#' @param output_format Character; "kableExtra" or "dataframe".
#' @param kable_options List; additional options for kable_styling.
#' @return A kable object for Rmd or a dataframe.
#'
#' @examples
#' # cor_vars <- c("esg_score", "environmental_pillar_score", "social_pillar_score",
#' #               "governance_pillar_score", "cet1_risk_exposure", "leverage_ratio",
#' #               "log_assets", "equity_to_assets")
#' # correlation_table <- generate_correlation_table(data_analysis, cor_vars)
#' # print(correlation_table)
#' # save_kable(correlation_table, "table_correlation.tex")
generate_correlation_table <- function(data, vars,
                                       title = "Correlation Matrix",
                                       digits = 2,
                                       output_format = "kableExtra",
                                       kable_options = list(latex_options = c("scale_down", "HOLD_position"),
                                                            full_width = FALSE,
                                                            font_size = 7)) { # Smaller font often needed
  
  # Check if variables exist
  missing_vars <- setdiff(vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop("The following variables were not found in the data: ", paste(missing_vars, collapse = ", "))
  }
  
  # Select numeric columns and calculate correlation
  cor_matrix <- data %>%
    select(all_of(vars)) %>%
    select(where(is.numeric)) %>% # Ensure only numeric cols
    cor(use = "pairwise.complete.obs") # Handle missing values robustly
  
  # Optional: Nicer variable names for the table
  colnames(cor_matrix) <- rownames(cor_matrix) <- tools::toTitleCase(gsub("_", " ", colnames(cor_matrix)))
  
  if (output_format == "dataframe") {
    return(as.data.frame(cor_matrix))
  } else if (output_format == "kableExtra") {
    # Format using kableExtra
    table_out <- kbl(cor_matrix,
                     caption = title,
                     digits = digits,
                     format = "latex",
                     booktabs = TRUE,
                     linesep = "") %>%
      kable_styling(latex_options = kable_options$latex_options %||% c("scale_down", "HOLD_position"),
                    full_width = kable_options$full_width %||% FALSE,
                    font_size = kable_options$font_size %||% 7) %>%
      # Add a footnote about significance if needed (requires more complex calculation)
      # For now, just note the calculation method
      footnote(general = "Pairwise Pearson correlations calculated.",
               threeparttable = TRUE, escape = FALSE)
    
    return(table_out)
  } else {
    stop("Invalid output_format. Choose 'kableExtra' or 'dataframe'.")
  }
}


# --- Function 3: Generate Distribution Plots ---

#' Generates distribution plots (histogram + density) for selected variables.
#'
#' @param data The input dataframe (e.g., data_analysis).
#' @param vars A character vector of variable names to plot.
#' @param n_col Integer; number of columns for arranging plots. NULL for auto.
#' @param hist_bins Integer; number of bins for histograms.
#' @return A ggplot object (combined plot using patchwork).
#'
#' @examples
#' # dist_vars <- c("esg_score", "cet1_risk_exposure", "leverage_ratio",
#' #                "log_assets", "equity_to_assets")
#' # distribution_plot <- generate_distribution_plots(data_analysis, dist_vars)
#' # print(distribution_plot) # To display in R console or Rmd chunk
#' # ggsave("figure_distributions.png", plot = distribution_plot, width = 8, height = 6, dpi = 300)
generate_distribution_plots <- function(data, vars, n_col = NULL, hist_bins = 20) {
  
  # Check if variables exist
  missing_vars <- setdiff(vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop("The following variables were not found in the data: ", paste(missing_vars, collapse = ", "))
  }
  
  plot_list <- list()
  
  for (var in vars) {
    # Tidy variable name for title
    plot_title <- tools::toTitleCase(gsub("_", " ", var))
    
    # Create plot for the variable
    p <- ggplot(data, aes_string(x = var)) +
      geom_histogram(aes(y = ..density..), bins = hist_bins, fill = "lightblue", color = "grey30", alpha = 0.7, na.rm = TRUE) +
      geom_density(color = "darkred", size = 0.8, na.rm = TRUE) +
      labs(title = plot_title, x = NULL, y = "Density") + # Keep titles concise
      theme_minimal(base_size = 10) + # Adjust base size as needed
      theme(plot.title = element_text(hjust = 0.5, size = rel(1.0))) # Center title
    
    plot_list[[var]] <- p
  }
  
  # Combine plots using patchwork
  if (is.null(n_col)) {
    n_col <- ceiling(sqrt(length(plot_list)))
  }
  combined_plot <- wrap_plots(plot_list, ncol = n_col)
  
  # Add an overall title (optional)
  # combined_plot <- combined_plot + plot_annotation(title = "Distributions of Key Variables")
  
  return(combined_plot)
}

# --- Function 4 (Optional): Generate Time Trend Plots ---

#' Generates plots showing the trend of variable means over time.
#'
#' @param data The input dataframe (must contain 'year' column).
#' @param vars A character vector of variable names to plot trends for.
#' @param n_col Integer; number of columns for arranging plots. NULL for auto.
#' @return A ggplot object (combined plot using patchwork).
#'
#' @examples
#' # trend_vars <- c("esg_score", "cet1_risk_exposure", "leverage_ratio")
#' # time_trend_plot <- generate_time_trend_plots(data_analysis, trend_vars)
#' # print(time_trend_plot)
#' # ggsave("figure_time_trends.png", plot = time_trend_plot, width = 8, height = 4, dpi = 300)
generate_time_trend_plots <- function(data, vars, n_col = NULL) {
  # Check for 'year' column
  if (!"year" %in% colnames(data)) {
    stop("Dataframe must contain a 'year' column.")
  }
  
  # Check if variables exist
  missing_vars <- setdiff(vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop("The following variables were not found in the data: ", paste(missing_vars, collapse = ", "))
  }
  
  plot_list <- list()
  
  # Calculate yearly means
  yearly_means <- data %>%
    select(year, all_of(vars)) %>%
    group_by(year) %>%
    summarise(across(all_of(vars), ~mean(., na.rm = TRUE)), .groups = 'drop') %>%
    pivot_longer(-year, names_to = "variable", values_to = "mean_value") %>%
    mutate(variable_tidy = tools::toTitleCase(gsub("_", " ", variable)))
  
  
  for (var_tidy in unique(yearly_means$variable_tidy)) {
    current_var_data <- filter(yearly_means, variable_tidy == var_tidy)
    p <- ggplot(current_var_data, aes(x = year, y = mean_value)) +
      geom_line(color = "dodgerblue", size = 1) +
      geom_point(color = "dodgerblue", size = 2) +
      scale_x_continuous(breaks = unique(data$year)) + # Ensure all years are shown as ticks
      labs(title = var_tidy, x = "Year", y = "Average Value") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(hjust = 0.5, size = rel(1.0)))
    
    plot_list[[var_tidy]] <- p
  }
  
  # Combine plots using patchwork
  if (is.null(n_col)) {
    n_col <- length(plot_list) # Default to one row if not specified
    if (n_col > 3) n_col <- ceiling(length(plot_list) / ceiling(length(plot_list)/3)) # Limit columns
  }
  combined_plot <- wrap_plots(plot_list, ncol = n_col)
  
  # Add an overall title (optional)
  # combined_plot <- combined_plot + plot_annotation(title = "Average Trends Over Time (2020-2023)")
  
  return(combined_plot)
}