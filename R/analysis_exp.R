# Fixing the summary_statistics table creation in the run_exploratory_analysis function
# This modified version adds error handling and column checking

run_exploratory_analysis <- function(data, min_obs_per_bank = 3) {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(kableExtra)
  library(corrplot)
  library(gridExtra)
  library(scales)
  library(reshape2)
  
  # Check if required columns exist
  required_vars <- c("lei_code", "bank_name", "country", "year", "esg_score", 
                     "environmental_pillar_score", "social_pillar_score", "governance_pillar_score")
  
  risk_vars <- c("cet1_risk_exposure", "tier1_risk_exposure", "totalcap_risk_exposure", 
                 "leverage_ratio", "provisions", "provisions_ratio", "liquidity_ratio", "rwa_ratio")
  
  control_vars <- c("log_assets", "equity_to_assets")
  
  missing_vars <- setdiff(c(required_vars, risk_vars), names(data))
  if (length(missing_vars) > 0) {
    warning(paste("Missing required variables:", paste(missing_vars, collapse = ", ")))
  }
  
  # Create list to store results
  results <- list()
  
  # ---- 1. Data Coverage and Structure ----
  
  # 1.1 Sample composition
  banks_per_country <- data %>%
    group_by(country) %>%
    summarize(
      unique_banks = n_distinct(lei_code),
      total_obs = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(unique_banks))
  
  # Create a nicer table for display
  country_table <- banks_per_country %>%
    rename(
      "Country" = country,
      "Number of Banks" = unique_banks,
      "Total Observations" = total_obs
    )
  
  # Safely create the kable table
  tryCatch({
    results$country_composition <- kable(country_table,
                                         caption = "Sample Composition by Country",
                                         booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped", "hold_position"),
                    full_width = FALSE,
                    font_size = 9)
  }, error = function(e) {
    warning("Error creating country composition table: ", e$message)
    results$country_composition <- NULL
  })
  
  # 1.2 Time series coverage
  yearly_coverage <- data %>%
    group_by(year) %>%
    summarize(
      banks = n_distinct(lei_code),
      .groups = "drop"
    )
  
  # Banks with complete time series
  bank_coverage <- data %>%
    group_by(lei_code) %>%
    summarize(
      observations = n(),
      years = paste(sort(year), collapse = ", "),
      .groups = "drop"
    )
  
  complete_banks <- bank_coverage %>%
    filter(observations >= min_obs_per_bank) %>%
    nrow()
  
  # Create a coverage summary
  coverage_data <- data.frame(
    Category = c(
      "Total unique banks",
      "Banks with complete time series",
      "Total bank-year observations",
      "Average observations per bank",
      "Time period"
    ),
    Value = c(
      n_distinct(data$lei_code),
      complete_banks,
      nrow(data),
      round(mean(bank_coverage$observations), 1),
      paste(min(data$year), "-", max(data$year))
    )
  )
  
  # Safely create the kable table
  tryCatch({
    results$data_coverage <- kable(coverage_data,
                                   col.names = c("Category", "Value"),
                                   caption = "Data Coverage Summary",
                                   booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped", "hold_position"),
                    full_width = FALSE,
                    font_size = 9)
  }, error = function(e) {
    warning("Error creating data coverage table: ", e$message)
    results$data_coverage <- NULL
  })
  
  # 1.3 Missing values analysis
  vars_to_check <- c(required_vars, risk_vars, control_vars)
  vars_to_check <- intersect(vars_to_check, names(data))
  
  missing_analysis <- data %>%
    select(all_of(vars_to_check)) %>%
    summarize(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Variable", 
                 values_to = "Missing_Values") %>%
    mutate(
      Percentage = round(Missing_Values / nrow(data) * 100, 1),
      Category = case_when(
        Variable %in% c("esg_score", "environmental_pillar_score", 
                        "social_pillar_score", "governance_pillar_score") ~ "ESG",
        Variable %in% risk_vars ~ "Risk",
        Variable %in% control_vars ~ "Control",
        TRUE ~ "Identifier"
      )
    ) %>%
    arrange(Category, desc(Missing_Values))
  
  # Safely create the kable table
  tryCatch({
    results$missing_values <- kable(missing_analysis,
                                    col.names = c("Variable", "Missing Values", "Percentage (%)", "Category"),
                                    caption = "Missing Values Analysis",
                                    booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped", "hold_position"),
                    full_width = FALSE,
                    font_size = 9) %>%
      collapse_rows(columns = 4, valign = "top")
  }, error = function(e) {
    warning("Error creating missing values table: ", e$message)
    results$missing_values <- NULL
  })
  
  # ---- 2. Descriptive Statistics ----
  
  # 2.1 Summary statistics for key variables
  vars_for_summary <- c(
    "esg_score", "environmental_pillar_score", "social_pillar_score", "governance_pillar_score",
    risk_vars, control_vars
  )
  vars_for_summary <- intersect(vars_for_summary, names(data))
  
  summary_stats <- data %>%
    select(all_of(vars_for_summary)) %>%
    summarize(across(everything(), 
                     list(
                       Mean = ~mean(., na.rm = TRUE),
                       SD = ~sd(., na.rm = TRUE), 
                       Min = ~min(., na.rm = TRUE),
                       Q1 = ~quantile(., 0.25, na.rm = TRUE),
                       Median = ~median(., na.rm = TRUE),
                       Q3 = ~quantile(., 0.75, na.rm = TRUE),
                       Max = ~max(., na.rm = TRUE),
                       N = ~sum(!is.na(.))
                     )))
  
  # Reshape for better presentation
  summary_long <- summary_stats %>%
    pivot_longer(cols = everything(), 
                 names_to = c("Variable", "Statistic"), 
                 names_sep = "_") %>%
    pivot_wider(names_from = Statistic, values_from = value) %>%
    mutate(
      Category = case_when(
        Variable %in% c("esg_score", "environmental_pillar_score", 
                        "social_pillar_score", "governance_pillar_score") ~ "ESG Metrics",
        Variable %in% c("cet1_risk_exposure", "tier1_risk_exposure", 
                        "totalcap_risk_exposure") ~ "Capital Adequacy",
        Variable %in% c("leverage_ratio", "rwa_ratio") ~ "Risk Exposure",
        Variable %in% c("provisions", "provisions_ratio", "liquidity_ratio") ~ "Liquidity & Provisions",
        TRUE ~ "Control Variables"
      )
    ) %>%
    arrange(Category, Variable) 
  
  summary_long <- summary_long %>%
    mutate(across(c(Mean, SD, Min, Q1, Median, Q3, Max), 
                  ~as.numeric(as.character(.)), 
                  .names = "{.col}")) %>%
    mutate(across(c(Mean, SD, Min, Q1, Median, Q3, Max), 
                  ~round(., 3)))
  
  # Create a nicer variable description
  var_labels <- c(
    "esg_score" = "ESG Score",
    "environmental_pillar_score" = "Environmental Score",
    "social_pillar_score" = "Social Score",
    "governance_pillar_score" = "Governance Score",
    "cet1_risk_exposure" = "CET1 Risk Exposure",
    "tier1_risk_exposure" = "Tier 1 Risk Exposure",
    "totalcap_risk_exposure" = "Total Capital Risk Exposure",
    "leverage_ratio" = "Leverage Ratio",
    "provisions" = "Provisions",
    "provisions_ratio" = "Provisions Ratio",
    "liquidity_ratio" = "Liquidity Ratio",
    "rwa_ratio" = "RWA Ratio",
    "log_assets" = "Log Assets",
    "equity_to_assets" = "Equity to Assets"
  )
  
  summary_long$Variable <- ifelse(
    summary_long$Variable %in% names(var_labels),
    var_labels[summary_long$Variable],
    gsub("_", " ", tools::toTitleCase(summary_long$Variable))
  )
  
  # Check if Category column exists before using it for collapse_rows
  has_category_column <- "Category" %in% colnames(summary_long)
  
  # Safely create the kable table with correct column count
  tryCatch({
    table_columns <- c("Variable", "Mean", "Std. Dev.", "Min", "Q1", "Median", "Q3", "Max", "N")
    if(has_category_column) {
      table_columns <- c(table_columns, "Category")
    }
    
    # Ensure column count matches
    actual_columns <- intersect(table_columns, colnames(summary_long))
    
    results$summary_statistics <- kable(summary_long,
                                        col.names = actual_columns,
                                        caption = "Descriptive Statistics for Key Variables",
                                        booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                    full_width = FALSE,
                    font_size = 9)
    
    # Only use collapse_rows if Category column exists
    if(has_category_column) {
      # Find the position of the Category column
      category_col_pos <- which(actual_columns == "Category")
      if(length(category_col_pos) > 0) {
        results$summary_statistics <- results$summary_statistics %>%
          collapse_rows(columns = category_col_pos, valign = "top")
      }
    }
  }, error = function(e) {
    warning("Error creating summary statistics table: ", e$message)
    results$summary_statistics <- NULL
  })
  
  # 2.2 Yearly trends for ESG scores
  yearly_esg <- data %>%
    group_by(year) %>%
    summarize(
      ESG_Mean = mean(esg_score, na.rm = TRUE),
      ESG_Median = median(esg_score, na.rm = TRUE),
      ESG_SD = sd(esg_score, na.rm = TRUE),
      Env_Mean = mean(environmental_pillar_score, na.rm = TRUE),
      Soc_Mean = mean(social_pillar_score, na.rm = TRUE),
      Gov_Mean = mean(governance_pillar_score, na.rm = TRUE),
      N = n(),
      .groups = "drop"
    ) %>%
    mutate(across(starts_with(c("ESG_", "Env_", "Soc_", "Gov_")), ~round(., 2)))
  
  # Safely create the kable table
  tryCatch({
    results$yearly_esg <- kable(yearly_esg,
                                col.names = c("Year", "ESG Mean", "ESG Median", "ESG SD", 
                                              "Environmental", "Social", "Governance", "N"),
                                caption = "Yearly ESG Score Trends",
                                booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped", "hold_position"),
                    full_width = FALSE,
                    font_size = 9)
  }, error = function(e) {
    warning("Error creating yearly ESG table: ", e$message)
    results$yearly_esg <- NULL
  })
  
  # 2.3 ESG by bank size (using assets)
  if ("total_assets" %in% names(data) || "log_assets" %in% names(data)) {
    # Create size quartiles
    size_var <- if("total_assets" %in% names(data)) "total_assets" else "log_assets"
    
    data <- data %>%
      mutate(
        size_quartile = ntile(get(size_var), 4),
        size_group = case_when(
          size_quartile == 1 ~ "Q1 (Smallest)",
          size_quartile == 2 ~ "Q2",
          size_quartile == 3 ~ "Q3",
          size_quartile == 4 ~ "Q4 (Largest)"
        )
      )
    
    esg_by_size <- data %>%
      group_by(size_group) %>%
      summarize(
        ESG_Mean = mean(esg_score, na.rm = TRUE),
        ESG_Median = median(esg_score, na.rm = TRUE),
        ESG_SD = sd(esg_score, na.rm = TRUE),
        Env_Mean = mean(environmental_pillar_score, na.rm = TRUE),
        Soc_Mean = mean(social_pillar_score, na.rm = TRUE),
        Gov_Mean = mean(governance_pillar_score, na.rm = TRUE),
        Mean_Assets = if(size_var == "total_assets") 
          mean(total_assets/1e9, na.rm = TRUE) else mean(exp(log_assets)/1e9, na.rm = TRUE),
        N = n(),
        .groups = "drop"
      ) %>%
      mutate(
        across(c(starts_with(c("ESG_", "Env_", "Soc_", "Gov_"))), ~round(., 2)),
        Mean_Assets = round(Mean_Assets, 1)
      )
    
    # Safely create the kable table
    tryCatch({
      results$esg_by_size <- kable(esg_by_size,
                                   col.names = c("Size Group", "ESG Mean", "ESG Median", "ESG SD", 
                                                 "Environmental", "Social", "Governance", 
                                                 "Mean Assets (€B)", "N"),
                                   caption = "ESG Scores by Bank Size",
                                   booktabs = TRUE) %>%
        kable_styling(latex_options = c("striped", "hold_position"),
                      full_width = FALSE,
                      font_size = 9)
    }, error = function(e) {
      warning("Error creating ESG by size table: ", e$message)
      results$esg_by_size <- NULL
    })
  }
  
  # ---- 3. Visualizations ----
  
  # Safely create visualization with error handling for possible missing columns
  tryCatch({
    # 3.1 ESG scores distribution
    if("esg_score" %in% names(data)) {
      esg_hist <- ggplot(data, aes(x = esg_score)) +
        geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.7) +
        geom_vline(aes(xintercept = mean(esg_score, na.rm = TRUE)), 
                   color = "red", linetype = "dashed", size = 1) +
        labs(
          title = "Distribution of Overall ESG Scores",
          subtitle = paste("Mean:", round(mean(data$esg_score, na.rm = TRUE), 1),
                           "| Median:", round(median(data$esg_score, na.rm = TRUE), 1)),
          x = "ESG Score",
          y = "Frequency"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10),
          axis.title = element_text(face = "bold")
        )
      
      # Store the plot in results
      results$visualizations$esg_hist <- esg_hist
    }
    
    # 3.2 ESG pillar scores comparison
    pillar_cols <- c("environmental_pillar_score", "social_pillar_score", "governance_pillar_score")
    if(all(pillar_cols %in% names(data))) {
      pillar_data <- data %>%
        select(any_of(c("bank_name", "lei_code", "year")), all_of(pillar_cols)) %>%
        pivot_longer(
          cols = all_of(pillar_cols),
          names_to = "pillar",
          values_to = "score"
        ) %>%
        mutate(
          pillar = case_when(
            pillar == "environmental_pillar_score" ~ "Environmental",
            pillar == "social_pillar_score" ~ "Social",
            pillar == "governance_pillar_score" ~ "Governance"
          )
        )
      
      pillar_boxplot <- ggplot(pillar_data, aes(x = pillar, y = score, fill = pillar)) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_manual(values = c("Environmental" = "#4DAF4A", 
                                     "Social" = "#377EB8", 
                                     "Governance" = "#E41A1C")) +
        labs(
          title = "Comparison of ESG Pillar Scores",
          subtitle = "Distribution across all banks and years",
          x = "",
          y = "Score"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10),
          axis.title = element_text(face = "bold"),
          legend.position = "none"
        )
      
      # Store the plot in results
      results$visualizations$pillar_boxplot <- pillar_boxplot
    }
    
    # 3.3 ESG score trends over time
    if(all(c("esg_score", "year") %in% names(data)) && 
       all(pillar_cols %in% names(data))) {
      esg_time_data <- data %>%
        group_by(year) %>%
        summarize(
          esg_mean = mean(esg_score, na.rm = TRUE),
          env_mean = mean(environmental_pillar_score, na.rm = TRUE),
          soc_mean = mean(social_pillar_score, na.rm = TRUE),
          gov_mean = mean(governance_pillar_score, na.rm = TRUE),
          esg_se = sd(esg_score, na.rm = TRUE) / sqrt(sum(!is.na(esg_score))),
          .groups = "drop"
        )
      
      esg_trend_plot <- ggplot(esg_time_data, aes(x = year, y = esg_mean)) +
        geom_line(size = 1.2, color = "steelblue") +
        geom_point(size = 3, color = "steelblue") +
        geom_errorbar(aes(ymin = esg_mean - esg_se, ymax = esg_mean + esg_se), 
                      width = 0.2, color = "steelblue") +
        labs(
          title = "Average ESG Score Trend Over Time",
          subtitle = "With standard error bars",
          x = "Year",
          y = "Average ESG Score"
        ) +
        scale_x_continuous(breaks = unique(esg_time_data$year)) +
        expand_limits(y = c(min(esg_time_data$esg_mean) * 0.9, 
                            max(esg_time_data$esg_mean) * 1.05)) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10),
          axis.title = element_text(face = "bold")
        )
      
      # Store the plot in results
      results$visualizations$esg_trend_plot <- esg_trend_plot
      
      # 3.4 Pillar scores trends over time
      pillar_time_data <- esg_time_data %>%
        select(year, env_mean, soc_mean, gov_mean) %>%
        pivot_longer(
          cols = c(env_mean, soc_mean, gov_mean),
          names_to = "pillar",
          values_to = "score"
        ) %>%
        mutate(
          pillar = case_when(
            pillar == "env_mean" ~ "Environmental",
            pillar == "soc_mean" ~ "Social",
            pillar == "gov_mean" ~ "Governance"
          )
        )
      
      pillar_trend_plot <- ggplot(pillar_time_data, aes(x = year, y = score, color = pillar, group = pillar)) +
        geom_line(size = 1) +
        geom_point(size = 2.5) +
        scale_color_manual(values = c("Environmental" = "#4DAF4A", 
                                      "Social" = "#377EB8", 
                                      "Governance" = "#E41A1C")) +
        labs(
          title = "ESG Pillar Scores Trends Over Time",
          x = "Year",
          y = "Average Score"
        ) +
        scale_x_continuous(breaks = unique(pillar_time_data$year)) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          axis.title = element_text(face = "bold"),
          legend.title = element_blank(),
          legend.position = "bottom"
        )
      
      # Store the plot in results
      results$visualizations$pillar_trend_plot <- pillar_trend_plot
    }
    
    # Combine plots for easier viewing
    if(exists("esg_hist") && exists("pillar_boxplot")) {
      results$esg_dist_plot <- gridExtra::grid.arrange(
        esg_hist, pillar_boxplot, 
        ncol = 2
      )
    }
    
    if(exists("esg_trend_plot") && exists("pillar_trend_plot")) {
      results$esg_trends_plot <- gridExtra::grid.arrange(
        esg_trend_plot, pillar_trend_plot, 
        ncol = 2
      )
    }
    
  }, error = function(e) {
    warning("Error creating visualizations: ", e$message)
  })
  
  # Additional code for risk measures, correlation analysis, etc.
  # Omitted for brevity but would use the same error handling approach
  
  # Return all results
  return(results)
}

# Create a simplified version of the create_exploratory_report function
create_exploratory_report <- function(results, output_format = "latex") {
  
  library(kableExtra)
  library(gridExtra)
  library(cowplot)
  
  report <- list()
  
  # Include only tables and plots that exist
  if(!is.null(results$data_coverage)) 
    report$data_coverage <- results$data_coverage
  
  if(!is.null(results$country_composition)) 
    report$country_composition <- results$country_composition
  
  if(!is.null(results$summary_statistics)) 
    report$summary_statistics <- results$summary_statistics
  
  if(!is.null(results$yearly_esg)) 
    report$yearly_esg <- results$yearly_esg
  
  if(!is.null(results$esg_by_size)) 
    report$esg_by_size <- results$esg_by_size
  
  if(!is.null(results$esg_dist_plot)) 
    report$esg_dist_plot <- results$esg_dist_plot
  
  if(!is.null(results$esg_trends_plot)) 
    report$esg_trends_plot <- results$esg_trends_plot
  
  if(!is.null(results$risk_trend_plot)) 
    report$risk_trend_plot <- results$risk_trend_plot
  
  if(!is.null(results$risk_by_esg)) 
    report$risk_by_esg <- results$risk_by_esg
  
  if(!is.null(results$esg_cet1_plot)) 
    report$esg_cet1_plot <- results$esg_cet1_plot
  
  if(!is.null(results$pillar_leverage_plots)) 
    report$pillar_leverage_plots <- results$pillar_leverage_plots
  
  if(!is.null(results$change_analysis)) 
    report$change_analysis <- results$change_analysis
  
  if(!is.null(results$esg_change_plot)) 
    report$esg_change_plot <- results$esg_change_plot
  
  if(!is.null(results$risk_esg_change_plot)) 
    report$risk_esg_change_plot <- results$risk_esg_change_plot
  
  # Add individual visualizations if combined ones don't exist
  if(is.null(report$esg_dist_plot) && !is.null(results$visualizations)) {
    if(!is.null(results$visualizations$esg_hist))
      report$esg_hist <- results$visualizations$esg_hist
    
    if(!is.null(results$visualizations$pillar_boxplot))
      report$pillar_boxplot <- results$visualizations$pillar_boxplot
  }
  
  if(is.null(report$esg_trends_plot) && !is.null(results$visualizations)) {
    if(!is.null(results$visualizations$esg_trend_plot))
      report$esg_trend_plot <- results$visualizations$esg_trend_plot
    
    if(!is.null(results$visualizations$pillar_trend_plot))
      report$pillar_trend_plot <- results$visualizations$pillar_trend_plot
  }
  
  return(report)
}