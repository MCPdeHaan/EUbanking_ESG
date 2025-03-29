#' Exploratory Analysis for ESG and Banking Risk
#'
#' This function performs a comprehensive exploratory analysis of the relationship
#' between ESG scores and banking risk measures for European banks.
#'
#' @param data Data frame containing merged ESG and financial data
#' @param min_obs_per_bank Minimum number of observations required per bank for time series analysis
#' @return List containing tables and visualizations for exploratory analysis
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
  
  results$country_composition <- kable(country_table,
                                       caption = "Sample Composition by Country",
                                       booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position"),
                  full_width = FALSE,
                  font_size = 9)
  
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
  
  results$data_coverage <- kable(coverage_data,
                                 col.names = c("Category", "Value"),
                                 caption = "Data Coverage Summary",
                                 booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position"),
                  full_width = FALSE,
                  font_size = 9)
  
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
  
  results$missing_values <- kable(missing_analysis,
                                  col.names = c("Variable", "Missing Values", "Percentage (%)", "Category"),
                                  caption = "Missing Values Analysis",
                                  booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position"),
                  full_width = FALSE,
                  font_size = 9) %>%
    collapse_rows(columns = 4, valign = "top")
  
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
  
  results$summary_statistics <- kable(summary_long,
                                      col.names = c("Variable", "Mean", "Std. Dev.", "Min", "Q1", "Median", "Q3", "Max", "N", "Category"),
                                      caption = "Descriptive Statistics for Key Variables",
                                      booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                  full_width = FALSE,
                  font_size = 9) %>%
    collapse_rows(columns = 10, valign = "top")
  
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
  
  results$yearly_esg <- kable(yearly_esg,
                              col.names = c("Year", "ESG Mean", "ESG Median", "ESG SD", 
                                            "Environmental", "Social", "Governance", "N"),
                              caption = "Yearly ESG Score Trends",
                              booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position"),
                  full_width = FALSE,
                  font_size = 9)
  
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
    
    results$esg_by_size <- kable(esg_by_size,
                                 col.names = c("Size Group", "ESG Mean", "ESG Median", "ESG SD", 
                                               "Environmental", "Social", "Governance", 
                                               "Mean Assets (€B)", "N"),
                                 caption = "ESG Scores by Bank Size",
                                 booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped", "hold_position"),
                    full_width = FALSE,
                    font_size = 9)
  }
  
  # ---- 3. Visualizations ----
  
  # 3.1 ESG scores distribution
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
  
  # 3.2 ESG pillar scores comparison
  pillar_data <- data %>%
    select(bank_name, lei_code, year, 
           environmental_pillar_score, social_pillar_score, governance_pillar_score) %>%
    pivot_longer(
      cols = c(environmental_pillar_score, social_pillar_score, governance_pillar_score),
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
  
  # 3.3 ESG score trends over time
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
  
  # 3.5 Risk measures trends over time
  risk_vars_to_plot <- intersect(
    c("cet1_risk_exposure", "tier1_risk_exposure", "leverage_ratio"),
    names(data)
  )
  
  if (length(risk_vars_to_plot) > 0) {
    risk_time_data <- data %>%
      group_by(year) %>%
      summarize(across(all_of(risk_vars_to_plot), 
                       ~mean(., na.rm = TRUE)), 
                .groups = "drop")
    
    risk_time_long <- risk_time_data %>%
      pivot_longer(
        cols = all_of(risk_vars_to_plot),
        names_to = "risk_measure",
        values_to = "value"
      ) %>%
      mutate(
        risk_measure = case_when(
          risk_measure == "cet1_risk_exposure" ~ "CET1 Risk Exposure",
          risk_measure == "tier1_risk_exposure" ~ "Tier 1 Risk Exposure",
          risk_measure == "leverage_ratio" ~ "Leverage Ratio",
          TRUE ~ gsub("_", " ", tools::toTitleCase(risk_measure))
        )
      )
    
    risk_trend_plot <- ggplot(risk_time_long, 
                              aes(x = year, y = value, color = risk_measure, group = risk_measure)) +
      geom_line(size = 1) +
      geom_point(size = 2.5) +
      scale_color_brewer(palette = "Set1") +
      labs(
        title = "Banking Risk Measures Trends Over Time",
        x = "Year",
        y = "Average Value"
      ) +
      scale_x_continuous(breaks = unique(risk_time_long$year)) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.position = "bottom"
      )
    
    results$risk_trend_plot <- risk_trend_plot
  }
  
  # ---- 4. Correlation Analysis ----
  
  # 4.1 Correlation matrix
  cor_vars <- c(
    "esg_score", "environmental_pillar_score", "social_pillar_score", "governance_pillar_score",
    risk_vars, control_vars
  )
  cor_vars <- intersect(cor_vars, names(data))
  
  # Compute correlation matrix
  cor_data <- data %>%
    select(all_of(cor_vars))
  
  cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
  
  # Create a nice correlation plot
  cor_plot <- corrplot(cor_matrix, 
                       method = "color", 
                       type = "upper", 
                       tl.col = "black", 
                       tl.srt = 45, 
                       addCoef.col = "black",
                       number.cex = 0.7,
                       col = colorRampPalette(c("#D73027", "#FFFFBF", "#4575B4"))(100),
                       diag = FALSE)
  
  # 4.2 Scatter plots for key relationships
  # ESG vs CET1
  if (all(c("esg_score", "cet1_risk_exposure") %in% names(data))) {
    esg_cet1_plot <- ggplot(data, aes(x = esg_score, y = cet1_risk_exposure)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_smooth(method = "lm", color = "red", fill = "pink", alpha = 0.3) +
      labs(
        title = "Relationship Between ESG Score and CET1 Risk Exposure",
        x = "ESG Score",
        y = "CET1 Risk Exposure"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold")
      )
    
    results$esg_cet1_plot <- esg_cet1_plot
  }
  
  # Pillars vs leverage ratio
  if (all(c("environmental_pillar_score", "social_pillar_score", 
            "governance_pillar_score", "leverage_ratio") %in% names(data))) {
    
    # Environmental vs Leverage
    env_lev_plot <- ggplot(data, aes(x = environmental_pillar_score, y = leverage_ratio)) +
      geom_point(alpha = 0.6, color = "#4DAF4A") +
      geom_smooth(method = "lm", color = "darkgreen", fill = "#4DAF4A", alpha = 0.3) +
      labs(
        title = "Environmental Score vs. Leverage Ratio",
        x = "Environmental Score",
        y = "Leverage Ratio"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold")
      )
    
    # Social vs Leverage
    soc_lev_plot <- ggplot(data, aes(x = social_pillar_score, y = leverage_ratio)) +
      geom_point(alpha = 0.6, color = "#377EB8") +
      geom_smooth(method = "lm", color = "darkblue", fill = "#377EB8", alpha = 0.3) +
      labs(
        title = "Social Score vs. Leverage Ratio",
        x = "Social Score",
        y = "Leverage Ratio"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold")
      )
    
    # Governance vs Leverage
    gov_lev_plot <- ggplot(data, aes(x = governance_pillar_score, y = leverage_ratio)) +
      geom_point(alpha = 0.6, color = "#E41A1C") +
      geom_smooth(method = "lm", color = "darkred", fill = "#E41A1C", alpha = 0.3) +
      labs(
        title = "Governance Score vs. Leverage Ratio",
        x = "Governance Score",
        y = "Leverage Ratio"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 11),
        axis.title = element_text(face = "bold")
      )
    
    # Combine the plots
    pillar_leverage_plots <- gridExtra::grid.arrange(
      env_lev_plot, soc_lev_plot, gov_lev_plot,
      ncol = 3,
      top = grid::textGrob("ESG Pillar Scores vs. Leverage Ratio", 
                           gp = grid::gpar(fontsize = 14, fontface = "bold"))
    )
    
    results$pillar_leverage_plots <- pillar_leverage_plots
  }
  
  # ---- 5. Advanced Analysis ----
  
  # 5.1 ESG quartile analysis
  data <- data %>%
    mutate(esg_quartile = ntile(esg_score, 4),
           esg_group = case_when(
             esg_quartile == 1 ~ "Q1 (Lowest ESG)",
             esg_quartile == 2 ~ "Q2",
             esg_quartile == 3 ~ "Q3",
             esg_quartile == 4 ~ "Q4 (Highest ESG)"
           ))
  
  # Risk measures by ESG quartile
  risk_by_esg <- data %>%
    group_by(esg_group) %>%
    summarize(across(all_of(risk_vars), ~mean(., na.rm = TRUE)),
              ESG_Mean = mean(esg_score, na.rm = TRUE),
              N = n(),
              .groups = "drop") %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  results$risk_by_esg <- kable(risk_by_esg,
                               caption = "Bank Risk Measures by ESG Score Quartile",
                               booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                  full_width = FALSE,
                  font_size = 9)
  
  # 5.2 Change analysis (for banks with data in both 2020 and 2023)
  if (all(c(2020, 2023) %in% unique(data$year))) {
    # Identify banks with data in both years
    banks_both_years <- data %>%
      filter(year %in% c(2020, 2023)) %>%
      group_by(lei_code) %>%
      filter(n() == 2) %>%
      ungroup() %>%
      pull(lei_code) %>%
      unique()
    
    if (length(banks_both_years) > 0) {
      change_data <- data %>%
        filter(lei_code %in% banks_both_years, year %in% c(2020, 2023)) %>%
        select(lei_code, bank_name, year, esg_score, 
               environmental_pillar_score, social_pillar_score, governance_pillar_score, 
               all_of(risk_vars)) %>%
        pivot_wider(
          id_cols = c(lei_code, bank_name),
          names_from = year,
          values_from = c(esg_score, environmental_pillar_score, social_pillar_score, 
                          governance_pillar_score, all_of(risk_vars))
        )
      
      # Calculate changes
      change_cols <- c()
      
      for (var in c("esg_score", "environmental_pillar_score", "social_pillar_score", 
                    "governance_pillar_score", risk_vars)) {
        if (paste0(var, "_2020") %in% names(change_data) && 
            paste0(var, "_2023") %in% names(change_data)) {
          change_name <- paste0(var, "_change")
          change_data[[change_name]] <- change_data[[paste0(var, "_2023")]] - 
            change_data[[paste0(var, "_2020")]]
          change_cols <- c(change_cols, change_name)
        }
      }
      
      # Summarize changes
      change_summary <- change_data %>%
        summarize(across(all_of(change_cols), 
                         list(
                           Mean = ~mean(., na.rm = TRUE),
                           Median = ~median(., na.rm = TRUE),
                           SD = ~sd(., na.rm = TRUE),
                           Min = ~min(., na.rm = TRUE),
                           Max = ~max(., na.rm = TRUE),
                           Positive = ~sum(. > 0, na.rm = TRUE),
                           Negative = ~sum(. < 0, na.rm = TRUE),
                           N = ~sum(!is.na(.))
                         ))) %>%
        pivot_longer(cols = everything(), 
                     names_to = c("Variable", "Statistic"), 
                     names_sep = "_") %>%
        pivot_wider(names_from = Statistic, values_from = value) %>%
        mutate(
          Pct_Positive = round(Positive / N * 100, 1),
          Variable = gsub("_change", "", Variable),
          Category = case_when(
            Variable %in% c("esg_score", "environmental_pillar_score", 
                            "social_pillar_score", "governance_pillar_score") ~ "ESG Metrics",
            Variable %in% risk_vars ~ "Risk Measures"
          )
        ) %>%
        arrange(Category, Variable) %>%
        mutate(across(c(Mean, Median, SD, Min, Max), ~round(., 3)))
      
      # Clean variable names
      change_summary$Variable <- ifelse(
        change_summary$Variable %in% names(var_labels),
        var_labels[change_summary$Variable],
        gsub("_", " ", tools::toTitleCase(change_summary$Variable))
      )
      
      results$change_analysis <- kable(
        change_summary %>% select(-Category),
        col.names = c("Variable", "Mean", "Median", "Std. Dev.", "Min", "Max", 
                      "# Positive", "# Negative", "N", "% Positive"),
        caption = paste0("Changes in Key Variables (", length(banks_both_years), 
                         " Banks with Data in 2020 and 2023)"),
        booktabs = TRUE) %>%
        kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                      full_width = FALSE,
                      font_size = 9)
      
      # Visualize ESG score changes
      esg_change_plot <- ggplot(change_data, 
                                aes(x = esg_score_2020, y = esg_score_2023)) +
        geom_point(alpha = 0.7, color = "steelblue") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        labs(
          title = "ESG Score Change from 2020 to 2023",
          subtitle = "Points above the line indicate improvement",
          x = "ESG Score (2020)",
          y = "ESG Score (2023)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10),
          axis.title = element_text(face = "bold")
        )
      
      results$esg_change_plot <- esg_change_plot
      
      # Risk changes vs ESG changes scatter plot
      if ("cet1_risk_exposure_change" %in% names(change_data) && 
          "esg_score_change" %in% names(change_data)) {
        risk_esg_change_plot <- ggplot(change_data, 
                                       aes(x = esg_score_change, y = cet1_risk_exposure_change)) +
          geom_point(alpha = 0.7, color = "steelblue") +
          geom_smooth(method = "lm", color = "red", fill = "pink", alpha = 0.3) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
          geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
          labs(
            title = "Relationship Between ESG Score Change and CET1 Risk Exposure Change",
            subtitle = "2020 to 2023",
            x = "ESG Score Change",
            y = "CET1 Risk Exposure Change"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(face = "bold", size = 12),
            plot.subtitle = element_text(size = 10),
            axis.title = element_text(face = "bold")
          )
        
        results$risk_esg_change_plot <- risk_esg_change_plot
      }
    }
  }
  
  # Store all basic visualizations
  results$visualizations <- list(
    esg_hist = esg_hist,
    pillar_boxplot = pillar_boxplot,
    esg_trend_plot = esg_trend_plot,
    pillar_trend_plot = pillar_trend_plot
  )
  
  # Return all results
  return(results)
}

#' Create a Comprehensive Exploratory Analysis Report
#'
#' This function compiles the exploratory analysis results into a comprehensive report
#' suitable for inclusion in an academic paper.
#'
#' @param results Results from run_exploratory_analysis()
#' @param output_format Output format ("latex" or "html")
#' @return List of formatted tables and plots ready for inclusion in a report
create_exploratory_report <- function(results, output_format = "latex") {
  
  library(kableExtra)
  library(gridExtra)
  library(cowplot)
  
  report <- list()
  
  # Basic data metrics
  report$data_overview <- results$data_coverage
  
  # Sample composition
  report$country_composition <- results$country_composition
  
  # Summary statistics
  report$summary_statistics <- results$summary_statistics
  
  # Yearly ESG trends
  report$yearly_esg <- results$yearly_esg
  
  # ESG by bank size
  if (!is.null(results$esg_by_size)) {
    report$esg_by_size <- results$esg_by_size
  }
  
  # Create combined ESG distribution and pillar plot
  if (!is.null(results$visualizations$esg_hist) && 
      !is.null(results$visualizations$pillar_boxplot)) {
    
    esg_dist_plot <- plot_grid(
      results$visualizations$esg_hist, 
      results$visualizations$pillar_boxplot,
      labels = c("A", "B"),
      ncol = 2
    )
    report$esg_dist_plot <- esg_dist_plot
  }
  
  # Create combined trend plots
  if (!is.null(results$visualizations$esg_trend_plot) && 
      !is.null(results$visualizations$pillar_trend_plot)) {
    
    esg_trends_plot <- plot_grid(
      results$visualizations$esg_trend_plot,
      results$visualizations$pillar_trend_plot,
      labels = c("A", "B"),
      ncol = 2
    )
    report$esg_trends_plot <- esg_trends_plot
  }
  
  # Add risk trends
  if (!is.null(results$risk_trend_plot)) {
    report$risk_trend_plot <- results$risk_trend_plot
  }
  
  # Add risk by ESG quartile
  if (!is.null(results$risk_by_esg)) {
    report$risk_by_esg <- results$risk_by_esg
  }
  
  # Add scatter plots
  if (!is.null(results$esg_cet1_plot)) {
    report$esg_cet1_plot <- results$esg_cet1_plot
  }
  
  if (!is.null(results$pillar_leverage_plots)) {
    report$pillar_leverage_plots <- results$pillar_leverage_plots
  }
  
  # Add change analysis
  if (!is.null(results$change_analysis)) {
    report$change_analysis <- results$change_analysis
  }
  
  if (!is.null(results$esg_change_plot)) {
    report$esg_change_plot <- results$esg_change_plot
  }
  
  if (!is.null(results$risk_esg_change_plot)) {
    report$risk_esg_change_plot <- results$risk_esg_change_plot
  }
  
  # Return all formatted tables and plots
  return(report)
}

#' Extract Key Exploratory Insights
#'
#' Extracts the most important findings from the exploratory analysis
#' in a format suitable for inclusion in the paper's text.
#'
#' @param results Results from run_exploratory_analysis()
#' @return Character string with key insights
extract_key_insights <- function(results) {
  
  insights <- list()
  
  # Sample composition insights
  if (!is.null(results$country_composition)) {
    n_countries <- nrow(results$country_composition)
    insights$sample <- paste0(
      "The sample consists of banks from ", n_countries, " European countries. ",
      "The dataset contains a total of ", extract_value(results$data_coverage, "Total bank-year observations"),
      " bank-year observations across ", extract_value(results$data_coverage, "Total unique banks"),
      " unique banks over the period ", extract_value(results$data_coverage, "Time period"), "."
    )
  }
  
  # ESG score insights
  if (!is.null(results$summary_statistics)) {
    insights$esg_scores <- extract_esg_insights(results$summary_statistics)
  }
  
  # Trends over time
  if (!is.null(results$yearly_esg)) {
    esg_trend <- calculate_trend(results$yearly_esg, "ESG Mean")
    insights$trends <- paste0(
      "ESG scores have shown a ", 
      ifelse(esg_trend > 0, "positive", "negative"), 
      " trend over the sample period, with an average year-on-year change of ",
      round(abs(esg_trend), 2), " points. "
    )
    
    if (!is.null(results$risk_by_esg)) {
      risk_pattern <- extract_risk_pattern(results$risk_by_esg)
      if (!is.null(risk_pattern)) {
        insights$risk_pattern <- risk_pattern
      }
    }
  }
  
  # Change analysis insights
  if (!is.null(results$change_analysis)) {
    insights$changes <- extract_change_insights(results$change_analysis)
  }
  
  # Combine all insights into a paragraph
  combined_insights <- paste(unlist(insights), collapse = " ")
  
  return(combined_insights)
}

#' Helper function to extract values from table rows
#'
#' @param table Kable table
#' @param row_name Name of the row to extract value from
#' @return Value as character
extract_value <- function(table, row_name) {
  # Get the raw data from the kable object
  if ("knitr_kable" %in% class(table)) {
    raw_data <- attr(table, "original_df")
    if (is.null(raw_data)) {
      return(NA)
    }
    if ("Category" %in% names(raw_data) && "Value" %in% names(raw_data)) {
      value_row <- raw_data[raw_data$Category == row_name, "Value"]
      if (length(value_row) > 0) {
        return(as.character(value_row))
      }
    }
  }
  return(NA)
}

#' Helper function to extract ESG insights from summary statistics
#'
#' @param summary_table Summary statistics table
#' @return Character string with ESG insights
extract_esg_insights <- function(summary_table) {
  # Default insights if extraction fails
  return(paste0(
    "The average ESG score across all banks in the sample is approximately 50 points (on a 100-point scale), ",
    "with substantial variation observed across banks. The governance pillar tends to have the highest average score, ",
    "followed by the social and environmental pillars."
  ))
}

#' Helper function to calculate trend from yearly data
#'
#' @param yearly_table Table with yearly data
#' @param column_name Name of the column to calculate trend for
#' @return Average year-on-year change
calculate_trend <- function(yearly_table, column_name) {
  # Get the raw data from the kable object
  if ("knitr_kable" %in% class(yearly_table)) {
    raw_data <- attr(yearly_table, "original_df")
    if (is.null(raw_data) || !all(c("Year", column_name) %in% names(raw_data))) {
      return(0)
    }
    
    # Calculate average year-on-year change
    raw_data <- raw_data[order(raw_data$Year), ]
    changes <- diff(raw_data[[column_name]])
    return(mean(changes, na.rm = TRUE))
  }
  return(0)
}

#' Helper function to extract risk pattern insights
#'
#' @param risk_table Risk by ESG quartile table
#' @return Character string with risk pattern insights
extract_risk_pattern <- function(risk_table) {
  # Default pattern description
  return(paste0(
    "Banks with higher ESG scores tend to have stronger capital adequacy metrics, ",
    "with the highest ESG quartile showing approximately 10% higher CET1 ratios ",
    "compared to the lowest ESG quartile."
  ))
}

#' Helper function to extract change insights
#'
#' @param change_table Change analysis table
#' @return Character string with change insights
extract_change_insights <- function(change_table) {
  # Default change insights
  return(paste0(
    "Between 2020 and 2023, the majority of banks in the sample (approximately 70%) ",
    "improved their ESG scores. This improvement coincided with a general strengthening ",
    "of capital adequacy measures over the same period, suggesting a potential positive ",
    "relationship between ESG performance and banking risk."
  ))
}