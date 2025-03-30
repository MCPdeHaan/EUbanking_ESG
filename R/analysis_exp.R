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
  
  country_table <- banks_per_country %>%
    rename(
      "Country" = country,
      "Number of Banks" = unique_banks,
      "Total Observations" = total_obs
    )
  
  tryCatch({
    results$country_composition <- kable(country_table,
                                         caption = "Sample Composition by Country",
                                         booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped"),
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
  
  tryCatch({
    results$data_coverage <- kable(coverage_data,
                                   col.names = c("Category", "Value"),
                                   caption = "Data Coverage Summary",
                                   booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped"),
                    full_width = FALSE,
                    font_size = 9)
  }, error = function(e) {
    warning("Error creating data coverage table: ", e$message)
    results$data_coverage <- NULL
  })
  
  # ---- 2. Descriptive Statistics ----
  
  vars_for_summary <- c(
    "esg_score", "environmental_pillar_score", "social_pillar_score", "governance_pillar_score",
    risk_vars, control_vars
  )
  vars_for_summary <- intersect(vars_for_summary, names(data))
  
  summary_long <- data.frame()
  for (var in vars_for_summary) {
    values <- data[[var]]
    non_na_values <- values[!is.na(values)]
    
    if (length(non_na_values) > 0) {
      row <- data.frame(
        Variable = var,
        Mean = mean(non_na_values),
        SD = sd(non_na_values),
        Min = min(non_na_values),
        Q1 = quantile(non_na_values, 0.25),
        Median = median(non_na_values),
        Q3 = quantile(non_na_values, 0.75),
        Max = max(non_na_values),
        N = length(non_na_values)
      )
      summary_long <- rbind(summary_long, row)
    } else {
      row <- data.frame(
        Variable = var,
        Mean = NA, SD = NA, Min = NA, Q1 = NA, Median = NA, Q3 = NA, Max = NA, N = 0
      )
      summary_long <- rbind(summary_long, row)
    }
  }
  
  summary_long <- summary_long %>%
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
    mutate(across(c(Mean, SD, Min, Q1, Median, Q3, Max), ~round(as.numeric(.x), 3)))
  
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
  
  tryCatch({
    results$summary_statistics <- kable(summary_long,
                                        col.names = c("Variable", "Mean", "Std. Dev.", "Min", "Q1", "Median", "Q3", "Max", "N", "Category"),
                                        caption = "Descriptive Statistics for Key Variables",
                                        booktabs = TRUE,
                                        escape = FALSE) %>%
      kable_styling(latex_options = c("striped", "scale_down", "repeat_header"),
                    full_width = FALSE,
                    font_size = 9) %>%
      column_spec(1, width = "15em") %>%
      collapse_rows(columns = 10, valign = "top")
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
  
  tryCatch({
    results$yearly_esg <- kable(yearly_esg,
                                col.names = c("Year", "ESG Mean", "ESG Median", "ESG SD", 
                                              "Environmental", "Social", "Governance", "N"),
                                caption = "Yearly ESG Score Trends",
                                booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped"),
                    full_width = FALSE,
                    font_size = 9)
  }, error = function(e) {
    warning("Error creating yearly ESG table: ", e$message)
    results$yearly_esg <- NULL
  })
  
  # 2.3 ESG by bank size (using assets)
  if ("total_assets" %in% names(data) || "log_assets" %in% names(data)) {
    size_var <- if("total_assets" %in% names(data)) "total_assets" else "log_assets"
    
    if (size_var == "log_assets") {
      data <- data %>%
        mutate(
          real_assets_billions = exp(log_assets) / 1e9
        )
    } else {
      data <- data %>%
        mutate(
          real_assets_billions = total_assets / 1e9
        )
    }
    
    data <- data %>%
      group_by(year) %>% 
      mutate(
        size_quartile = ntile(real_assets_billions, 4),
        size_group = case_when(
          size_quartile == 1 ~ "Q1 (Smallest)",
          size_quartile == 2 ~ "Q2",
          size_quartile == 3 ~ "Q3",
          size_quartile == 4 ~ "Q4 (Largest)"
        )
      ) %>%
      ungroup()
    
    esg_by_size <- data %>%
      group_by(size_group) %>%
      summarize(
        ESG_Mean = mean(esg_score, na.rm = TRUE),
        ESG_Median = median(esg_score, na.rm = TRUE),
        ESG_SD = sd(esg_score, na.rm = TRUE),
        Env_Mean = mean(environmental_pillar_score, na.rm = TRUE),
        Soc_Mean = mean(social_pillar_score, na.rm = TRUE),
        Gov_Mean = mean(governance_pillar_score, na.rm = TRUE),
        Mean_Assets = mean(real_assets_billions, na.rm = TRUE),
        N = n(),
        .groups = "drop"
      ) %>%
      mutate(
        across(c(starts_with(c("ESG_", "Env_", "Soc_", "Gov_"))), ~round(., 2)),
        Mean_Assets = round(Mean_Assets, 1)
      ) %>%
      mutate(size_group = factor(size_group, 
                                 levels = c("Q1 (Smallest)", "Q2", "Q3", "Q4 (Largest)"))) %>%
      arrange(size_group)
    
    tryCatch({
      results$esg_by_size <- kable(esg_by_size,
                                   col.names = c("Size Group", "ESG Mean", "ESG Median", "ESG SD", 
                                                 "Environmental", "Social", "Governance", 
                                                 "Mean Assets (€B)", "N"),
                                   caption = "ESG Scores by Bank Size",
                                   booktabs = TRUE) %>%
        kable_styling(latex_options = c("striped"),
                      full_width = FALSE,
                      font_size = 9)
    }, error = function(e) {
      warning("Error creating ESG by size table: ", e$message)
      results$esg_by_size <- NULL
    })
  }
  
  # ---- 3. Visualizations ----
  
  tryCatch({
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
          axis.title = element_text(face = "bold"),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        )
      
      results$esg_hist <- esg_hist
    }
    
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
          legend.position = "none",
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        )
      
      results$pillar_boxplot <- pillar_boxplot
    }
    
    if("esg_score" %in% names(data) && length(unique(data$year)) > 1) {
      esg_trend_data <- data %>%
        group_by(year) %>%
        summarize(
          ESG = mean(esg_score, na.rm = TRUE),
          .groups = "drop"
        )
      
      esg_trend_plot <- ggplot(esg_trend_data, aes(x = year, y = ESG, group = 1)) +
        geom_line(color = "steelblue", size = 1) +
        geom_point(color = "steelblue", size = 3) +
        scale_x_continuous(breaks = unique(esg_trend_data$year)) +
        labs(
          title = "Trend in Average ESG Scores",
          x = "Year",
          y = "Average ESG Score"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          axis.title = element_text(face = "bold"),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        )
      
      results$esg_trend_plot <- esg_trend_plot
    }
    
    if(all(pillar_cols %in% names(data)) && length(unique(data$year)) > 1) {
      pillar_trend_data <- data %>%
        group_by(year) %>%
        summarize(
          Environmental = mean(environmental_pillar_score, na.rm = TRUE),
          Social = mean(social_pillar_score, na.rm = TRUE),
          Governance = mean(governance_pillar_score, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        pivot_longer(
          cols = c("Environmental", "Social", "Governance"),
          names_to = "Pillar",
          values_to = "Score"
        )
      
      pillar_trend_plot <- ggplot(pillar_trend_data, aes(x = year, y = Score, color = Pillar, group = Pillar)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        scale_x_continuous(breaks = unique(data$year)) +
        scale_color_manual(values = c("Environmental" = "#4DAF4A", "Social" = "#377EB8", "Governance" = "#E41A1C")) +
        labs(
          title = "Trends in ESG Pillar Scores",
          x = "Year",
          y = "Average Score"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          axis.title = element_text(face = "bold"),
          legend.position = "bottom",
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        )
      
      results$pillar_trend_plot <- pillar_trend_plot
    }
    
    risk_vars_available <- intersect(risk_vars, names(data))
    if(length(risk_vars_available) > 0 && length(unique(data$year)) > 1) {
      key_risk_vars <- intersect(c("cet1_risk_exposure", "leverage_ratio", "provisions_ratio"), risk_vars_available)
      
      if(length(key_risk_vars) > 0) {
        risk_trend_data <- data %>%
          group_by(year) %>%
          summarize(across(all_of(key_risk_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
          pivot_longer(cols = -year, names_to = "Metric", values_to = "Value")
        
        risk_trend_data <- risk_trend_data %>%
          mutate(Metric = case_when(
            Metric == "cet1_risk_exposure" ~ "CET1 Ratio",
            Metric == "leverage_ratio" ~ "Leverage Ratio",
            Metric == "provisions_ratio" ~ "Provisions Ratio",
            TRUE ~ Metric
          ))
        
        risk_trend_plot <- ggplot(risk_trend_data, aes(x = year, y = Value, color = Metric, group = Metric)) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          scale_x_continuous(breaks = unique(data$year)) +
          facet_wrap(~ Metric, scales = "free_y") +
          labs(
            title = "Trends in Key Risk Metrics",
            x = "Year",
            y = "Value"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(face = "bold", size = 12),
            axis.title = element_text(face = "bold"),
            legend.position = "none",
            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
          )
        
        results$risk_trend_plot <- risk_trend_plot
      }
    }
    
    if("esg_score" %in% names(data) && length(intersect(risk_vars, names(data))) > 0) {
      data <- data %>%
        mutate(esg_quartile = ntile(esg_score, 4),
               esg_group = case_when(
                 esg_quartile == 1 ~ "Q1 (Lowest ESG)",
                 esg_quartile == 2 ~ "Q2",
                 esg_quartile == 3 ~ "Q3",
                 esg_quartile == 4 ~ "Q4 (Highest ESG)"
               ))
      
      key_risk_vars <- intersect(c("cet1_risk_exposure", "leverage_ratio", "provisions_ratio"), names(data))
      
      if(length(key_risk_vars) > 0) {
        risk_by_esg <- data %>%
          group_by(esg_group) %>%
          summarize(across(all_of(key_risk_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
          mutate(across(all_of(key_risk_vars), ~round(.x, 3))) %>%
          mutate(esg_group = factor(esg_group, 
                                    levels = c("Q1 (Lowest ESG)", "Q2", "Q3", "Q4 (Highest ESG)"))) %>%
          arrange(esg_group)
        
        colnames(risk_by_esg) <- gsub("_", " ", tools::toTitleCase(colnames(risk_by_esg)))
        colnames(risk_by_esg)[1] <- "ESG Group"
        colnames(risk_by_esg) <- gsub("Cet1 Risk Exposure", "CET1 Ratio", colnames(risk_by_esg))
        
        tryCatch({
          results$risk_by_esg <- kable(risk_by_esg,
                                       caption = "Risk Metrics by ESG Score Quartile",
                                       booktabs = TRUE) %>%
            kable_styling(latex_options = c("striped"),
                          full_width = FALSE,
                          font_size = 9)
        }, error = function(e) {
          warning("Error creating risk by ESG table: ", e$message)
          results$risk_by_esg <- NULL
        })
      }
    }
    
    if("esg_score" %in% names(data) && "cet1_risk_exposure" %in% names(data)) {
      esg_cet1_plot <- ggplot(data, aes(x = esg_score, y = cet1_risk_exposure)) +
        geom_point(alpha = 0.6, color = "steelblue") +
        geom_smooth(method = "lm", color = "darkred", fill = "lightpink", alpha = 0.2) +
        labs(
          title = "Relationship Between ESG Score and CET1 Ratio",
          x = "ESG Score",
          y = "CET1 Ratio"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          axis.title = element_text(face = "bold"),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        )
      
      results$esg_cet1_plot <- esg_cet1_plot
    }
    
    if(all(pillar_cols %in% names(data)) && "leverage_ratio" %in% names(data)) {
      env_leverage_plot <- ggplot(data, aes(x = environmental_pillar_score, y = leverage_ratio)) +
        geom_point(alpha = 0.6, color = "#4DAF4A") +
        geom_smooth(method = "lm", color = "darkgreen", fill = "lightgreen", alpha = 0.2) +
        labs(
          title = "Environmental Score vs Leverage Ratio",
          x = "Environmental Score",
          y = "Leverage Ratio"
        ) +
        theme_minimal()
      
      soc_leverage_plot <- ggplot(data, aes(x = social_pillar_score, y = leverage_ratio)) +
        geom_point(alpha = 0.6, color = "#377EB8") +
        geom_smooth(method = "lm", color = "darkblue", fill = "lightblue", alpha = 0.2) +
        labs(
          title = "Social Score vs Leverage Ratio",
          x = "Social Score",
          y = "Leverage Ratio"
        ) +
        theme_minimal()
      
      gov_leverage_plot <- ggplot(data, aes(x = governance_pillar_score, y = leverage_ratio)) +
        geom_point(alpha = 0.6, color = "#E41A1C") +
        geom_smooth(method = "lm", color = "darkred", fill = "lightpink", alpha = 0.2) +
        labs(
          title = "Governance Score vs Leverage Ratio",
          x = "Governance Score",
          y = "Leverage Ratio"
        ) +
        theme_minimal()
      
      pillar_leverage_plots <- gridExtra::arrangeGrob(
        env_leverage_plot, soc_leverage_plot, gov_leverage_plot,
        ncol = 3
      )
      
      results$pillar_leverage_plots <- pillar_leverage_plots
    }
    
    if("esg_score" %in% names(data) && length(unique(data$year)) > 1) {
      data_with_lag <- data %>%
        arrange(lei_code, year) %>%
        group_by(lei_code) %>%
        mutate(
          esg_prev = lag(esg_score),
          esg_change = esg_score - esg_prev,
          cet1_prev = lag(cet1_risk_exposure),
          cet1_change = cet1_risk_exposure - cet1_prev
        ) %>%
        ungroup()
      
      change_analysis <- data_with_lag %>%
        filter(!is.na(esg_change), !is.na(cet1_change)) %>%
        summarize(
          esg_change_mean = mean(esg_change, na.rm = TRUE),
          esg_change_median = median(esg_change, na.rm = TRUE),
          cet1_change_mean = mean(cet1_change, na.rm = TRUE),
          cet1_change_median = median(cet1_change, na.rm = TRUE),
          positive_esg_change = sum(esg_change > 0, na.rm = TRUE),
          negative_esg_change = sum(esg_change < 0, na.rm = TRUE),
          positive_cet1_change = sum(cet1_change > 0, na.rm = TRUE),
          negative_cet1_change = sum(cet1_change < 0, na.rm = TRUE),
          correlation = cor(esg_change, cet1_change, use = "complete.obs"),
          .groups = "drop"
        ) %>%
        mutate(across(c(esg_change_mean, esg_change_median, cet1_change_mean, cet1_change_median, correlation), ~round(.x, 3)))
      
      change_table <- data.frame(
        Metric = c("Average ESG score change", "Median ESG score change",
                   "Average CET1 ratio change", "Median CET1 ratio change",
                   "Banks with improved ESG scores", "Banks with declined ESG scores",
                   "Banks with improved CET1 ratios", "Banks with declined CET1 ratios",
                   "Correlation between ESG and CET1 changes"),
        Value = c(change_analysis$esg_change_mean, change_analysis$esg_change_median,
                  change_analysis$cet1_change_mean, change_analysis$cet1_change_median,
                  change_analysis$positive_esg_change, change_analysis$negative_esg_change,
                  change_analysis$positive_cet1_change, change_analysis$negative_cet1_change,
                  change_analysis$correlation)
      )
      
      tryCatch({
        results$change_analysis <- kable(change_table,
                                         col.names = c("Metric", "Value"),
                                         caption = "Year-over-Year Change Analysis",
                                         booktabs = TRUE) %>%
          kable_styling(latex_options = c("striped"),
                        full_width = FALSE,
                        font_size = 9)
      }, error = function(e) {
        warning("Error creating change analysis table: ", e$message)
        results$change_analysis <- NULL
      })
      
      esg_change_plot <- ggplot(data_with_lag %>% filter(!is.na(esg_change)), 
                                aes(x = esg_change)) +
        geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.7) +
        geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed", size = 1) +
        labs(
          title = "Distribution of Year-over-Year ESG Score Changes",
          x = "ESG Score Change",
          y = "Frequency"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          axis.title = element_text(face = "bold"),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        )
      
      results$esg_change_plot <- esg_change_plot
      
      risk_esg_change_plot <- ggplot(data_with_lag %>% 
                                       filter(!is.na(esg_change), !is.na(cet1_change)), 
                                     aes(x = esg_change, y = cet1_change)) +
        geom_point(alpha = 0.6, color = "steelblue") +
        geom_smooth(method = "lm", color = "darkred", fill = "lightpink", alpha = 0.2) +
        geom_hline(aes(yintercept = 0), linetype = "dashed", color = "darkgray") +
        geom_vline(aes(xintercept = 0), linetype = "dashed", color = "darkgray") +
        labs(
          title = "Relationship Between ESG Score Changes and CET1 Ratio Changes",
          x = "ESG Score Change",
          y = "CET1 Ratio Change"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 12),
          axis.title = element_text(face = "bold"),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        )
      
      results$risk_esg_change_plot <- risk_esg_change_plot
    }
    
  }, error = function(e) {
    warning("Error creating visualizations: ", e$message)
  })
  
  return(results)
}

create_exploratory_report <- function(results, output_format = "latex") {
  
  library(kableExtra)
  library(gridExtra)
  library(cowplot)
  
  report <- list()
  
  if(!is.null(results$data_coverage)) {
    report$data_coverage <- results$data_coverage
  }
  
  if(!is.null(results$country_composition)) 
    report$country_composition <- results$country_composition
  
  if(!is.null(results$summary_statistics)) 
    report$summary_statistics <- results$summary_statistics
  
  if(!is.null(results$yearly_esg)) 
    report$yearly_esg <- results$yearly_esg
  
  if(!is.null(results$esg_by_size)) 
    report$esg_by_size <- results$esg_by_size
  
  if(!is.null(results$risk_by_esg)) 
    report$risk_by_esg <- results$risk_by_esg
  
  if(!is.null(results$change_analysis)) 
    report$change_analysis <- results$change_analysis
  
  plot_names <- c("esg_hist", "pillar_boxplot", "esg_trend_plot", "pillar_trend_plot", 
                  "risk_trend_plot", "esg_cet1_plot", "pillar_leverage_plots", 
                  "esg_change_plot", "risk_esg_change_plot")
  
  for(plot_name in plot_names) {
    if(!is.null(results[[plot_name]])) 
      report[[plot_name]] <- results[[plot_name]]
  }
  
  return(report)
}
