# analysis_iv.R

# Load required packages
library(plm)
library(dplyr)
library(modelsummary)
library(kableExtra)
library(tidyr)
library(ggplot2)
library(tibble)
library(AER)  # For ivreg

# Utility function to get nice display names for dependent variables
get_nice_dep_name <- function(var_name) {
  nice_names <- c(
    "cet1_risk_exposure" = "CET1 Risk Exposure",
    "tier1_risk_exposure" = "Tier 1 Risk Exposure",
    "totalcap_risk_exposure" = "Total Capital Risk Exposure",
    "leverage_ratio" = "Leverage Ratio",
    "provisions" = "Provisions",
    "provisions_ratio" = "Provisions Ratio",
    "liquidity_ratio" = "Liquidity Ratio",
    "rwa_ratio" = "RWA Ratio"
  )
  if (var_name %in% names(nice_names)) {
    return(nice_names[var_name])
  } else {
    return(var_name)
  }
}

# Main function for IV analysis
run_iv_analysis <- function(data, 
                            dep_vars = c("cet1_risk_exposure", "tier1_risk_exposure", 
                                         "totalcap_risk_exposure", "leverage_ratio", "provisions", 
                                         "provisions_ratio", "liquidity_ratio", "rwa_ratio")) {
  
  # -------------------------------
  # Step 0: Validate Input Data
  # -------------------------------
  if (is.null(data) || nrow(data) == 0) {
    warning("Empty or NULL dataset provided. Cannot proceed with IV analysis.")
    return(NULL)
  }
  
  required_columns <- c("esg_score", "environmental_pillar_score", "social_pillar_score", 
                        "governance_pillar_score", "log_assets", "equity_to_assets", "year", "lei_code")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    warning(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
    return(NULL)
  }
  
  available_dep_vars <- intersect(dep_vars, colnames(data))
  if (length(available_dep_vars) == 0) {
    warning("None of the specified dependent variables are present in the dataset.")
    return(NULL)
  }
  
  # Keep complete cases for the essential variables and dependent variables
  data <- data %>% 
    select(all_of(c("lei_code", required_columns, available_dep_vars))) %>%
    na.omit()
  
  if (nrow(data) == 0) {
    warning("No complete observations found after removing NA values.")
    return(NULL)
  }
  
  # -------------------------------
  # Step 1: Create Instrument Variables
  # -------------------------------
  # If no explicit country column, extract from first two characters of LEI code
  if (!"country" %in% colnames(data)) {
    data$country <- substr(data$lei_code, 1, 2)
  }
  data$country_year <- paste(data$country, data$year, sep = "_")
  
  # Compute leave-one-out averages for ESG scores by country-year
  instruments_data <- data %>%
    group_by(country_year) %>%
    mutate(
      country_year_count = n(),
      instr_esg_score = ifelse(country_year_count > 1, 
                               (sum(esg_score, na.rm = TRUE) - esg_score) / (sum(!is.na(esg_score)) - 1),
                               NA),
      instr_environmental_pillar_score = ifelse(country_year_count > 1,
                                                (sum(environmental_pillar_score, na.rm = TRUE) - environmental_pillar_score) / 
                                                  (sum(!is.na(environmental_pillar_score)) - 1),
                                                NA),
      instr_social_pillar_score = ifelse(country_year_count > 1,
                                         (sum(social_pillar_score, na.rm = TRUE) - social_pillar_score) / 
                                           (sum(!is.na(social_pillar_score)) - 1),
                                         NA),
      instr_governance_pillar_score = ifelse(country_year_count > 1,
                                             (sum(governance_pillar_score, na.rm = TRUE) - governance_pillar_score) / 
                                               (sum(!is.na(governance_pillar_score)) - 1),
                                             NA)
    ) %>%
    ungroup()
  
  # For groups with only one bank, fill missing instrument values using country means
  instruments_data$country <- sub("_.*", "", instruments_data$country_year)
  country_means <- instruments_data %>%
    group_by(country) %>%
    summarize(
      mean_esg = mean(esg_score, na.rm = TRUE),
      mean_env = mean(environmental_pillar_score, na.rm = TRUE),
      mean_soc = mean(social_pillar_score, na.rm = TRUE),
      mean_gov = mean(governance_pillar_score, na.rm = TRUE),
      .groups = "drop"
    )
  instruments_data <- instruments_data %>%
    left_join(country_means, by = "country") %>%
    mutate(
      instr_esg_score = ifelse(is.na(instr_esg_score), mean_esg, instr_esg_score),
      instr_environmental_pillar_score = ifelse(is.na(instr_environmental_pillar_score), mean_env, instr_environmental_pillar_score),
      instr_social_pillar_score = ifelse(is.na(instr_social_pillar_score), mean_soc, instr_social_pillar_score),
      instr_governance_pillar_score = ifelse(is.na(instr_governance_pillar_score), mean_gov, instr_governance_pillar_score)
    ) %>%
    select(-mean_esg, -mean_env, -mean_soc, -mean_gov)
  
  # -------------------------------
  # Step 2: Estimate IV Models
  # -------------------------------
  iv_results_list <- list()
  for (dep_var in available_dep_vars) {
    tryCatch({
      # Convert to panel data (for IV estimation)
      panel_data <- pdata.frame(instruments_data, index = c("lei_code", "year"))
      
      # First stage for overall ESG score
      first_stage_esg <- plm(esg_score ~ instr_esg_score + log_assets + equity_to_assets, 
                             data = panel_data, 
                             model = "within", 
                             effect = "twoways")
      panel_data$pred_esg_score <- fitted(first_stage_esg)
      
      # Second stage regression using predicted ESG score
      second_stage_esg <- plm(as.formula(paste0(dep_var, " ~ pred_esg_score + log_assets + equity_to_assets")),
                              data = panel_data,
                              model = "within",
                              effect = "twoways")
      
      # Prepare storage for this risk measure
      iv_results_list[[dep_var]] <- list(
        first_stage = first_stage_esg,
        second_stage = second_stage_esg
      )
      
      # First stages for ESG pillar scores (if possible)
      first_stage_env <- tryCatch({
        plm(environmental_pillar_score ~ instr_environmental_pillar_score + log_assets + equity_to_assets,
            data = panel_data, 
            model = "within", 
            effect = "twoways")
      }, error = function(e) NULL)
      
      first_stage_soc <- tryCatch({
        plm(social_pillar_score ~ instr_social_pillar_score + log_assets + equity_to_assets,
            data = panel_data, 
            model = "within", 
            effect = "twoways")
      }, error = function(e) NULL)
      
      first_stage_gov <- tryCatch({
        plm(governance_pillar_score ~ instr_governance_pillar_score + log_assets + equity_to_assets,
            data = panel_data, 
            model = "within", 
            effect = "twoways")
      }, error = function(e) NULL)
      
      # If all pillar first stages are estimated, run second stage for pillars
      if (!is.null(first_stage_env) && !is.null(first_stage_soc) && !is.null(first_stage_gov)) {
        panel_data$pred_env_score <- fitted(first_stage_env)
        panel_data$pred_soc_score <- fitted(first_stage_soc)
        panel_data$pred_gov_score <- fitted(first_stage_gov)
        
        second_stage_pillars <- tryCatch({
          plm(as.formula(paste0(dep_var, " ~ pred_env_score + pred_soc_score + pred_gov_score + log_assets + equity_to_assets")),
              data = panel_data,
              model = "within",
              effect = "twoways")
        }, error = function(e) NULL)
        
        if (!is.null(second_stage_pillars)) {
          iv_results_list[[dep_var]]$first_stage_pillars <- list(
            env = first_stage_env,
            soc = first_stage_soc,
            gov = first_stage_gov
          )
          iv_results_list[[dep_var]]$second_stage_pillars <- second_stage_pillars
        }
      }
    }, error = function(e) {
      warning(paste("Error in IV analysis for", dep_var, ":", e$message))
    })
  }
  
  # -------------------------------
  # Step 3: Estimate Fixed Effects Models for Comparison
  # -------------------------------
  fe_results_list <- list()
  for (dep_var in names(iv_results_list)) {
    tryCatch({
      panel_data <- pdata.frame(data, index = c("lei_code", "year"))
      # Fixed Effects for overall ESG score
      fe_esg <- plm(as.formula(paste0(dep_var, " ~ esg_score + log_assets + equity_to_assets")),
                    data = panel_data,
                    model = "within",
                    effect = "twoways")
      # Fixed Effects for individual ESG pillars
      fe_pillars <- plm(as.formula(paste0(dep_var, " ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_assets + equity_to_assets")),
                        data = panel_data,
                        model = "within",
                        effect = "twoways")
      fe_results_list[[dep_var]] <- list(
        fe_esg = fe_esg,
        fe_pillars = fe_pillars
      )
    }, error = function(e) {
      warning(paste("Error in FE analysis for", dep_var, ":", e$message))
    })
  }
  
  # -------------------------------
  # Step 4: Create Summary Tables & Comparison Tables
  # -------------------------------
  # Set up coefficient maps for better labels
  coef_map_esg <- c(
    "esg_score" = "ESG Score (FE)",
    "pred_esg_score" = "ESG Score (IV)",
    "log_assets" = "Log Assets",
    "equity_to_assets" = "Equity to Assets"
  )
  
  coef_map_pillars <- c(
    "environmental_pillar_score" = "Environmental Score (FE)",
    "social_pillar_score" = "Social Score (FE)",
    "governance_pillar_score" = "Governance Score (FE)",
    "pred_env_score" = "Environmental Score (IV)",
    "pred_soc_score" = "Social Score (IV)",
    "pred_gov_score" = "Governance Score (IV)",
    "log_assets" = "Log Assets",
    "equity_to_assets" = "Equity to Assets"
  )
  
  # Goodness-of-fit map for modelsummary
  gof_map <- tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "Observations",  0,
    "r.squared", "R²",            3
  )
  
  comparison_tables <- list()
  for (dep_var in names(iv_results_list)) {
    # Only create comparison tables if both FE and IV models exist
    if (!dep_var %in% names(fe_results_list)) next
    if (is.null(fe_results_list[[dep_var]]$fe_esg) || is.null(iv_results_list[[dep_var]]$second_stage))
      next
    
    # Table comparing overall ESG models
    models_esg <- list(
      "Fixed Effects" = fe_results_list[[dep_var]]$fe_esg,
      "Instrumental Variable" = iv_results_list[[dep_var]]$second_stage
    )
    comparison_tables[[paste0(dep_var, "_esg")]] <- modelsummary(
      models_esg,
      title = paste("Comparison of FE and IV Estimates for", get_nice_dep_name(dep_var), "- ESG Score"),
      stars = c('†' = 0.1, '*' = 0.05, '**' = 0.01, '***' = 0.001),
      gof_map = gof_map,
      coef_map = coef_map_esg,
      fmt = 3,
      output = "kableExtra",
      note = "Bank and year fixed effects included in all models. †p<0.1; *p<0.05; **p<0.01; ***p<0.001",
      add_rows = tibble(
        term = c("Bank FE", "Year FE"),
        "Fixed Effects" = c("Yes", "Yes"),
        "Instrumental Variable" = c("Yes", "Yes")
      )
    ) %>% 
      kable_styling(latex_options = c("HOLD_position"), full_width = FALSE, font_size = 9)
    
    # Table for pillar models if available
    if (!is.null(fe_results_list[[dep_var]]$fe_pillars) &&
        !is.null(iv_results_list[[dep_var]]$second_stage_pillars)) {
      models_pillars <- list(
        "Fixed Effects" = fe_results_list[[dep_var]]$fe_pillars,
        "Instrumental Variable" = iv_results_list[[dep_var]]$second_stage_pillars
      )
      comparison_tables[[paste0(dep_var, "_pillars")]] <- modelsummary(
        models_pillars,
        title = paste("Comparison of FE and IV Estimates for", get_nice_dep_name(dep_var), "- ESG Pillar Scores"),
        stars = c('†' = 0.1, '*' = 0.05, '**' = 0.01, '***' = 0.001),
        gof_map = gof_map,
        coef_map = coef_map_pillars,
        fmt = 3,
        output = "kableExtra",
        note = "Bank and year fixed effects included in all models. †p<0.1; *p<0.05; **p<0.01; ***p<0.001",
        add_rows = tibble(
          term = c("Bank FE", "Year FE"),
          "Fixed Effects" = c("Yes", "Yes"),
          "Instrumental Variable" = c("Yes", "Yes")
        )
      ) %>% 
        kable_styling(latex_options = c("HOLD_position"), full_width = FALSE, font_size = 9)
    }
  }
  
  # Create a summary table across risk measures
  summary_rows <- list()
  for (dep_var in names(iv_results_list)) {
    if (!dep_var %in% names(fe_results_list) ||
        is.null(fe_results_list[[dep_var]]$fe_esg) ||
        is.null(iv_results_list[[dep_var]]$second_stage))
      next
    
    # Extract coefficients and SE for FE
    fe_model <- fe_results_list[[dep_var]]$fe_esg
    fe_coef <- coef(fe_model)["esg_score"]
    fe_se <- sqrt(diag(vcov(fe_model)))["esg_score"]
    
    # Extract coefficients and SE for IV
    iv_model <- iv_results_list[[dep_var]]$second_stage
    iv_coef <- coef(iv_model)["pred_esg_score"]
    iv_se <- sqrt(diag(vcov(iv_model)))["pred_esg_score"]
    
    # Compute significance stars
    fe_p <- 2 * pt(abs(fe_coef / fe_se), df = fe_model$df.residual, lower.tail = FALSE)
    iv_p <- 2 * pt(abs(iv_coef / iv_se), df = iv_model$df.residual, lower.tail = FALSE)
    
    fe_sig <- ifelse(fe_p < 0.001, "***", ifelse(fe_p < 0.01, "**", ifelse(fe_p < 0.05, "*", ifelse(fe_p < 0.1, "†", ""))))
    iv_sig <- ifelse(iv_p < 0.001, "***", ifelse(iv_p < 0.01, "**", ifelse(iv_p < 0.05, "*", ifelse(iv_p < 0.1, "†", ""))))
    
    # Extract F-statistic from first stage (if available)
    f_stat <- NA
    first_stage_sum <- summary(iv_results_list[[dep_var]]$first_stage)
    if (!is.null(first_stage_sum$fstatistic) && length(first_stage_sum$fstatistic) > 0) {
      f_stat <- first_stage_sum$fstatistic[1]
    }
    
    summary_rows[[dep_var]] <- data.frame(
      Risk_Measure = get_nice_dep_name(dep_var),
      FE_Coef = sprintf("%.3f%s", fe_coef, fe_sig),
      FE_SE = sprintf("(%.3f)", fe_se),
      IV_Coef = sprintf("%.3f%s", iv_coef, iv_sig),
      IV_SE = sprintf("(%.3f)", iv_se),
      First_Stage_F = if (!is.na(f_stat)) sprintf("%.2f", f_stat) else "N/A",
      stringsAsFactors = FALSE
    )
  }
  
  if (length(summary_rows) > 0) {
    summary_table <- do.call(rbind, summary_rows)
    all_risk_summary <- kable(summary_table,
                              col.names = c("Risk Measure", "FE Coefficient", "SE", "IV Coefficient", "SE", "First Stage F"),
                              caption = "Comparison of FE and IV Estimates for ESG Score Across Risk Measures",
                              booktabs = TRUE,
                              align = c("l", "c", "c", "c", "c", "c")) %>% 
      kable_styling(latex_options = c("striped", "HOLD_position"), full_width = FALSE, font_size = 9) %>% 
      add_footnote("†p<0.1; *p<0.05; **p<0.01; ***p<0.001. Standard errors in parentheses.", notation = "none")
  } else {
    all_risk_summary <- kable(data.frame(Message = "No valid IV models could be estimated. Check your data."),
                              caption = "IV Analysis Results") %>% 
      kable_styling(latex_options = c("HOLD_position"), full_width = FALSE, font_size = 9)
  }
  
  # -------------------------------
  # Step 5: Create Diagnostic Visualizations
  # -------------------------------
  viz_data <- data.frame(
    Risk_Measure = character(),
    Method = character(),
    Coefficient = numeric(),
    SE = numeric(),
    Lower_CI = numeric(),
    Upper_CI = numeric(),
    stringsAsFactors = FALSE
  )
  
  f_stats <- data.frame(
    Risk_Measure = character(),
    F_Statistic = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (dep_var in names(iv_results_list)) {
    if (!dep_var %in% names(fe_results_list) ||
        is.null(fe_results_list[[dep_var]]$fe_esg) ||
        is.null(iv_results_list[[dep_var]]$second_stage))
      next
    
    # FE estimates
    fe_model <- fe_results_list[[dep_var]]$fe_esg
    fe_coef <- coef(fe_model)["esg_score"]
    fe_se <- sqrt(diag(vcov(fe_model)))["esg_score"]
    
    # IV estimates
    iv_model <- iv_results_list[[dep_var]]$second_stage
    iv_coef <- coef(iv_model)["pred_esg_score"]
    iv_se <- sqrt(diag(vcov(iv_model)))["pred_esg_score"]
    
    # Append estimates to visualization data
    viz_data <- rbind(viz_data, data.frame(
      Risk_Measure = get_nice_dep_name(dep_var),
      Method = "Fixed Effects",
      Coefficient = fe_coef,
      SE = fe_se,
      Lower_CI = fe_coef - 1.96 * fe_se,
      Upper_CI = fe_coef + 1.96 * fe_se,
      stringsAsFactors = FALSE
    ))
    viz_data <- rbind(viz_data, data.frame(
      Risk_Measure = get_nice_dep_name(dep_var),
      Method = "Instrumental Variable",
      Coefficient = iv_coef,
      SE = iv_se,
      Lower_CI = iv_coef - 1.96 * iv_se,
      Upper_CI = iv_coef + 1.96 * iv_se,
      stringsAsFactors = FALSE
    ))
    
    # Record first stage F-statistic
    first_stage_sum <- summary(iv_results_list[[dep_var]]$first_stage)
    if (!is.null(first_stage_sum) && 
        !is.null(first_stage_sum$fstatistic) && 
        length(first_stage_sum$fstatistic) > 0 && 
        !is.na(first_stage_sum$fstatistic[1])) {
      f_stats <- rbind(f_stats, data.frame(
        Risk_Measure = get_nice_dep_name(dep_var),
        F_Statistic = first_stage_sum$fstatistic[1],
        stringsAsFactors = FALSE
      ))
    }
  }  # <-- Added missing closing curly brace for the for-loop
  
  # Coefficient Comparison Plot
  if (nrow(viz_data) > 0) {
    comparison_plot <- ggplot(viz_data, aes(x = Risk_Measure, y = Coefficient, color = Method, shape = Method)) +
      geom_point(position = position_dodge(width = 0.5), size = 3) +
      geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), position = position_dodge(width = 0.5), width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(title = "Comparison of ESG Coefficients: Fixed Effects vs. Instrumental Variable",
           subtitle = "Effect of ESG Score on Various Banking Risk Measures",
           y = "Coefficient Estimate", x = "") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 12),
            axis.title = element_text(face = "bold"),
            legend.position = "bottom",
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    comparison_plot <- ggplot() + 
      annotate("text", x = 0, y = 0, label = "No valid IV models could be estimated.") +
      theme_void()
  }
  
  # First-Stage F-statistics Plot
  if (nrow(f_stats) > 0 && "F_Statistic" %in% colnames(f_stats)) {
    f_stat_plot <- ggplot(f_stats, aes(x = Risk_Measure, y = F_Statistic)) +
      geom_col(fill = "steelblue") +
      geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
      geom_text(aes(label = sprintf("%.1f", F_Statistic), y = F_Statistic + 1), size = 3) +
      labs(title = "First-Stage F-Statistics for IV Estimation",
           subtitle = "Values above 10 suggest strong instruments (red dashed line)",
           y = "F-Statistic", x = "") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 12),
            axis.title = element_text(face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    f_stat_plot <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "F-statistic plot not available") +
      theme_void() + 
      theme(panel.border = element_rect(color = "gray", fill = NA))
  }
  
  # Percentage Difference Plot between IV and FE estimates
  pct_diff <- data.frame(
    Risk_Measure = character(),
    Pct_Difference = numeric(),
    stringsAsFactors = FALSE
  )
  for (dep_var in unique(viz_data$Risk_Measure)) {
    fe_val <- viz_data$Coefficient[viz_data$Risk_Measure == dep_var & viz_data$Method == "Fixed Effects"]
    iv_val <- viz_data$Coefficient[viz_data$Risk_Measure == dep_var & viz_data$Method == "Instrumental Variable"]
    if (length(fe_val) > 0 && abs(fe_val) > 1e-5) {
      pct_diff <- rbind(pct_diff, data.frame(
        Risk_Measure = dep_var,
        Pct_Difference = 100 * (iv_val - fe_val) / abs(fe_val),
        stringsAsFactors = FALSE
      ))
    }
  }
  if (nrow(pct_diff) > 0) {
    pct_diff_plot <- ggplot(pct_diff, aes(x = Risk_Measure, y = Pct_Difference)) +
      geom_col(aes(fill = ifelse(Pct_Difference > 0, "Positive", "Negative"))) +
      geom_text(aes(label = sprintf("%.1f%%", Pct_Difference),
                    y = ifelse(Pct_Difference > 0, Pct_Difference + 5, Pct_Difference - 5)),
                size = 3) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      labs(title = "Percentage Difference: IV vs. FE Estimates",
           subtitle = "Positive values indicate IV estimates are larger than FE",
           y = "Percentage Difference (%)", x = "") +
      scale_fill_manual(values = c("Positive" = "steelblue", "Negative" = "firebrick")) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 12),
            axis.title = element_text(face = "bold"),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    pct_diff_plot <- ggplot() + 
      annotate("text", x = 0, y = 0, label = "No valid comparison data available.") +
      theme_void()
  }
  
  # -------------------------------
  # Step 6: Create Hausman-like Table (with Commentary)
  # -------------------------------
  hausman_results <- data.frame(
    Risk_Measure = unique(viz_data$Risk_Measure),
    Comments = "Limited variation prevents formal Hausman testing. Consider alternative endogeneity approaches (e.g., lagged variables or GMM).",
    stringsAsFactors = FALSE
  )
  if (nrow(hausman_results) == 0) {
    hausman_results <- data.frame(
      Risk_Measure = "All measures",
      Comments = "Insufficient data for endogeneity assessment.",
      stringsAsFactors = FALSE
    )
  }
  hausman_table <- kable(hausman_results,
                         col.names = c("Risk Measure", "Endogeneity Assessment"),
                         caption = "Notes on Endogeneity in ESG-Risk Relationship",
                         booktabs = TRUE,
                         align = c("l", "l")) %>% 
    kable_styling(latex_options = c("striped", "HOLD_position"), full_width = FALSE, font_size = 9) %>% 
    add_footnote("Formal Hausman tests could not be calculated due to data limitations. Alternative approaches are recommended.", 
                 notation = "none")
  
  # -------------------------------
  # Step 7: Compile and Return Results
  # -------------------------------
  results <- list(
    iv_models = iv_results_list,
    fe_models = fe_results_list,
    comparison_tables = comparison_tables,
    summary_table = all_risk_summary,
    comparison_plot = comparison_plot,
    f_stat_plot = f_stat_plot,
    pct_diff_plot = pct_diff_plot,
    hausman_table = hausman_table,
    viz_data = viz_data
  )
  
  # Assign key outputs to global variables for the Rmd document
  iv_summary_table <<- all_risk_summary
  iv_comparison_plot <<- comparison_plot
  iv_f_stat_plot <<- f_stat_plot
  iv_pct_diff_plot <<- pct_diff_plot
  iv_hausman_table <<- hausman_table
  # For the pillar models table, choose the first available one if exists:
  if (length(comparison_tables) > 0) {
    pillar_keys <- grep("_pillars$", names(comparison_tables), value = TRUE)
    if (length(pillar_keys) > 0) {
      iv_pillar_table <<- comparison_tables[[pillar_keys[1]]]
    }
  }
  
  return(results)
}

# Wrapper function to create placeholders if analysis objects are missing
create_iv_visualizations <- function() {
  if (!exists("iv_summary_table", envir = .GlobalEnv)) {
    iv_summary_table <<- kable(data.frame(Message = "IV analysis results not available."),
                               caption = "IV Analysis Results") %>% 
      kable_styling(latex_options = c("HOLD_position"), full_width = FALSE, font_size = 9)
  }
  
  if (!exists("iv_f_stat_plot", envir = .GlobalEnv) || 
      !inherits(get("iv_f_stat_plot", envir = .GlobalEnv), "ggplot")) {
    iv_f_stat_plot <<- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "F-statistic plot not available") +
      theme_void() + 
      theme(panel.border = element_rect(color = "gray", fill = NA))
  }
  
  if (!exists("iv_comparison_plot", envir = .GlobalEnv)) {
    iv_comparison_plot <<- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "Coefficient comparison plot not available") +
      theme_void() + 
      theme(panel.border = element_rect(color = "gray", fill = NA))
  }
  
  if (!exists("iv_pct_diff_plot", envir = .GlobalEnv)) {
    iv_pct_diff_plot <<- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "Percentage difference plot not available") +
      theme_void() + 
      theme(panel.border = element_rect(color = "gray", fill = NA))
  }
  
  if (!exists("iv_hausman_table", envir = .GlobalEnv)) {
    iv_hausman_table <<- kable(data.frame(
      Risk_Measure = "All measures",
      Comments = "Hausman test results not available."
    ), caption = "Notes on Endogeneity") %>% 
      kable_styling(latex_options = c("HOLD_position"), full_width = FALSE, font_size = 9)
  }
  
  if (!exists("iv_first_stage_table", envir = .GlobalEnv)) {
    iv_first_stage_table <<- kable(data.frame(
      Variable = c("Instrument", "Log Assets", "Equity to Assets"),
      Coefficient = c("N/A", "N/A", "N/A")
    ), caption = "First Stage Results") %>% 
      kable_styling(latex_options = c("HOLD_position"), full_width = FALSE, font_size = 9)
  }
  
  if (!exists("iv_pillar_table", envir = .GlobalEnv)) {
    iv_pillar_table <<- kable(data.frame(
      Variable = c("Environmental Score", "Social Score", "Governance Score"),
      FE = c("N/A", "N/A", "N/A"),
      IV = c("N/A", "N/A", "N/A")
    ), caption = "ESG Pillar Effects") %>% 
      kable_styling(latex_options = c("HOLD_position"), full_width = FALSE, font_size = 9)
  }
  
  return(TRUE)
}
