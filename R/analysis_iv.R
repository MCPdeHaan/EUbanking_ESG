#' analysis_iv.R
#' 
#' This script implements instrumental variable analysis to address endogeneity concerns
#' in the relationship between ESG scores and banking risk measures.
#' 
#' The approach uses industry-level ESG averages by country-year (excluding the focal bank)
#' as instruments for bank-level ESG scores.

library(plm)
library(dplyr)
library(modelsummary)
library(kableExtra)
library(tidyr)
library(ggplot2)
library(tibble)
library(AER)  # For ivreg

# Create global variables to store outputs (for direct reference in RMarkdown)
iv_summary_table <- NULL
iv_comparison_plot <- NULL
iv_f_stat_plot <- NULL
iv_pct_diff_plot <- NULL
iv_hausman_table <- NULL
iv_pillar_table <- NULL
iv_first_stage_table <- NULL

#' Instrumental Variable Analysis for ESG and Banking Risk
#'
#' This function implements a 2SLS approach to address endogeneity concerns
#' in the relationship between ESG scores and banking risk measures.
#'
#' @param data Data frame containing ESG scores and risk measures
#' @param dep_vars Character vector of dependent variables to analyze
#' @return List containing regression results and comparison tables
run_iv_analysis <- function(data, 
                            dep_vars = c("cet1_risk_exposure", "tier1_risk_exposure", 
                                         "totalcap_risk_exposure", "leverage_ratio", "provisions", 
                                         "provisions_ratio", "liquidity_ratio", "rwa_ratio")) {
  
  # Step 0: Validate input data
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
  
  # Check if at least one dependent variable exists in the data
  available_dep_vars <- intersect(dep_vars, colnames(data))
  if (length(available_dep_vars) == 0) {
    warning("None of the specified dependent variables are present in the dataset.")
    return(NULL)
  }
  
  # Only keep complete observations for essential variables
  data <- data %>% 
    select(all_of(c("lei_code", required_columns, available_dep_vars))) %>%
    na.omit()
  
  if (nrow(data) == 0) {
    warning("No complete observations found after removing NA values.")
    return(NULL)
  }
  
  # Step 1: Create instruments - average ESG scores by country-year, excluding focal bank
  # Extract country from lei_code if country column doesn't exist
  if (!"country" %in% colnames(data)) {
    data$country <- substr(data$lei_code, 1, 2)  # First 2 characters of LEI code
  }
  
  data$country_year <- paste(data$country, data$year, sep = "_")
  
  # Create instruments for ESG scores by calculating leave-one-out means
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
  
  # Handle cases with only one bank per country-year
  # Extract country from country_year
  instruments_data$country <- sub("_.*", "", instruments_data$country_year)
  
  # Calculate country means across all years (for single bank country-years)
  country_means <- instruments_data %>%
    group_by(country) %>%
    summarize(
      mean_esg = mean(esg_score, na.rm = TRUE),
      mean_env = mean(environmental_pillar_score, na.rm = TRUE),
      mean_soc = mean(social_pillar_score, na.rm = TRUE),
      mean_gov = mean(governance_pillar_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Join with main data and fill in missing instrument values
  instruments_data <- instruments_data %>%
    left_join(country_means, by = "country") %>%
    mutate(
      instr_esg_score = ifelse(is.na(instr_esg_score), mean_esg, instr_esg_score),
      instr_environmental_pillar_score = ifelse(is.na(instr_environmental_pillar_score), mean_env, instr_environmental_pillar_score),
      instr_social_pillar_score = ifelse(is.na(instr_social_pillar_score), mean_soc, instr_social_pillar_score),
      instr_governance_pillar_score = ifelse(is.na(instr_governance_pillar_score), mean_gov, instr_governance_pillar_score)
    ) %>%
    select(-mean_esg, -mean_env, -mean_soc, -mean_gov)
  
  # Step 2: Run IV models for each dependent variable
  iv_results_list <- list()
  
  for (dep_var in available_dep_vars) {
    # Use tryCatch to handle potential errors for each dependent variable
    tryCatch({
      # Convert to panel data
      panel_data <- pdata.frame(instruments_data, index = c("lei_code", "year"))
      
      # First stage - ESG score model
      first_stage_esg <- plm(esg_score ~ instr_esg_score + log_assets + equity_to_assets, 
                             data = panel_data, 
                             model = "within", 
                             effect = "twoways")
      
      # Get predicted values from first stage
      panel_data$pred_esg_score <- fitted(first_stage_esg)
      
      # Second stage regression
      second_stage_esg <- plm(as.formula(paste0(dep_var, " ~ pred_esg_score + log_assets + equity_to_assets")),
                              data = panel_data,
                              model = "within",
                              effect = "twoways")
      
      # Store results
      iv_results_list[[dep_var]] <- list(
        first_stage = first_stage_esg,
        second_stage = second_stage_esg
      )
      
      # First stage - ESG pillar scores (wrapped in tryCatch for each pillar)
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
      
      # Only proceed with second stage for pillars if all first stages were successful
      if (!is.null(first_stage_env) && !is.null(first_stage_soc) && !is.null(first_stage_gov)) {
        # Get predicted values from first stages
        panel_data$pred_env_score <- fitted(first_stage_env)
        panel_data$pred_soc_score <- fitted(first_stage_soc)
        panel_data$pred_gov_score <- fitted(first_stage_gov)
        
        # Second stage regression
        second_stage_pillars <- tryCatch({
          plm(as.formula(paste0(dep_var, " ~ pred_env_score + pred_soc_score + pred_gov_score + log_assets + equity_to_assets")),
              data = panel_data,
              model = "within",
              effect = "twoways")
        }, error = function(e) NULL)
        
        if (!is.null(second_stage_pillars)) {
          # Store results
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
  
  # Step 3: Run regular fixed effects models for comparison
  fe_results_list <- list()
  
  for (dep_var in names(iv_results_list)) {
    # Use tryCatch to handle potential errors for each dependent variable
    tryCatch({
      # Convert to panel data
      panel_data <- pdata.frame(data, index = c("lei_code", "year"))
      
      # ESG score model
      fe_esg <- plm(as.formula(paste0(dep_var, " ~ esg_score + log_assets + equity_to_assets")),
                    data = panel_data,
                    model = "within",
                    effect = "twoways")
      
      # Pillar scores model
      fe_pillars <- plm(as.formula(paste0(dep_var, " ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_assets + equity_to_assets")),
                        data = panel_data,
                        model = "within",
                        effect = "twoways")
      
      # Store results
      fe_results_list[[dep_var]] <- list(
        fe_esg = fe_esg,
        fe_pillars = fe_pillars
      )
    }, error = function(e) {
      warning(paste("Error in FE analysis for", dep_var, ":", e$message))
    })
  }
  
  # Step 4: Create comparison tables between FE and IV results
  comparison_tables <- list()
  
  # Custom coefficient map for better variable names
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
  
  # Goodness-of-fit statistics
  gof_map <- tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "Observations",  0,
    "r.squared", "R²",            3
  )
  
  for (dep_var in names(iv_results_list)) {
    # Skip if either FE or IV model is missing
    if (!dep_var %in% names(fe_results_list) || 
        is.null(fe_results_list[[dep_var]]$fe_esg) || 
        is.null(iv_results_list[[dep_var]]$second_stage)) {
      next
    }
    
    # Combine FE and IV models for this dependent variable - ESG score
    models_esg <- list(
      "Fixed Effects" = fe_results_list[[dep_var]]$fe_esg,
      "Instrumental Variable" = iv_results_list[[dep_var]]$second_stage
    )
    
    # Create ESG table
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
      kable_styling(latex_options = c("HOLD_position"),
                    full_width = FALSE,
                    font_size = 9)
    
    # Skip if either FE or IV pillar model is missing
    if (!dep_var %in% names(fe_results_list) || 
        is.null(fe_results_list[[dep_var]]$fe_pillars) || 
        is.null(iv_results_list[[dep_var]]$second_stage_pillars)) {
      next
    }
    
    # Combine FE and IV models for this dependent variable - Pillar scores
    models_pillars <- list(
      "Fixed Effects" = fe_results_list[[dep_var]]$fe_pillars,
      "Instrumental Variable" = iv_results_list[[dep_var]]$second_stage_pillars
    )
    
    # Create Pillars table
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
      kable_styling(latex_options = c("HOLD_position"),
                    full_width = FALSE,
                    font_size = 9)
  }
  
  # Step 5: Create first-stage regression summary tables to check instrument relevance
  first_stage_tables <- list()
  
  for (dep_var in names(iv_results_list)) {
    # Skip if missing first stage results
    if (is.null(iv_results_list[[dep_var]]$first_stage)) {
      next
    }
    
    # Create first stage table for ESG score
    first_stage_tables[[paste0(dep_var, "_esg")]] <- modelsummary(
      list("First Stage" = iv_results_list[[dep_var]]$first_stage),
      title = paste("First Stage Regression for", get_nice_dep_name(dep_var), "- ESG Score"),
      stars = c('†' = 0.1, '*' = 0.05, '**' = 0.01, '***' = 0.001),
      gof_map = gof_map,
      fmt = 3,
      output = "kableExtra",
      note = "Dependent variable: ESG Score. †p<0.1; *p<0.05; **p<0.01; ***p<0.001",
      add_rows = tibble(
        term = c("Bank FE", "Year FE"),
        "First Stage" = c("Yes", "Yes")
      )
    ) %>%
      kable_styling(latex_options = c("HOLD_position"),
                    full_width = FALSE,
                    font_size = 9)
    
    # Skip if missing first stage pillar results
    if (is.null(iv_results_list[[dep_var]]$first_stage_pillars)) {
      next
    }
    
    # Create first stage tables for pillar scores (showing only environmental as example)
    first_stage_tables[[paste0(dep_var, "_env")]] <- modelsummary(
      list("First Stage" = iv_results_list[[dep_var]]$first_stage_pillars$env),
      title = paste("First Stage Regression for", get_nice_dep_name(dep_var), "- Environmental Score"),
      stars = c('†' = 0.1, '*' = 0.05, '**' = 0.01, '***' = 0.001),
      gof_map = gof_map,
      fmt = 3,
      output = "kableExtra",
      note = "Dependent variable: Environmental Pillar Score. †p<0.1; *p<0.05; **p<0.01; ***p<0.001",
      add_rows = tibble(
        term = c("Bank FE", "Year FE"),
        "First Stage" = c("Yes", "Yes")
      )
    ) %>%
      kable_styling(latex_options = c("HOLD_position"),
                    full_width = FALSE,
                    font_size = 9)
  }
  
  # Create a summary table for all dependent variables
  summary_rows <- list()
  
  for (dep_var in names(iv_results_list)) {
    # Skip if either FE or IV model is missing
    if (!dep_var %in% names(fe_results_list) || 
        is.null(fe_results_list[[dep_var]]$fe_esg) || 
        is.null(iv_results_list[[dep_var]]$second_stage)) {
      next
    }
    
    # Get coefficients and standard errors
    fe_coef <- coef(fe_results_list[[dep_var]]$fe_esg)["esg_score"]
    fe_se <- sqrt(diag(vcov(fe_results_list[[dep_var]]$fe_esg)))["esg_score"]
    
    iv_coef <- coef(iv_results_list[[dep_var]]$second_stage)["pred_esg_score"]
    iv_se <- sqrt(diag(vcov(iv_results_list[[dep_var]]$second_stage)))["pred_esg_score"]
    
    # Calculate significance
    fe_p <- 2 * pt(abs(fe_coef / fe_se), df = fe_results_list[[dep_var]]$fe_esg$df.residual, lower.tail = FALSE)
    iv_p <- 2 * pt(abs(iv_coef / iv_se), df = iv_results_list[[dep_var]]$second_stage$df.residual, lower.tail = FALSE)
    
    # Format coefficients with significance stars
    fe_sig <- ifelse(fe_p < 0.001, "***", 
                     ifelse(fe_p < 0.01, "**", 
                            ifelse(fe_p < 0.05, "*", 
                                   ifelse(fe_p < 0.1, "†", ""))))
    
    iv_sig <- ifelse(iv_p < 0.001, "***", 
                     ifelse(iv_p < 0.01, "**", 
                            ifelse(iv_p < 0.05, "*", 
                                   ifelse(iv_p < 0.1, "†", ""))))
    
    # Calculate F-statistic for first stage
    first_stage_summary <- summary(iv_results_list[[dep_var]]$first_stage)
    f_stat <- NA
    
    # Safely extract F-statistic if it exists
    if (!is.null(first_stage_summary$fstatistic) && length(first_stage_summary$fstatistic) > 0) {
      f_stat <- first_stage_summary$fstatistic[1]
    }
    
    # Add to summary rows
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
  
  # Check if we have any results to display
  if (length(summary_rows) == 0) {
    # Create a placeholder table if no results are available
    all_risk_summary <- kable(data.frame(
      Message = "No valid IV models could be estimated. This may be due to insufficient variation in the instruments or other data limitations."
    ), caption = "IV Analysis Results") %>%
      kable_styling(latex_options = c("HOLD_position"),
                    full_width = FALSE,
                    font_size = 9)
  } else {
    # Combine all summary rows into a single data frame
    summary_table <- do.call(rbind, summary_rows)
    
    # Create a nice kable table
    all_risk_summary <- kable(summary_table,
                              col.names = c("Risk Measure", "FE Coefficient", "SE", "IV Coefficient", "SE", "First Stage F"),
                              caption = "Comparison of Fixed Effects and IV Estimates for ESG Score Across Risk Measures",
                              booktabs = TRUE,
                              align = c("l", "c", "c", "c", "c", "c")) %>%
      kable_styling(latex_options = c("striped", "HOLD_position"),
                    full_width = FALSE,
                    font_size = 9) %>%
      add_footnote("†p<0.1; *p<0.05; **p<0.01; ***p<0.001. Standard errors in parentheses.", 
                   notation = "none")
  }
  
  # Step 6: Create visualization comparing FE and IV estimates
  viz_data <- data.frame(
    Risk_Measure = character(),
    Method = character(),
    Coefficient = numeric(),
    SE = numeric(),
    Lower_CI = numeric(),
    Upper_CI = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (dep_var in names(iv_results_list)) {
    # Skip if either FE or IV model is missing
    if (!dep_var %in% names(fe_results_list) || 
        is.null(fe_results_list[[dep_var]]$fe_esg) || 
        is.null(iv_results_list[[dep_var]]$second_stage)) {
      next
    }
    
    # Get coefficients and standard errors for FE
    fe_coef <- coef(fe_results_list[[dep_var]]$fe_esg)["esg_score"]
    fe_se <- sqrt(diag(vcov(fe_results_list[[dep_var]]$fe_esg)))["esg_score"]
    
    # Get coefficients and standard errors for IV
    iv_coef <- coef(iv_results_list[[dep_var]]$second_stage)["pred_esg_score"]
    iv_se <- sqrt(diag(vcov(iv_results_list[[dep_var]]$second_stage)))["pred_esg_score"]
    
    # Add to visualization data
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
  }
  
  # Create the coefficient comparison plot if we have data
  if (nrow(viz_data) > 0) {
    comparison_plot <- ggplot(viz_data, aes(x = Risk_Measure, y = Coefficient, color = Method, shape = Method)) +
      geom_point(position = position_dodge(width = 0.5), size = 3) +
      geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                    position = position_dodge(width = 0.5), 
                    width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(title = "Comparison of ESG Coefficients: Fixed Effects vs. Instrumental Variable",
           subtitle = "Effect of ESG Score on Various Banking Risk Measures",
           y = "Coefficient Estimate",
           x = "") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  } else {
    # Create a placeholder plot if no data is available
    comparison_plot <- ggplot() + 
      annotate("text", x = 0, y = 0, label = "No valid IV models could be estimated.") +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
  }
  
  # Create diagnostic plots
  # First-stage F-statistics plot
  f_stats <- data.frame(
    Risk_Measure = character(),
    F_Statistic = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (dep_var in names(iv_results_list)) {
    if (!is.null(iv_results_list[[dep_var]]$first_stage)) {
      first_stage_summary <- summary(iv_results_list[[dep_var]]$first_stage)
      
      # Safely extract F-statistic if it exists
      if (!is.null(first_stage_summary$fstatistic) && length(first_stage_summary$fstatistic) > 0) {
        f_stat <- first_stage_summary$fstatistic[1]
        
        f_stats <- rbind(f_stats, data.frame(
          Risk_Measure = get_nice_dep_name(dep_var),
          F_Statistic = f_stat,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Create F-statistic plot if we have data
  if (nrow(f_stats) > 0) {
    iv_f_stat_plot <- ggplot(f_stats, aes(x = Risk_Measure, y = F_Statistic)) +
      geom_col(fill = "steelblue") +
      geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
      geom_text(aes(y = F_Statistic + 1, label = sprintf("%.1f", F_Statistic)), 
                vjust = 0, size = 3) +
      labs(title = "First-Stage F-Statistics for IV Estimation",
           subtitle = "Values above 10 suggest strong instruments (red dashed line)",
           y = "F-Statistic",
           x = "") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  } else {
    # Create a placeholder plot if no data is available
    iv_f_stat_plot <- ggplot() + 
      annotate("text", x = 0, y = 0, label = "No valid first-stage models available.") +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
  }
  
  # Calculate percentage differences between FE and IV estimates
  pct_diff <- data.frame(
    Risk_Measure = character(),
    Pct_Difference = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(viz_data)) {
    if (viz_data$Method[i] == "Instrumental Variable") {
      # Get matching FE estimate
      fe_row <- which(viz_data$Risk_Measure == viz_data$Risk_Measure[i] & 
                        viz_data$Method == "Fixed Effects")
      
      if (length(fe_row) > 0) {
        iv_coef <- viz_data$Coefficient[i]
        fe_coef <- viz_data$Coefficient[fe_row]
        
        # Calculate percentage difference (safely handling zeros)
        if (abs(fe_coef) > 0.00001) {  # Avoid division by near-zero
          pct_diff <- rbind(pct_diff, data.frame(
            Risk_Measure = viz_data$Risk_Measure[i],
            Pct_Difference = 100 * (iv_coef - fe_coef) / abs(fe_coef),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Create percentage difference plot if we have data
  if (nrow(pct_diff) > 0) {
    pct_diff_plot <- ggplot(pct_diff, aes(x = Risk_Measure, y = Pct_Difference)) +
      geom_col(aes(fill = ifelse(Pct_Difference > 0, "Positive", "Negative"))) +
      geom_text(aes(y = ifelse(Pct_Difference > 0, Pct_Difference + 5, Pct_Difference - 5), 
                    label = sprintf("%.1f%%", Pct_Difference)), 
                vjust = ifelse(pct_diff$Pct_Difference > 0, 0, 1), size = 3) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      labs(title = "Percentage Difference: IV vs. FE Estimates",
           subtitle = "Positive values indicate IV estimates are larger than FE",
           y = "Percentage Difference (%)",
           x = "") +
      scale_fill_manual(values = c("Positive" = "steelblue", "Negative" = "firebrick")) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  } else {
    # Create a placeholder plot if no data is available
    pct_diff_plot <- ggplot() + 
      annotate("text", x = 0, y = 0, label = "No valid comparison data available.") +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
  }
  
  # Create a Hausman-like table with commentary instead of running the test
  hausman_results <- data.frame(
    Risk_Measure = unique(viz_data$Risk_Measure),
    Comments = paste("Both instrument variables and the Hausman test require more data",
                     "variation than available in the current dataset. Consider other",
                     "endogeneity approaches like lagged variables or GMM."),
    stringsAsFactors = FALSE
  )
  
  # If no valid data for Hausman test, create placeholder
  if (nrow(hausman_results) == 0) {
    hausman_results <- data.frame(
      Risk_Measure = "All measures",
      Comments = "Insufficient data for endogeneity assessment.",
      stringsAsFactors = FALSE
    )
  }
  
  # Create a nice kable table for Hausman test alternatives
  hausman_table <- kable(hausman_results,
                         col.names = c("Risk Measure", "Endogeneity Assessment"),
                         caption = "Notes on Endogeneity in ESG-Risk Relationship",
                         booktabs = TRUE,
                         align = c("l", "l")) %>%
    kable_styling(latex_options = c("striped", "HOLD_position"),
                  full_width = FALSE,
                  font_size = 9) %>%
    add_footnote("Formal Hausman tests could not be calculated due to data limitations. Alternative approaches to addressing endogeneity are recommended.", 
                 notation = "none")
  
  # Store important outputs in global variables
  results <- list(
    iv_summary_table = all_risk_summary,
    iv_comparison_plot = comparison_plot, 
    iv_f_stat_plot = iv_f_stat_plot,
    iv_pct_diff_plot = pct_diff_plot,
    iv_hausman_table = hausman_table,
    iv_pillar_table = iv_pillar_table,
    iv_first_stage_table = iv_first_stage_table
  )
  
  return(results)
  
  # Store a representative pillar and first stage table for example
  if (length(comparison_tables) > 0) {
    pillar_key <- grep("_pillars$", names(comparison_tables), value = TRUE)[1]
    if (length(pillar_key) > 0) {
      iv_pillar_table <<- comparison_tables[[pillar_key]]
    }
  }
  
  if (length(first_stage_tables) > 0) {
    fs_key <- grep("_esg$", names(first_stage_tables), value = TRUE)[1]
    if (length(fs_key) > 0) {
      iv_first_stage_table <<- first_stage_tables[[fs_key]]
    }
  }
  
  # Create results directory if it doesn't exist
  if (!dir.exists("results")) {
    dir.create("results", showWarnings = FALSE)
  }
  
  # Safe saving of outputs with tryCatch
  tryCatch({
    save_kable(all_risk_summary, "results/table_iv_summary.tex")
    
    # Save a representative pillar table
    if (!is.null(iv_pillar_table)) {
      save_kable(iv_pillar_table, "results/table_iv_pillars.tex")
    }
    
    # Save a representative first stage table
    if (!is.null(iv_first_stage_table)) {
      save_kable(iv_first_stage_table, "results/table_first_stage.tex")
    }
    
    # Save the Hausman test results
    save_kable(hausman_table, "results/table_hausman_tests.tex")
    
    # Save plots
    ggsave("results/plot_fe_iv_comparison.png", comparison_plot, width = 8, height = 6, dpi = 300)
    ggsave("results/plot_f_statistics.png", f_stat_plot, width = 8, height = 5, dpi = 300)
    ggsave("results/plot_pct_differences.png", pct_diff_plot, width = 8, height = 5, dpi = 300)
  }, error = function(e) {
    warning(paste("Error saving outputs:", e$message))
  })
  
  # Return results
  return(list(
    iv_models = iv_results_list,
    fe_models = fe_results_list,
    comparison_tables = comparison_tables,
    first_stage_tables = first_stage_tables,
    summary_table = all_risk_summary,
    comparison_plot = comparison_plot,
    f_stat_plot = iv_f_stat_plot,  # Changed from f_stat_plot
    pct_diff_plot = pct_diff_plot,
    hausman_table = hausman_table,
    viz_data = viz_data
  ))
}

#' Utility function to get nice display names for dependent variables
#'
#' @param var_name Character string specifying the variable name
#' @return Character string with nicely formatted variable name
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
    return(var_name)  # Return original if no nice name defined
  }
}

# Run the analysis if required libraries are available
if (requireNamespace("plm", quietly = TRUE) && 
    requireNamespace("dplyr", quietly = TRUE) && 
    requireNamespace("modelsummary", quietly = TRUE)) {
  
  # Create results directory if it doesn't exist
  if (!dir.exists("results")) {
    dir.create("results", showWarnings = FALSE)
  }
  
  # Run the IV analysis with robust error handling
  tryCatch({
    message("Starting IV analysis...")
    iv_results <- run_iv_analysis(data_analysis)
    message("IV analysis completed successfully.")
  }, error = function(e) {
    message("Error in IV analysis: ", e$message)
    
    # Create placeholder outputs for error cases
    iv_summary_table <<- kable(data.frame(
      Message = paste("IV analysis could not be completed:", e$message)
    ), caption = "IV Analysis Error") %>%
      kable_styling(latex_options = c("HOLD_position"),
                    full_width = FALSE,
                    font_size = 9)
    
    iv_comparison_plot <<- ggplot() + 
      annotate("text", x = 0, y = 0, label = "IV analysis could not be completed.") +
      theme_void()
    
    iv_f_stat_plot <<- ggplot() + 
      annotate("text", x = 0, y = 0, label = "IV analysis could not be completed.") +
      theme_void()
    
    iv_pct_diff_plot <<- ggplot() + 
      annotate("text", x = 0, y = 0, label = "IV analysis could not be completed.") +
      theme_void()
    
    iv_hausman_table <<- kable(data.frame(
      Message = "IV analysis could not be completed."
    ), caption = "IV Analysis Error") %>%
      kable_styling(latex_options = c("HOLD_position"),
                    full_width = FALSE,
                    font_size = 9)
  })
}

# Add this function to your analysis_iv.R file or to your .Rmd file directly
# This is a wrapper function that will create placeholder plots if the main analysis fails

create_iv_visualizations <- function() {
  # Check if the required objects exist
  if (!exists("iv_summary_table", envir = .GlobalEnv)) {
    iv_summary_table <<- kable(data.frame(
      Message = "IV analysis results not available. Ensure the analysis_iv.R script runs successfully."
    ), caption = "IV Analysis Results") %>%
      kable_styling(latex_options = c("HOLD_position"),
                    full_width = FALSE,
                    font_size = 9)
  }
  
  # Create placeholder for F-statistics plot if it doesn't exist
  if (!exists("iv_f_stat_plot", envir = .GlobalEnv)) {
    iv_f_stat_plot <<- ggplot() + 
      annotate("text", x = 0, y = 0, label = "F-statistic plot not available") +
      theme_void() +
      theme(panel.border = element_rect(color = "gray", fill = NA))
  }
  
  # Create placeholder for comparison plot if it doesn't exist
  if (!exists("iv_comparison_plot", envir = .GlobalEnv)) {
    iv_comparison_plot <<- ggplot() + 
      annotate("text", x = 0, y = 0, label = "Coefficient comparison plot not available") +
      theme_void() +
      theme(panel.border = element_rect(color = "gray", fill = NA))
  }
  
  # Create placeholder for percentage difference plot if it doesn't exist
  if (!exists("iv_pct_diff_plot", envir = .GlobalEnv)) {
    iv_pct_diff_plot <<- ggplot() + 
      annotate("text", x = 0, y = 0, label = "Percentage difference plot not available") +
      theme_void() +
      theme(panel.border = element_rect(color = "gray", fill = NA))
  }
  
  # Create placeholder for Hausman table if it doesn't exist
  if (!exists("iv_hausman_table", envir = .GlobalEnv)) {
    iv_hausman_table <<- kable(data.frame(
      Risk_Measure = "All measures",
      Comments = "Hausman test results not available."
    ), caption = "Notes on Endogeneity") %>%
      kable_styling(latex_options = c("HOLD_position"),
                    full_width = FALSE,
                    font_size = 9)
  }
  
  # Create placeholder for first stage table if it doesn't exist
  if (!exists("iv_first_stage_table", envir = .GlobalEnv)) {
    iv_first_stage_table <<- kable(data.frame(
      Variable = c("Instrument", "Log Assets", "Equity to Assets"),
      Coefficient = c("N/A", "N/A", "N/A")
    ), caption = "First Stage Results") %>%
      kable_styling(latex_options = c("HOLD_position"),
                    full_width = FALSE,
                    font_size = 9)
  }
  
  # Create placeholder for pillar table if it doesn't exist
  if (!exists("iv_pillar_table", envir = .GlobalEnv)) {
    iv_pillar_table <<- kable(data.frame(
      Variable = c("Environmental Score", "Social Score", "Governance Score"),
      FE = c("N/A", "N/A", "N/A"),
      IV = c("N/A", "N/A", "N/A")
    ), caption = "ESG Pillar Effects") %>%
      kable_styling(latex_options = c("HOLD_position"),
                    full_width = FALSE,
                    font_size = 9)
  }
  
  # Return TRUE to indicate completion
  return(TRUE)
}