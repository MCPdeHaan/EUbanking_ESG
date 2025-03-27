library(quantreg)
library(ggplot2)
library(dplyr)
library(tidyr)
library(kableExtra)

#' Run Quantile Regression Analysis for ESG Scores and Banking Risk Measures
#'
#' This function performs quantile regression analysis to explore how ESG scores 
#' affect banking risk measures across different quantiles of the risk distribution.
#'
#' @param data Data frame containing ESG scores and risk measures
#' @param dep_vars Character vector of dependent variables to analyze
#' @param tau_values Numeric vector of quantiles to analyze
#' @return List containing regression results, plot data, and plots
run_simplified_esg_analysis <- function(data, 
                                        dep_vars = c("cet1_risk_exposure", "tier1_risk_exposure", 
                                                     "totalcap_risk_exposure", "leverage_ratio", "provisions", 
                                                     "provisions_ratio", "liquidity_ratio", "rwa_ratio"), 
                                        tau_values = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  # Initialize lists to store results
  esg_models <- list()
  pillar_models <- list()
  esg_plot_data <- list()
  pillar_plot_data <- list()
  
  # Run models for each dependent variable
  for (dep_var in dep_vars) {
    # Models with ESG score
    esg_models[[dep_var]] <- run_quantile_regression(data, dep_var, "esg", tau_values)
    
    # Models with pillar scores
    pillar_models[[dep_var]] <- run_quantile_regression(data, dep_var, "pillars", tau_values)
    
    # Prepare data for plotting
    esg_plot_data[[dep_var]] <- extract_plot_data(esg_models[[dep_var]], "esg_score")
    
    # Extract and combine pillar data
    env_data <- extract_plot_data(pillar_models[[dep_var]], "environmental_pillar_score") %>% 
      mutate(pillar = "Environmental")
    soc_data <- extract_plot_data(pillar_models[[dep_var]], "social_pillar_score") %>% 
      mutate(pillar = "Social")
    gov_data <- extract_plot_data(pillar_models[[dep_var]], "governance_pillar_score") %>% 
      mutate(pillar = "Governance")
    
    pillar_plot_data[[dep_var]] <- bind_rows(env_data, soc_data, gov_data)
  }
  
  # Create plots
  plots <- create_plots(esg_plot_data, pillar_plot_data, dep_vars)
  
  # Return results
  return(list(
    coefficients = list(esg = esg_models, pillars = pillar_models),
    plot_data = list(esg = esg_plot_data, pillars = pillar_plot_data),
    plots = plots
  ))
}

#' Run Quantile Regression for a Specific Dependent Variable
#'
#' @param data Data frame containing ESG scores and risk measures
#' @param dep_var Character string specifying the dependent variable
#' @param model_type Character string specifying the model type ("esg" or "pillars")
#' @param tau_values Numeric vector of quantiles to analyze
#' @return List of quantile regression models
run_quantile_regression <- function(data, dep_var, model_type, tau_values) {
  models <- list()
  
  if (model_type == "esg") {
    # ESG score model formula
    formula <- as.formula(paste0(dep_var, " ~ esg_score + log_assets + equity_to_assets"))
  } else if (model_type == "pillars") {
    # Pillar scores model formula
    formula <- as.formula(paste0(dep_var, " ~ environmental_pillar_score + social_pillar_score + governance_pillar_score + log_assets + equity_to_assets"))
  }
  
  # Run models for each quantile
  for (tau in tau_values) {
    tryCatch({
      models[[as.character(tau)]] <- rq(formula, data = data, tau = tau)
    }, error = function(e) {
      warning(paste("Error fitting model for", dep_var, "at quantile", tau, ":", e$message))
      models[[as.character(tau)]] <- NULL
    })
  }
  
  return(models)
}

#' Extract Coefficient Data for Plotting
#'
#' @param models List of quantile regression models
#' @param var_name Character string specifying the variable to extract
#' @return Data frame containing coefficient estimates and confidence intervals
extract_plot_data <- function(models, var_name) {
  # Initialize empty data frame for cases where all models failed
  if (length(models) == 0) {
    return(data.frame(tau = numeric(), coef = numeric(), 
                      lower = numeric(), upper = numeric(), 
                      variable = character()))
  }
  
  # Extract coefficients and standard errors
  valid_models <- !sapply(models, is.null)
  if (!any(valid_models)) {
    return(data.frame(tau = numeric(), coef = numeric(), 
                      lower = numeric(), upper = numeric(), 
                      variable = character()))
  }
  
  models <- models[valid_models]
  coefs <- sapply(models, function(m) coef(m)[var_name])
  ses <- sapply(models, function(m) summary(m, se = "nid")$coefficients[var_name, "Std. Error"])
  
  # Create data frame for plotting
  tau_values <- as.numeric(names(models))
  plot_data <- data.frame(
    tau = tau_values,
    coef = coefs,
    lower = coefs - 1.96 * ses,
    upper = coefs + 1.96 * ses,
    variable = var_name
  )
  
  return(plot_data)
}

#' Create Plots for Quantile Regression Results
#'
#' @param esg_plot_data List of data frames containing ESG coefficient data
#' @param pillar_plot_data List of data frames containing pillar coefficient data
#' @param dep_vars Character vector of dependent variables
#' @return List of plots
create_plots <- function(esg_plot_data, pillar_plot_data, dep_vars) {
  plots <- list(individual = list(), combined = list())
  
  # Create combined plots
  # ESG score plot for all dependent variables
  combined_esg_data <- bind_rows(lapply(names(esg_plot_data), function(dep_var) {
    if (nrow(esg_plot_data[[dep_var]]) > 0) {
      esg_plot_data[[dep_var]] %>% mutate(dep_var = get_nice_dep_name(dep_var))
    }
  }))
  
  plots$combined$esg <- ggplot(combined_esg_data, aes(x = tau, y = coef, color = dep_var)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = dep_var), alpha = 0.1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(
      title = "Effect of ESG Score on Risk Measures Across Quantiles",
      x = "Quantile",
      y = "Coefficient Estimate"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  # Pillar score plots for all dependent variables
  pillar_names <- c("Environmental", "Social", "Governance")
  
  for (pillar in pillar_names) {
    combined_pillar_data <- bind_rows(lapply(names(pillar_plot_data), function(dep_var) {
      if (nrow(pillar_plot_data[[dep_var]]) > 0) {
        pillar_plot_data[[dep_var]] %>% 
          filter(pillar == !!pillar) %>%
          mutate(dep_var = get_nice_dep_name(dep_var))
      }
    }))
    
    plots$combined[[tolower(pillar)]] <- ggplot(combined_pillar_data, aes(x = tau, y = coef, color = dep_var)) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = dep_var), alpha = 0.1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(
        title = paste("Effect of", pillar, "Score on Risk Measures Across Quantiles"),
        x = "Quantile",
        y = "Coefficient Estimate"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  }
  
  return(plots)
}

#' Get Nice Display Names for Dependent Variables
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
  
  return(nice_names[var_name])
}

#' Extract Raw Summary Tables from Quantile Regression Results
#'
#' @param esg_models List of ESG quantile regression models
#' @param pillar_models List of pillar quantile regression models
#' @return List of summary tables
get_raw_summary_tables <- function(esg_models, pillar_models) {
  # Process ESG models
  esg_summaries <- lapply(names(esg_models), function(dep_var) {
    models <- esg_models[[dep_var]]
    summaries <- lapply(names(models), function(tau) {
      if (!is.null(models[[tau]])) {
        summary(models[[tau]], se = "nid")$coefficients
      } else {
        NULL
      }
    })
    names(summaries) <- names(models)
    return(list(dep_var = dep_var, summaries = summaries))
  })
  
  # Process pillar models
  pillar_summaries <- lapply(names(pillar_models), function(dep_var) {
    models <- pillar_models[[dep_var]]
    summaries <- lapply(names(models), function(tau) {
      if (!is.null(models[[tau]])) {
        summary(models[[tau]], se = "nid")$coefficients
      } else {
        NULL
      }
    })
    names(summaries) <- names(models)
    return(list(dep_var = dep_var, summaries = summaries))
  })
  
  return(list(esg = esg_summaries, pillars = pillar_summaries))
}

#' Create a Simple Summary Table for ESG Score Effects
#'
#' @param summary_data List of summary tables
#' @return Formatted kable table
make_simple_esg_summary_table <- function(summary_data) {
  # Extract ESG coefficient data
  data_list <- lapply(summary_data$esg, function(dep_data) {
    dep_var <- dep_data$dep_var
    
    # Get median (0.5 quantile) results
    if (is.null(dep_data$summaries[["0.5"]])) {
      return(NULL)
    }
    
    median_results <- dep_data$summaries[["0.5"]]
    
    # Extract ESG score coefficient, standard error, and p-value
    coef <- median_results["esg_score", "Value"]
    se <- median_results["esg_score", "Std. Error"]
    pval <- median_results["esg_score", "Pr(>|t|)"]
    
    # Format with significance stars
    sig <- ifelse(pval < 0.001, "***", 
                  ifelse(pval < 0.01, "**", 
                         ifelse(pval < 0.05, "*", 
                                ifelse(pval < 0.1, "†", ""))))
    
    formatted_coef <- sprintf("%.3f%s", coef, sig)
    
    return(c(dep_var = get_nice_dep_name(dep_var), 
             coef = formatted_coef, 
             se = sprintf("(%.3f)", se)))
  })
  
  # Remove NULL entries
  data_list <- data_list[!sapply(data_list, is.null)]
  
  # Check if there are any valid results
  if (length(data_list) == 0) {
    return(NULL)
  }
  
  # Convert to data frame
  result_df <- do.call(rbind, data_list)
  colnames(result_df) <- c("Risk Measure", "ESG Score Coefficient", "Std. Error")
  
  # Create and return kable table
  kable(as.data.frame(result_df), 
        caption = "Effect of ESG Score on Bank Risk Measures (Median Quantile Regression)",
        booktabs = TRUE,
        align = c("l", "c", "c")) %>%
    kable_styling(latex_options = c("striped", "hold_position"),
                  full_width = FALSE,
                  font_size = 9) %>%
    add_footnote("†p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
                 notation = "none")
}

#' Create a Simple Summary Table for ESG Pillar Score Effects
#'
#' @param summary_data List of summary tables
#' @return Formatted kable table
make_simple_pillar_summary_table <- function(summary_data) {
  # Extract pillar coefficient data
  data_list <- lapply(summary_data$pillars, function(dep_data) {
    dep_var <- dep_data$dep_var
    
    # Get median (0.5 quantile) results
    if (is.null(dep_data$summaries[["0.5"]])) {
      return(NULL)
    }
    
    median_results <- dep_data$summaries[["0.5"]]
    
    # Format with significance stars
    format_with_stars <- function(var) {
      coef <- median_results[var, "Value"]
      se <- median_results[var, "Std. Error"]
      pval <- median_results[var, "Pr(>|t|)"]
      
      sig <- ifelse(pval < 0.001, "***", 
                    ifelse(pval < 0.01, "**", 
                           ifelse(pval < 0.05, "*", 
                                  ifelse(pval < 0.1, "†", ""))))
      
      return(list(
        coef = sprintf("%.3f%s", coef, sig),
        se = sprintf("(%.3f)", se)
      ))
    }
    
    env <- format_with_stars("environmental_pillar_score")
    soc <- format_with_stars("social_pillar_score")
    gov <- format_with_stars("governance_pillar_score")
    
    return(c(dep_var = get_nice_dep_name(dep_var), 
             env_coef = env$coef, env_se = env$se,
             soc_coef = soc$coef, soc_se = soc$se,
             gov_coef = gov$coef, gov_se = gov$se))
  })
  
  # Remove NULL entries
  data_list <- data_list[!sapply(data_list, is.null)]
  
  # Check if there are any valid results
  if (length(data_list) == 0) {
    return(NULL)
  }
  
  # Convert to data frame
  result_df <- do.call(rbind, data_list)
  colnames(result_df) <- c("Risk Measure", "Environmental", "SE", "Social", "SE", "Governance", "SE")
  
  # Create and return kable table
  kable(as.data.frame(result_df), 
        caption = "Effect of ESG Pillar Scores on Bank Risk Measures (Median Quantile Regression)",
        booktabs = TRUE,
        align = c("l", "c", "c", "c", "c", "c", "c")) %>%
    kable_styling(latex_options = c("striped", "hold_position"),
                  full_width = FALSE,
                  font_size = 9) %>%
    add_footnote("†p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
                 notation = "none")
}

#' Create a Detailed Table for a Specific Dependent Variable (ESG Score)
#'
#' @param esg_models List of ESG quantile regression models
#' @param dep_var Character string specifying the dependent variable
#' @return Formatted kable table
make_esg_table <- function(esg_models, dep_var) {
  models <- esg_models[[dep_var]]
  
  # Check if there are any valid models
  valid_models <- !sapply(models, is.null)
  if (!any(valid_models)) {
    return(NULL)
  }
  
  models <- models[valid_models]
  
  # Extract coefficients and standard errors for all quantiles
  results <- lapply(names(models), function(tau_val) {
    model <- models[[tau_val]]
    summary_model <- summary(model, se = "nid")
    
    coefs <- summary_model$coefficients[, "Value"]
    ses <- summary_model$coefficients[, "Std. Error"]
    pvals <- summary_model$coefficients[, "Pr(>|t|)"]
    
    # Format coefficients with significance stars
    formatted_coefs <- vapply(1:length(coefs), function(i) {
      coef <- coefs[i]
      pval <- pvals[i]
      
      sig <- ifelse(pval < 0.001, "***", 
                    ifelse(pval < 0.01, "**", 
                           ifelse(pval < 0.05, "*", 
                                  ifelse(pval < 0.1, "†", ""))))
      
      sprintf("%.3f%s\n(%.3f)", coef, sig, ses[i])
    }, character(1))
    
    names(formatted_coefs) <- names(coefs)
    return(formatted_coefs)
  })
  
  # Convert to data frame
  results_df <- as.data.frame(do.call(cbind, results))
  colnames(results_df) <- paste0("Q", names(models))
  
  # Create and return kable table
  kable(results_df, 
        caption = paste("Quantile Regression Results for", get_nice_dep_name(dep_var), "- ESG Score"),
        booktabs = TRUE,
        align = rep("c", ncol(results_df))) %>%
    kable_styling(latex_options = c("striped", "hold_position"),
                  full_width = FALSE,
                  font_size = 9) %>%
    add_footnote("†p<0.1; *p<0.05; **p<0.01; ***p<0.001. Standard errors in parentheses.", 
                 notation = "none")
}

#' Create a Detailed Table for a Specific Dependent Variable (Pillar Scores)
#'
#' @param pillar_models List of pillar quantile regression models
#' @param dep_var Character string specifying the dependent variable
#' @return Formatted kable table
make_pillar_table <- function(pillar_models, dep_var) {
  models <- pillar_models[[dep_var]]
  
  # Check if there are any valid models
  valid_models <- !sapply(models, is.null)
  if (!any(valid_models)) {
    return(NULL)
  }
  
  models <- models[valid_models]
  
  # Extract coefficients and standard errors for all quantiles
  results <- lapply(names(models), function(tau_val) {
    model <- models[[tau_val]]
    summary_model <- summary(model, se = "nid")
    
    coefs <- summary_model$coefficients[, "Value"]
    ses <- summary_model$coefficients[, "Std. Error"]
    pvals <- summary_model$coefficients[, "Pr(>|t|)"]
    
    # Format coefficients with significance stars
    formatted_coefs <- vapply(1:length(coefs), function(i) {
      coef <- coefs[i]
      pval <- pvals[i]
      
      sig <- ifelse(pval < 0.001, "***", 
                    ifelse(pval < 0.01, "**", 
                           ifelse(pval < 0.05, "*", 
                                  ifelse(pval < 0.1, "†", ""))))
      
      sprintf("%.3f%s\n(%.3f)", coef, sig, ses[i])
    }, character(1))
    
    names(formatted_coefs) <- names(coefs)
    return(formatted_coefs)
  })
  
  # Convert to data frame
  results_df <- as.data.frame(do.call(cbind, results))
  colnames(results_df) <- paste0("Q", names(models))
  
  # Reorder rows for better presentation
  var_order <- c("(Intercept)", "environmental_pillar_score", "social_pillar_score", "governance_pillar_score", "log_assets", "equity_to_assets")
  var_order <- var_order[var_order %in% rownames(results_df)]
  results_df <- results_df[match(var_order, rownames(results_df)), ]
  
  # Create nice row names
  nice_rownames <- c("Intercept", "Environmental Score", "Social Score", "Governance Score", "Log Assets", "Equity to Assets")
  nice_rownames <- nice_rownames[1:length(var_order)]
  rownames(results_df) <- nice_rownames
  
  # Create and return kable table
  kable(results_df, 
        caption = paste("Quantile Regression Results for", get_nice_dep_name(dep_var), "- ESG Pillar Scores"),
        booktabs = TRUE,
        align = rep("c", ncol(results_df))) %>%
    kable_styling(latex_options = c("striped", "hold_position"),
                  full_width = FALSE,
                  font_size = 9) %>%
    add_footnote("†p<0.1; *p<0.05; **p<0.01; ***p<0.001. Standard errors in parentheses.", 
                 notation = "none")
}