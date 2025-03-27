#' Principal Component Analysis for ESG Component Scores
#'
#' This function performs PCA on ESG component scores and analyzes their 
#' relationship with banking risk measures.
#'
#' @param data Data frame containing ESG scores and risk measures
#' @param dep_vars Character vector of dependent variables to analyze
#' @param variance_threshold Percentage of variance to explain (default 0.8 for 80%)
#' @param max_components Maximum number of components to return (default 5)
#' @return List containing PCA results, regression models, and visualizations
run_pca_esg_analysis <- function(data, 
                                 dep_vars = c("cet1_risk_exposure", "tier1_risk_exposure", 
                                              "totalcap_risk_exposure", "leverage_ratio", "provisions", 
                                              "provisions_ratio", "liquidity_ratio", "rwa_ratio"),
                                 variance_threshold = 0.8,
                                 max_components = 5) {
  
  library(ggplot2)
  library(dplyr)
  library(kableExtra)
  library(plm)
  library(modelsummary)
  library(reshape2)
  
  # Step 1: Identify ESG component columns (not just the three main pillars)
  # Look for all potential ESG component scores using regex pattern
  esg_pattern <- ".*_(pillar|component|category|score).*"
  potential_esg_cols <- grep(esg_pattern, names(data), value = TRUE)
  
  # Filter out non-numeric columns
  esg_cols <- c()
  for (col in potential_esg_cols) {
    if (is.numeric(data[[col]])) {
      esg_cols <- c(esg_cols, col)
    }
  }
  
  # Filter out columns with too many NAs (more than 30%)
  valid_esg_cols <- c()
  for (col in esg_cols) {
    na_pct <- sum(is.na(data[[col]])) / nrow(data)
    if (na_pct <= 0.3) {
      valid_esg_cols <- c(valid_esg_cols, col)
    }
  }
  
  # If too few columns found, fallback to main pillar scores
  if (length(valid_esg_cols) < 3) {
    main_pillars <- c("environmental_pillar_score", "social_pillar_score", "governance_pillar_score")
    valid_esg_cols <- main_pillars[main_pillars %in% names(data)]
    message("Using only main pillar scores due to insufficient component scores")
  }
  
  # Step 2: Prepare data for PCA (remove rows with missing values)
  pca_data <- data[, valid_esg_cols]
  complete_rows <- complete.cases(pca_data)
  
  # Check if we have enough complete rows
  if (sum(complete_rows) < 30 || sum(complete_rows) < 0.3 * nrow(data)) {
    warning("Not enough complete cases for robust PCA. Consider imputation or using fewer variables.")
  }
  
  pca_data_complete <- pca_data[complete_rows, ]
  
  # Step 3: Run PCA
  pca_result <- prcomp(pca_data_complete, scale. = TRUE)
  
  # Step 4: Determine number of components to keep (explaining ~80% variance)
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  cum_var_explained <- cumsum(var_explained)
  num_components <- which(cum_var_explained >= variance_threshold)[1]
  
  # Make sure we don't keep too many components (capped by max_components)
  num_components <- min(num_components, max_components)
  
  # Step 5: Extract PC scores and add to original data
  pc_scores <- predict(pca_result, newdata = pca_data_complete)
  pc_score_cols <- paste0("PC", 1:num_components)
  
  # Create data frame with original complete data and PC scores
  analysis_data <- data[complete_rows, ]
  for (i in 1:num_components) {
    analysis_data[[paste0("PC", i)]] <- pc_scores[, i]
  }
  
  # Step 6: Run fixed effects panel models with principal components
  panel_data <- pdata.frame(analysis_data, index = c("lei_code", "year"))
  
  # Initialize list to store models
  pc_models <- list()
  original_models <- list()
  
  # Function to run models with principal components
  for (dep_var in dep_vars) {
    if (!(dep_var %in% names(panel_data))) {
      next
    }
    
    # 6.1: Original model with pillar scores
    pillar_cols <- c("environmental_pillar_score", "social_pillar_score", "governance_pillar_score")
    pillar_cols <- pillar_cols[pillar_cols %in% names(panel_data)]
    
    if (length(pillar_cols) > 0) {
      original_formula <- as.formula(paste0(
        dep_var, " ~ ", paste(pillar_cols, collapse = " + "), " + ",
        "log_assets + equity_to_assets"
      ))
      
      tryCatch({
        original_models[[dep_var]] <- plm(original_formula, 
                                          data = panel_data, 
                                          model = "within", 
                                          effect = "twoways")
      }, error = function(e) {
        warning(paste("Error fitting original model for", dep_var, ":", e$message))
        original_models[[dep_var]] <- NULL
      })
    }
    
    # 6.2: PC model
    pc_vars <- paste(pc_score_cols, collapse = " + ")
    pc_formula <- as.formula(paste0(dep_var, " ~ ", pc_vars, " + log_assets + equity_to_assets"))
    
    tryCatch({
      pc_models[[dep_var]] <- plm(pc_formula, 
                                  data = panel_data, 
                                  model = "within", 
                                  effect = "twoways")
    }, error = function(e) {
      warning(paste("Error fitting PC model for", dep_var, ":", e$message))
      pc_models[[dep_var]] <- NULL
    })
  }
  
  # Step 7: Create visualizations
  
  # 7.1: PCA visualization - Scree plot
  scree_data <- data.frame(
    Component = 1:length(var_explained),
    Variance = var_explained,
    Cumulative = cum_var_explained
  )
  
  scree_plot <- ggplot(scree_data, aes(x = Component)) +
    geom_bar(aes(y = Variance), stat = "identity", fill = "steelblue") +
    geom_line(aes(y = Cumulative), group = 1, color = "red") +
    geom_point(aes(y = Cumulative), color = "red", size = 3) +
    geom_hline(yintercept = variance_threshold, linetype = "dashed", color = "darkred") +
    labs(
      title = "Scree Plot of Principal Components",
      x = "Principal Component",
      y = "Proportion of Variance Explained"
    ) +
    scale_y_continuous(
      name = "Proportion of Variance",
      sec.axis = sec_axis(~., name = "Cumulative Proportion")
    ) +
    theme_minimal()
  
  # 7.2: Biplot of first two components
  biplot_data <- as.data.frame(pca_result$rotation[, 1:2])
  biplot_data$variable <- rownames(biplot_data)
  
  # Create a simplified variable name for display
  biplot_data$short_name <- gsub("_score$|_pillar_|_component_", " ", biplot_data$variable)
  biplot_data$short_name <- gsub("_", " ", biplot_data$short_name)
  biplot_data$short_name <- tools::toTitleCase(biplot_data$short_name)
  
  # Calculate positions for text labels with slight offset from arrow endpoints
  # This helps reduce overlapping text
  text_offset <- 0.05
  biplot_data$text_x <- biplot_data$PC1 * (1 + text_offset)
  biplot_data$text_y <- biplot_data$PC2 * (1 + text_offset)
  
  loading_plot <- ggplot(biplot_data, aes(x = PC1, y = PC2, label = short_name)) +
    geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                 arrow = arrow(length = unit(0.2, "cm")), color = "red") +
    geom_text(aes(x = text_x, y = text_y, label = short_name), 
              hjust = ifelse(biplot_data$PC1 > 0, 0, 1),
              vjust = ifelse(biplot_data$PC2 > 0, 0, 1),
              size = 3,
              fontface = "bold",
              check_overlap = TRUE) +
    labs(
      title = "Variable Loadings on First Two Principal Components",
      subtitle = "Based on ESG component scores",
      x = paste0("\nPC1 (", round(var_explained[1] * 100, 1), "% of variance)"),
      y = paste0("PC2 (", round(var_explained[2] * 100, 1), "% of variance)\n")
    ) +
    
    # Consistent plot dimensions
    xlim(c(-1, 1)) + ylim(c(-1, 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
      axis.title.x = element_text(margin = margin(t = 10, b = 5)),
      axis.title.y = element_text(margin = margin(r = 10, l = 5)),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
  
  
  # 7.3: Create PC interpretation table
  # Find top contributors to each PC
  get_top_loadings <- function(pc_num, n_top = 5) {
    loadings <- pca_result$rotation[, pc_num]
    sorted_idx <- order(abs(loadings), decreasing = TRUE)[1:min(n_top, length(loadings))]
    return(data.frame(
      Component = paste0("PC", pc_num),
      Variable = names(loadings)[sorted_idx],
      Loading = loadings[sorted_idx],
      stringsAsFactors = FALSE
    ))
  }
  
  top_loadings <- do.call(rbind, lapply(1:num_components, get_top_loadings))
  
  # Create interpretations
  pc_interpretation <- interpret_principal_components(pca_result, num_components)
  
  # 7.4: Create model summary table
  gof_map <- tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "Observations",  0,
    "r.squared", "R²",            3
  )
  
  # Custom coefficient map for PC models
  coef_map_pc <- c()
  for (i in 1:num_components) {
    coef_map_pc[paste0("PC", i)] <- paste0("PC", i)
  }
  coef_map_pc["log_assets"] <- "Log Assets"
  coef_map_pc["equity_to_assets"] <- "Equity to Assets"
  
  # Create model summary table
  # Create add_rows tibble with the correct structure
  add_rows_data <- data.frame(term = c("Bank FE", "Year FE"))
  
  # Add a column for each model
  for (model_name in names(pc_models)) {
    add_rows_data[[model_name]] <- "Yes"
  }
  
  pc_model_table <- modelsummary(
    pc_models,
    title = "Effects of Principal Components on Bank Risk Measures",
    stars = c('†' = 0.1, '*' = 0.05, '**' = 0.01, '***' = 0.001),
    gof_map = gof_map,
    coef_map = coef_map_pc,
    fmt = 3,
    output = "kableExtra",
    note = "Bank and year fixed effects included in all models. †p<0.1; *p<0.05; **p<0.01; ***p<0.001",
    add_rows = add_rows_data
  ) %>%
    kable_styling(latex_options = c("scale_down", "HOLD_position"),
                  full_width = FALSE,
                  font_size = 8)
  
  # 7.5: Create comparison table of R-squared
  model_comparison <- data.frame(
    DependentVariable = names(pc_models),
    R2_Original = sapply(names(pc_models), function(m) {
      if (m %in% names(original_models) && !is.null(original_models[[m]])) {
        summary(original_models[[m]])$r.squared[1]
      } else {
        NA
      }
    }),
    R2_PC = sapply(pc_models, function(m) if(!is.null(m)) summary(m)$r.squared[1] else NA),
    stringsAsFactors = FALSE
  )
  
  model_comparison$Difference <- model_comparison$R2_PC - model_comparison$R2_Original
  model_comparison$Improvement <- ifelse(
    model_comparison$Difference > 0,
    "Yes", 
    "No"
  )
  
  # Format R-squared values
  model_comparison$R2_Original <- sprintf("%.3f", model_comparison$R2_Original)
  model_comparison$R2_PC <- sprintf("%.3f", model_comparison$R2_PC)
  model_comparison$Difference <- sprintf("%.3f", model_comparison$Difference)
  
  # Rename variables for display
  model_comparison$DependentVariable <- sapply(model_comparison$DependentVariable, function(var) {
    label_map <- c(
      "cet1_risk_exposure" = "CET1 Risk Exposure",
      "tier1_risk_exposure" = "Tier 1 Risk Exposure",
      "totalcap_risk_exposure" = "Total Capital Risk Exposure",
      "leverage_ratio" = "Leverage Ratio",
      "provisions" = "Provisions",
      "provisions_ratio" = "Provisions Ratio",
      "liquidity_ratio" = "Liquidity Ratio",
      "rwa_ratio" = "RWA Ratio"
    )
    if (var %in% names(label_map)) label_map[var] else var
  })
  
  comparison_table <- kable(model_comparison,
                            col.names = c("Dependent Variable", "R² Original", "R² PCA", "Difference", "Improvement"),
                            caption = "Model Performance Comparison: Original vs. PCA",
                            booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position"),
                  full_width = FALSE,
                  font_size = 9)
  
  # Return results
  return(list(
    pca_result = pca_result,
    esg_cols_used = valid_esg_cols,
    num_components = num_components,
    variance_explained = var_explained,
    cumulative_variance = cum_var_explained,
    original_models = original_models,
    pc_models = pc_models,
    top_loadings = top_loadings,
    visualizations = list(
      scree_plot = scree_plot,
      loading_plot = loading_plot,
      pc_interpretation = pc_interpretation,
      pc_model_table = pc_model_table,
      comparison_table = comparison_table
    )
  ))
}

#' Interpret Principal Components
#'
#' This function generates interpretations for principal components
#' based on their loadings.
#'
#' @param pca_result Result of prcomp() function
#' @param num_components Number of components to interpret
#' @param top_n Number of top loadings to consider for interpretation
#' @return Formatted kable table with interpretations
interpret_principal_components <- function(pca_result, num_components = 3, top_n = 5) {
  # Get variable loadings
  loadings <- pca_result$rotation
  
  # Calculate variance explained
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  
  # Create a table to store interpretations
  interpretation <- data.frame(
    Component = paste0("PC", 1:num_components),
    Variance = sprintf("%.1f%%", var_explained[1:num_components] * 100),
    Interpretation = character(num_components),
    stringsAsFactors = FALSE
  )
  
  # Generate interpretations for each component
  for (i in 1:num_components) {
    # Get loadings for this component
    comp_loadings <- loadings[, i]
    
    # Sort by absolute loading value
    sorted_idx <- order(abs(comp_loadings), decreasing = TRUE)
    top_idx <- sorted_idx[1:min(top_n, length(sorted_idx))]
    
    # Split into positive and negative loadings
    pos_vars <- names(comp_loadings)[top_idx][comp_loadings[top_idx] > 0]
    neg_vars <- names(comp_loadings)[top_idx][comp_loadings[top_idx] < 0]
    
    # Clean variable names for display
    clean_name <- function(var) {
      var <- gsub("_score$|_pillar_|_component_", " ", var)
      var <- gsub("_", " ", var)
      var <- tools::toTitleCase(var)
      return(var)
    }
    
    pos_vars_clean <- sapply(pos_vars, clean_name)
    neg_vars_clean <- sapply(neg_vars, clean_name)
    
    # Create interpretation text
    pos_text <- if (length(pos_vars_clean) > 0) {
      paste("Positive association with", paste(pos_vars_clean, collapse = ", "))
    } else ""
    
    neg_text <- if (length(neg_vars_clean) > 0) {
      paste("Negative association with", paste(neg_vars_clean, collapse = ", "))
    } else ""
    
    # Combine texts
    if (pos_text != "" && neg_text != "") {
      interp_text <- paste(pos_text, neg_text, sep = ". ")
    } else {
      interp_text <- paste0(pos_text, neg_text)
    }
    
    interpretation$Interpretation[i] <- interp_text
  }
  
  # Create kable table
  interp_table <- kable(interpretation,
                        col.names = c("Component", "Variance Explained", "Interpretation"),
                        caption = "Interpretation of Principal Components",
                        booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position"),
                  full_width = FALSE,
                  font_size = 9) %>%
    column_spec(3, width = "4in")
  
  return(interp_table)
}

#' Create a nice loadings table for inclusion in the report
#'
#' @param pca_result Result of prcomp() function
#' @param num_components Number of components to include
#' @param top_n Number of top loadings to show per component
#' @return Formatted kable table
create_loadings_table <- function(pca_result, num_components = 3, top_n = 5) {
  # Get loadings
  loadings <- pca_result$rotation[, 1:num_components]
  
  # Function to get top loadings for a component
  get_top_vars <- function(comp_idx, n = top_n) {
    comp_loadings <- loadings[, comp_idx]
    sorted_idx <- order(abs(comp_loadings), decreasing = TRUE)[1:min(n, length(comp_loadings))]
    
    result <- data.frame(
      Variable = names(comp_loadings)[sorted_idx],
      Loading = comp_loadings[sorted_idx],
      Component = paste0("PC", comp_idx),
      stringsAsFactors = FALSE
    )
    
    return(result)
  }
  
  # Combine top loadings for all components
  all_loadings <- do.call(rbind, lapply(1:num_components, get_top_vars))
  
  # Clean variable names
  all_loadings$Variable <- gsub("_score$|_pillar_|_component_", " ", all_loadings$Variable)
  all_loadings$Variable <- gsub("_", " ", all_loadings$Variable)
  all_loadings$Variable <- tools::toTitleCase(all_loadings$Variable)
  
  # Format loadings
  all_loadings$Loading <- sprintf("%.3f", all_loadings$Loading)
  
  # Reshape to wide format for better presentation
  loadings_wide <- reshape2::dcast(all_loadings, Variable ~ Component, value.var = "Loading")
  
  # Create kable table
  kable(loadings_wide,
        caption = "Top Variable Loadings for Principal Components",
        booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position"),
                  full_width = FALSE,
                  font_size = 9)
}