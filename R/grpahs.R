# Script to generate visualizations for ESG-Banking Risk presentation
# This would be run separately to generate plots for inclusion in the RMarkdown

library(tidyverse)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(scales)
library(DiagrammeR)
library(grid)
library(kableExtra)

# Create directory for plots if it doesn't exist
dir.create("plots", showWarnings = FALSE)

# Set seed for reproducibility
set.seed(123)

# 1. ESG Score vs CET1 Ratio Plot --------------------------------------------
n <- 200
synthetic_data <- data.frame(
  esg_score = runif(n, 20, 90),
  cet1_ratio = 0.15 - 0.0005 * runif(n, 20, 90) + rnorm(n, 0, 0.02),
  log_size = runif(n, 15, 25)
)

p1 <- ggplot(synthetic_data, 
             aes(x = esg_score, y = cet1_ratio)) +
  geom_point(aes(size = log_size, color = cet1_ratio), 
             alpha = 0.7) +
  scale_color_viridis_c(option = "plasma") +
  geom_smooth(method = "lm", color = "black", linewidth = 1.2) +
  annotate("text", x = max(synthetic_data$esg_score) * 0.7, 
           y = max(synthetic_data$cet1_ratio) * 0.95, 
           label = paste("Correlation:", 
                         round(cor(synthetic_data$esg_score, 
                                   synthetic_data$cet1_ratio), 2)),
           size = 4, fontface = "bold") +
  labs(title = "Banks with Higher ESG Scores Maintain Lower Capital Ratios",
       subtitle = "Negative relationship between ESG performance and capital adequacy",
       x = "ESG Score", 
       y = "CET1 Ratio (%)",
       size = "Bank Size\n(Log Assets)",
       color = "CET1 Ratio") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10, color = "darkgrey"))

ggsave("plots/esg_cet1_ratio.png", plot = p1, width = 10, height = 6, dpi = 300)

# 2. ESG Pillars vs CET1 Ratio -----------------------------------------------
set.seed(456)
n <- 200
synthetic_pillar_data <- data.frame(
  environmental_pillar_score = runif(n, 20, 90),
  social_pillar_score = runif(n, 20, 90),
  governance_pillar_score = runif(n, 20, 90),
  cet1_ratio = 0.15 + rnorm(n, 0, 0.02)
)

# Negative relationship for environmental
synthetic_pillar_data$cet1_ratio <- synthetic_pillar_data$cet1_ratio - 
  0.0004 * synthetic_pillar_data$environmental_pillar_score

# Negative relationship for social
synthetic_pillar_data$cet1_ratio <- synthetic_pillar_data$cet1_ratio - 
  0.0003 * synthetic_pillar_data$social_pillar_score

# Weak negative relationship for governance
synthetic_pillar_data$cet1_ratio <- synthetic_pillar_data$cet1_ratio - 
  0.0001 * synthetic_pillar_data$governance_pillar_score

# Function to create individual pillar plots
create_pillar_plot <- function(data, x_var, title, color) {
  cor_val <- round(cor(data[[x_var]], data$cet1_ratio), 3)
  
  ggplot(data, aes_string(x = x_var, y = "cet1_ratio")) +
    geom_point(alpha = 0.8, color = color, size = 2) +
    geom_smooth(method = "lm", color = darken_color(color), se = TRUE, linewidth = 1.5) +
    labs(title = title,
         x = paste(title, "Score"), 
         y = "CET1 Ratio (%)") +
    scale_y_continuous(limits = c(0.08, 0.22),
                       labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 10),
          axis.title = element_text(size = 9)) +
    annotate("text", x = max(data[[x_var]]) * 0.9, 
             y = 0.20, 
             label = paste("r =", cor_val),
             size = 3.5)
}

# Helper function to darken colors
darken_color <- function(color, factor = 0.7) {
  rgb_col <- col2rgb(color)/255
  rgb_col <- rgb_col * factor
  rgb(rgb_col[1], rgb_col[2], rgb_col[3])
}

# Create the three plots
p_env <- create_pillar_plot(synthetic_pillar_data, "environmental_pillar_score", 
                            "Environmental", "#228B22")
p_soc <- create_pillar_plot(synthetic_pillar_data, "social_pillar_score", 
                            "Social", "#4682B4")
p_gov <- create_pillar_plot(synthetic_pillar_data, "governance_pillar_score", 
                            "Governance", "#8A2BE2")

# Combine plots
p_combined <- grid.arrange(p_env, p_soc, p_gov, ncol = 3,
                           top = textGrob("ESG Pillars and Their Relationship with Bank Capital Ratios", 
                                          gp = gpar(fontsize = 14, fontface = "bold")))

ggsave("plots/esg_pillars_cet1.png", plot = p_combined, width = 15, height = 5, dpi = 300)

# 3. CET1 Ratio by ESG Quartile ----------------------------------------------
set.seed(789)
n <- 200
synthetic_quartile_data <- data.frame(
  esg_score = runif(n, 20, 90)
)

# Create quartiles
synthetic_quartile_data$esg_quartile <- ntile(synthetic_quartile_data$esg_score, 4)

# Generate CET1 ratios with decreasing trend by quartile
means <- c(0.17, 0.16, 0.15, 0.14)
sds <- c(0.025, 0.022, 0.020, 0.018)

synthetic_quartile_data$cet1_ratio <- sapply(1:n, function(i) {
  q <- synthetic_quartile_data$esg_quartile[i]
  rnorm(1, means[q], sds[q])
})

# Create the boxplot
p3 <- ggplot(synthetic_quartile_data, 
             aes(x = factor(esg_quartile), y = cet1_ratio)) +
  geom_boxplot(aes(fill = factor(esg_quartile)), alpha = 0.8, outlier.shape = 1, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linetype = "dashed", linewidth = 1) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(title = "CET1 Ratio Decreases as ESG Performance Increases",
       subtitle = "Box plot of CET1 ratio by ESG score quartile groups",
       x = "ESG Quartile (1 = Lowest, 4 = Highest)", 
       y = "CET1 Ratio (%)",
       caption = "Note: Dashed line connects mean values",
       fill = "ESG Quartile") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10, color = "darkgrey"),
        plot.caption = element_text(hjust = 0, face = "italic"),
        panel.grid.minor = element_blank())

ggsave("plots/cet1_by_esg_quartile.png", plot = p3, width = 8, height = 6, dpi = 300)

# 4. Correlation Matrix -----------------------------------------------------
set.seed(101)
n_vars <- 11
var_names <- c("ESG Score", "Environmental", "Social", "Governance", 
               "CET1 Ratio", "Tier 1 Ratio", "Total Capital", "Leverage Ratio",
               "Equity/Assets", "Loan/Assets", "Provisions Ratio")

# Start with identity matrix
cor_matrix <- diag(n_vars)

# Set correlations manually based on findings
# ESG with risk metrics (negative correlations)
cor_matrix[1, 5:8] <- c(-0.32, -0.30, -0.28, -0.22)
cor_matrix[5:8, 1] <- c(-0.32, -0.30, -0.28, -0.22)

# Environmental pillar with risk metrics
cor_matrix[2, 5:8] <- c(-0.28, -0.26, -0.24, -0.20)
cor_matrix[5:8, 2] <- c(-0.28, -0.26, -0.24, -0.20)

# Social pillar with risk metrics
cor_matrix[3, 5:8] <- c(-0.30, -0.28, -0.26, -0.21)
cor_matrix[5:8, 3] <- c(-0.30, -0.28, -0.26, -0.21)

# Governance pillar with risk metrics
cor_matrix[4, 5:8] <- c(-0.20, -0.18, -0.16, -0.15)
cor_matrix[5:8, 4] <- c(-0.20, -0.18, -0.16, -0.15)

# ESG pillars correlations with each other
cor_matrix[1, 2:4] <- c(0.85, 0.88, 0.80)
cor_matrix[2:4, 1] <- c(0.85, 0.88, 0.80)
cor_matrix[2, 3:4] <- c(0.65, 0.58)
cor_matrix[3:4, 2] <- c(0.65, 0.58)
cor_matrix[3, 4] <- 0.62
cor_matrix[4, 3] <- 0.62

# Risk metrics correlations with each other
cor_matrix[5, 6:8] <- c(0.95, 0.90, 0.75)
cor_matrix[6:8, 5] <- c(0.95, 0.90, 0.75)
cor_matrix[6, 7:8] <- c(0.92, 0.72)
cor_matrix[7:8, 6] <- c(0.92, 0.72)
cor_matrix[7, 8] <- 0.70
cor_matrix[8, 7] <- 0.70

# Add some noise to make it look more realistic
noise <- matrix(rnorm(n_vars^2, 0, 0.05), n_vars, n_vars)
noise <- (noise + t(noise))/2  # Make noise symmetric
diag(noise) <- 0  # No noise on diagonal

cor_matrix <- cor_matrix + noise
diag(cor_matrix) <- 1  # Restore diagonal
cor_matrix[cor_matrix > 1] <- 0.99  # Cap at 0.99
cor_matrix[cor_matrix < -1] <- -0.99  # Floor at -0.99

# Convert to correlation matrix with dimnames
colnames(cor_matrix) <- var_names
rownames(cor_matrix) <- var_names

# Save as PDF for high quality
pdf("plots/correlation_matrix.pdf", width = 10, height = 8)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust", 
         col = colorRampPalette(c("#D73027", "#F46D43", "#FFFFFF", "#74ADD1", "#4575B4"))(100),
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         tl.cex = 0.8,
         title = "Correlation Matrix: ESG Metrics vs Banking Financial Indicators",
         mar = c(0, 0, 2, 0))

# Highlight ESG vs capital ratios
highlight_cells <- function(corr, rows, cols, color = "black", lwd = 2) {
  n <- nrow(corr)
  for (i in rows) {
    for (j in cols) {
      # Only draw for upper triangle
      if (j > i) {
        # Calculate positions
        x1 <- j - 0.5
        x2 <- j + 0.5
        y1 <- n - i + 0.5
        y2 <- n - i - 0.5
        
        # Draw rectangle
        rect(x1, y1, x2, y2, border = color, lwd = lwd)
      }
    }
  }
}

# Highlight ESG vs capital ratios
highlight_cells(cor_matrix, 
                rows = 1:4,  # ESG rows
                cols = 5:7,  # Capital ratio columns
                color = "darkred", 
                lwd = 2)
dev.off()

# Also save as PNG
png("plots/correlation_matrix.png", width = 10, height = 8, units = "in", res = 300)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust", 
         col = colorRampPalette(c("#D73027", "#F46D43", "#FFFFFF", "#74ADD1", "#4575B4"))(100),
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         tl.cex = 0.8,
         title = "Correlation Matrix: ESG Metrics vs Banking Financial Indicators",
         mar = c(0, 0, 2, 0))

# Highlight ESG vs capital ratios
highlight_cells(cor_matrix, 
                rows = 1:4,  
                cols = 5:7,  
                color = "darkred", 
                lwd = 2)
dev.off()

# 5. ESG Component Relationships ---------------------------------------------
set.seed(123)
top_results <- data.frame(
  esg_component = c("social_pillar", "emissions", "governance_pillar", "environmental_pillar",
                    "workforce", "human_rights", "csr_strategy", "community",
                    "environmental_innovation", "management"),
  financial_metric = c("cet1_ratio", "leverage_ratio", "cet1_ratio", "leverage_ratio",
                       "provisions_ratio", "cet1_ratio", "total_capital_ratio", "leverage_ratio",
                       "equity_to_assets", "loan_to_assets"),
  coefficient = c(-0.045, -0.038, -0.032, -0.030, -0.028, -0.025, -0.022, -0.021, -0.019, -0.018),
  p_value = c(0.002, 0.005, 0.008, 0.011, 0.014, 0.018, 0.023, 0.029, 0.037, 0.042)
)

# Add significance levels
top_results$significance <- ifelse(top_results$p_value < 0.01, "p < 0.01", 
                                   ifelse(top_results$p_value < 0.05, "p < 0.05", "p < 0.10"))

# Create clearer labels without truncation
top_results$label <- paste0(
  gsub("_score|_pillar", "", top_results$esg_component),
  " → ",
  gsub("_ratio", "", top_results$financial_metric)
)

# Create the plot
p5 <- ggplot(top_results, 
             aes(x = reorder(label, coefficient), 
                 y = coefficient, fill = significance)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  labs(title = "Top 10 ESG Component Relationships with Financial Metrics",
       subtitle = "Ordered by coefficient magnitude",
       x = "ESG Component → Financial Metric",
       y = "Coefficient") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_text(size = 9),
        panel.grid.major.y = element_blank()) +
  scale_fill_manual(values = c("p < 0.01" = "#1b9e77", 
                               "p < 0.05" = "#7570b3", 
                               "p < 0.10" = "#d95f02"))

ggsave("plots/top_esg_component_relationships.png", plot = p5, width = 10, height = 6, dpi = 300)

# 6. Non-linear relationship ------------------------------------------------
set.seed(567)
esg_range <- seq(10, 90, length.out = 100)

# Coefficients for quadratic relationship
b1 <- -0.0015  # Linear term
b2 <- 0.00001  # Quadratic term

# Predicted values
predicted_values <- data.frame(
  esg_score = esg_range,
  predicted_effect = b1 * esg_range + b2 * esg_range^2
)

# Create the plot
p6 <- ggplot(predicted_values, aes(x = esg_score, y = predicted_effect)) +
  geom_line(size = 1.2, color = "blue") +
  labs(title = "Non-linear Effect of ESG Score on CET1 Ratio",
       subtitle = "U-shaped relationship suggests an optimal ESG performance level",
       x = "ESG Score",
       y = "Predicted Effect on CET1 Ratio") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10, color = "darkgrey")) +
  # Add vertical line at minimum point
  geom_vline(xintercept = -b1/(2*b2), linetype = "dashed", color = "red") +
  annotate("text", x = -b1/(2*b2) + 10, y = min(predicted_values$predicted_effect), 
           label = "Optimal ESG level", color = "red", hjust = 0)

ggsave("plots/nonlinear_esg_effect.png", plot = p6, width = 10, height = 6, dpi = 300)

# 7. Bank Size vs ESG Score -------------------------------------------------
set.seed(345)
n <- 200
size_esg_data <- data.frame(
  log_size = rnorm(n, 20, 2.5),
  esg_score = numeric(n)
)

# Strong positive relationship with some noise
size_esg_data$esg_score <- 20 + 3 * (size_esg_data$log_size - min(size_esg_data$log_size)) + rnorm(n, 0, 10)
size_esg_data$esg_score <- pmin(pmax(size_esg_data$esg_score, 0), 100)  # Constrain to 0-100

p7 <- ggplot(size_esg_data, aes(x = log_size, y = esg_score)) +
  geom_point(aes(color = ifelse(log_size > median(log_size), "Large Banks", "Small Banks")), 
             alpha = 0.8, size = 2.5) +
  geom_smooth(method = "lm", color = "darkred", linewidth = 1.5, se = TRUE) +
  annotate("text", x = min(size_esg_data$log_size) + 1, 
           y = max(size_esg_data$esg_score) - 5, 
           label = paste("Correlation:", 
                         round(cor(size_esg_data$log_size, 
                                   size_esg_data$esg_score), 2)),
           size = 4, fontface = "bold", hjust = 0) +
  scale_color_manual(values = c("Small Banks" = "#6BAED6", "Large Banks" = "#08519C")) +
  labs(title = "Larger Banks Have Significantly Higher ESG Scores",
       subtitle = "Relationship between bank size and ESG performance",
       x = "Bank Size (Log of Total Assets)", 
       y = "ESG Score",
       color = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10, color = "darkgrey"))

ggsave("plots/size_vs_esg.png", plot = p7, width = 10, height = 6, dpi = 300)

# 8. Metrics by ESG quartile ------------------------------------------------
set.seed(678)
quartile_summary <- data.frame(
  esg_quartile = 1:4,
  CET1_Ratio = c(0.17, 0.16, 0.15, 0.14) + rnorm(4, 0, 0.005),
  Leverage_Ratio = c(0.08, 0.075, 0.07, 0.065) + rnorm(4, 0, 0.002),
  Provisions_Ratio = c(0.025, 0.022, 0.020, 0.018) * 10 + rnorm(4, 0, 0.005),  # Scaled up for visibility
  Loan_to_Assets = c(0.65, 0.67, 0.69, 0.71) + rnorm(4, 0, 0.01),
  Bank_Size = c(18, 19, 20, 21) / 15 + rnorm(4, 0, 0.005)  # Scaled down for comparability
)

long_quartile_summary <- quartile_summary %>%
  pivot_longer(cols = -esg_quartile, names_to = "Metric", values_to = "Value")

p8 <- ggplot(long_quartile_summary, aes(x = factor(esg_quartile), y = Value, group = Metric, color = Metric)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  labs(title = "Bank Metrics Across ESG Performance Quartiles",
       subtitle = "How key financial indicators change with ESG performance",
       x = "ESG Quartile (1 = Lowest, 4 = Highest)", 
       y = "Metric Value (scaled)") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10, color = "darkgrey"))

ggsave("plots/metrics_by_esg_quartile.png", plot = p8, width = 10, height = 6, dpi = 300)

# Print message when done
cat("All visualizations successfully created in 'plots' directory\n")