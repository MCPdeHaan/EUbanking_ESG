library(dplyr); library(janitor); library(tidyverse); library(ggplot2)
library(corrplot); library(gridExtra); library(scales); library(grid)

# Exploratory data analysis
# Summary dataset
dataset_summary <- data_analysis %>%
  summarise(
    total_banks = n_distinct(lei_code),
    total_years = n_distinct(year),
    total_obs = n(),
    missing_esg = sum(is.na(esg_score)),
    missing_cet1 = sum(is.na(common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition))
  )
print("Dataset Summary:")
print(dataset_summary)

year_distribution <- data_analysis %>%
  group_by(year) %>%
  summarise(
    n_obs = n(),
    n_banks = n_distinct(lei_code)
  ) %>%
  arrange(year)
print("Distribution by Year:")
print(year_distribution)

# Bank Distribution (Top 10)
bank_distribution <- data_analysis %>%
  group_by(name) %>%
  summarise(n_years = n_distinct(year)) %>%
  arrange(desc(n_years))
print("Top 10 Banks by Number of Years in Dataset:")
print(head(bank_distribution, 10))

# Export summary statistics to CSV
write.csv(dataset_summary, "results/dataset_summary.csv", row.names = FALSE)
write.csv(year_distribution, "results/year_distribution.csv", row.names = FALSE)
write.csv(bank_distribution, "results/bank_distribution.csv", row.names = FALSE)

# Summary statistics by ESG category
# Create a filtered dataset for analysis requiring complete cases
data_analysis_with_esg <- data_analysis %>%
  filter(!is.na(esg_score))

summary_stats <- data_analysis_with_esg %>%
  group_by(esg_category) %>%
  summarise(
    n = n(),
    # Capital ratios
    cet1_ratio_mean = mean(common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition, 
                           na.rm = TRUE),
    cet1_ratio_sd = sd(common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition, 
                       na.rm = TRUE),
    tier1_ratio_mean = mean(tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition, 
                            na.rm = TRUE),
    tier1_ratio_sd = sd(tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition, 
                        na.rm = TRUE),
    total_capital_ratio_mean = mean(total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition, 
                                    na.rm = TRUE),
    total_capital_ratio_sd = sd(total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition, 
                                na.rm = TRUE),
    
    # Leverage
    leverage_ratio_mean = mean(leverage_ratio_using_a_transitional_definition_of_tier_1_capital, 
                               na.rm = TRUE),
    leverage_ratio_sd = sd(leverage_ratio_using_a_transitional_definition_of_tier_1_capital, 
                           na.rm = TRUE),
    
    # Risk-weighted assets
    rwa_ratio_mean = mean(rwa_ratio, na.rm = TRUE),
    rwa_ratio_sd = sd(rwa_ratio, na.rm = TRUE),
    
    # Size
    log_assets_mean = mean(log_assets, na.rm = TRUE),
    log_assets_sd = sd(log_assets, na.rm = TRUE)
  )
print("Summary Statistics by ESG Category:")
print(summary_stats)

# Export summary statistics by ESG category to CSV
write.csv(summary_stats, "results/summary_stats_by_esg_category.csv", row.names = FALSE)

# 1. MAIN RELATIONSHIP: ESG Score vs CET1 Ratio
p1 <- ggplot(data_analysis_with_esg, 
                      aes(x = esg_score, 
                          y = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition)) +
  # Size points by bank size to show additional dimension
  geom_point(aes(size = log_assets, color = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition), 
             alpha = 0.7) +
  # Use a heatmap color scale for capital ratio
  scale_color_viridis_c(option = "plasma") +
  # Better fit line with confidence interval
  geom_smooth(method = "lm", color = "black", linewidth = 1.2) +
  # Add correlation annotation
  annotate("text", x = max(data_analysis_with_esg$esg_score, na.rm = TRUE) * 0.7, 
           y = max(data_analysis_with_esg$common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition, na.rm = TRUE) * 0.95, 
           label = paste("Correlation:", 
                         round(cor(data_analysis_with_esg$esg_score, 
                                   data_analysis_with_esg$common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition, 
                                   use = "pairwise.complete.obs"), 2)),
           size = 4, fontface = "bold") +
  # Improved labels
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

ggsave("plots/esg_cet1_ratio.jpg", plot = p1, width = 10, height = 6, dpi = 300)

# 2. ESG PILLARS vs CET1 RATIO
# Environmental pillar
p2 <- ggplot(data_analysis_with_esg, 
             aes(x = environmental_pillar_score, 
                 y = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition)) +
  geom_point(alpha = 0.8, color = "#228B22", size = 2) +
  geom_smooth(method = "lm", color = "#006400", se = TRUE, linewidth = 1.5) +
  labs(title = "Environmental Score vs CET1 Ratio",
       x = "Environmental Score", 
       y = "CET1 Ratio (%)") +
  scale_y_continuous(limits = c(0.4, 0.9)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 10),
        axis.title = element_text(size = 9))

# Add correlation value to panel
p2 <- p2 + annotate("text", x = max(data_analysis_with_esg$environmental_pillar_score, na.rm = TRUE) * 0.9, 
                    y = 0.85, 
                    label = paste("r =", round(cor(data_analysis_with_esg$environmental_pillar_score, 
                                                   data_analysis_with_esg$common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition, 
                                                   use = "pairwise.complete.obs"), 3)),
                    size = 3.5)

# Social pillar
p3 <- ggplot(data_analysis_with_esg, 
             aes(x = social_pillar_score, 
                 y = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition)) +
  geom_point(alpha = 0.8, color = "#4682B4", size = 2) +
  geom_smooth(method = "lm", color = "#000080", se = TRUE, linewidth = 1.5) +
  labs(title = "Social Score vs CET1 Ratio",
       x = "Social Score", 
       y = "CET1 Ratio (%)") +
  scale_y_continuous(limits = c(0.4, 0.9)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 10),
        axis.title = element_text(size = 9))

# Add correlation value to panel
p3 <- p3 + annotate("text", x = max(data_analysis_with_esg$social_pillar_score, na.rm = TRUE) * 0.9, 
                    y = 0.85, 
                    label = paste("r =", round(cor(data_analysis_with_esg$social_pillar_score, 
                                                   data_analysis_with_esg$common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition, 
                                                   use = "pairwise.complete.obs"), 3)),
                    size = 3.5)

# Governance pillar
p4 <- ggplot(data_analysis_with_esg, 
             aes(x = governance_pillar_score, 
                 y = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition)) +
  geom_point(alpha = 0.8, color = "#8A2BE2", size = 2) +
  geom_smooth(method = "lm", color = "#4B0082", se = TRUE, linewidth = 1.5) +
  labs(title = "Governance Score vs CET1 Ratio",
       x = "Governance Score", 
       y = "CET1 Ratio (%)") +
  scale_y_continuous(limits = c(0.4, 0.9)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 10),
        axis.title = element_text(size = 9))

# Add correlation value to panel
p4 <- p4 + annotate("text", x = max(data_analysis_with_esg$governance_pillar_score, na.rm = TRUE) * 0.9, 
                    y = 0.85, 
                    label = paste("r =", round(cor(data_analysis_with_esg$governance_pillar_score, 
                                                   data_analysis_with_esg$common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition, 
                                                   use = "pairwise.complete.obs"), 3)),
                    size = 3.5)

# Combine with a descriptive title
p_combined <- grid.arrange(p2, p3, p4, ncol = 3,
                           top = textGrob("ESG Pillars and Their Relationship with Bank Capital Ratios", 
                                          gp = gpar(fontsize = 12, fontface = "bold")))

ggsave("plots/esg_pillars_cet1_improved.jpg", plot = p_combined, width = 15, height = 5, dpi = 300)

# 3. CET1 RATIO BY ESG QUARTILE
p5_improved <- ggplot(data_analysis_with_esg, 
                      aes(x = factor(esg_quartile), 
                          y = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition)) +
  geom_boxplot(aes(fill = factor(esg_quartile)), alpha = 0.8, outlier.shape = 1, outlier.size = 2) +
  # Add mean points with connecting line to highlight trend
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linetype = "dashed", linewidth = 1) +
  # Better colors with clear distinction
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

ggsave("plots/cet1_by_esg_quartile_improved.jpg", plot = p5_improved, width = 8, height = 6, dpi = 300)

# 4. CORRELATION ANALYSIS
# 4.1 Calculate correlations with shorter variable names for clarity
esg_financial_cors_renamed <- data_analysis_with_esg %>%
  select(esg_score, environmental_pillar_score, social_pillar_score, governance_pillar_score,
         common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         leverage_ratio_using_a_transitional_definition_of_tier_1_capital,
         equity_to_assets, loan_to_assets, provisions_ratio) %>%
  rename("ESG Score" = esg_score,
         "Environmental" = environmental_pillar_score,
         "Social" = social_pillar_score,
         "Governance" = governance_pillar_score,
         "CET1 Ratio" = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         "Tier 1 Ratio" = tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         "Total Capital" = total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         "Leverage Ratio" = leverage_ratio_using_a_transitional_definition_of_tier_1_capital,
         "Equity/Assets" = equity_to_assets,
         "Loan/Assets" = loan_to_assets,
         "Provisions Ratio" = provisions_ratio) %>%
  cor(use = "pairwise.complete.obs")

# 4.2 Print correlation matrix
print("Correlation Matrix:")
print(round(esg_financial_cors_renamed, 3))

# 4.3 Export correlation to csv 
write.csv(esg_financial_cors_renamed, "results/esg_financial_cors_renamed.csv")

# Original correlation matrix (for compatibility with existing code)
esg_financial_cors <- data_analysis %>%
  select(esg_score, environmental_pillar_score, social_pillar_score, governance_pillar_score,
         common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         leverage_ratio_using_a_transitional_definition_of_tier_1_capital,
         equity_to_assets, loan_to_assets, provisions_ratio) %>%
  cor(use = "pairwise.complete.obs")

write.csv(esg_financial_cors, "results/esg_financial_cors.csv")

# 4.4 Improved correlation plot
# Define the highlight cells function
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

# Create both PDF and PNG versions
pdf("plots/correlation_matrix_improved.pdf", width = 10, height = 8)
corrplot(esg_financial_cors_renamed, 
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
         mar = c(0, 0, 1, 0))

# Highlight ESG vs capital ratios
highlight_cells(esg_financial_cors_renamed, 
                rows = 1:4,  # ESG rows
                cols = 5:7,  # Capital ratio columns
                color = "darkred", 
                lwd = 2)
dev.off()

png("plots/correlation_matrix_improved.png", width = 10, height = 8, units = "in", res = 300)
corrplot(esg_financial_cors_renamed, 
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
         mar = c(0, 0, 1, 0))

# Highlight ESG vs capital ratios
highlight_cells(esg_financial_cors_renamed, 
                rows = 1:4,  # ESG rows
                cols = 5:7,  # Capital ratio columns
                color = "darkred", 
                lwd = 2)
dev.off()

# 5. BANK SIZE vs ESG SCORE
p7_improved <- ggplot(data_analysis_with_esg, aes(x = log_assets, y = esg_score)) +
  # Add a subtle gradient color by bank size group
  geom_point(aes(color = ifelse(log_assets > median(log_assets, na.rm = TRUE), "Large Banks", "Small Banks")), 
             alpha = 0.8, size = 2.5) +
  geom_smooth(method = "lm", color = "darkred", linewidth = 1.5, se = TRUE) +
  # Add correlation annotation
  annotate("text", x = min(data_analysis_with_esg$log_assets, na.rm = TRUE) + 1, 
           y = max(data_analysis_with_esg$esg_score, na.rm = TRUE) - 5, 
           label = paste("Correlation:", 
                         round(cor(data_analysis_with_esg$log_assets, 
                                   data_analysis_with_esg$esg_score, 
                                   use = "pairwise.complete.obs"), 2)),
           size = 4, fontface = "bold", hjust = 0) +
  # Better colors
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

ggsave("plots/size_vs_esg_improved.jpg", plot = p7_improved, width = 10, height = 6, dpi = 300)

# 7. ADDITIONAL EXPLORATORY ANALYSIS
# 7.3 Provisions ratio vs ESG
p10 <- ggplot(data_analysis_with_esg, aes(x = esg_score, y = provisions_ratio)) +
  geom_point(aes(color = log_assets), alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  scale_color_viridis_c(option = "plasma") +
  labs(title = "ESG Score vs Provisions Ratio",
       subtitle = "Relationship between ESG performance and credit risk management",
       x = "ESG Score", 
       y = "Provisions Ratio",
       color = "Bank Size\n(Log Assets)") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10, color = "darkgrey"))

ggsave("plots/esg_vs_provisions.jpg", plot = p10, width = 10, height = 6, dpi = 300)

# 7.4 Leverage ratio vs ESG
p11 <- ggplot(data_analysis_with_esg, aes(x = esg_score, y = leverage_ratio_using_a_transitional_definition_of_tier_1_capital)) +
  geom_point(aes(color = log_assets), alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  scale_color_viridis_c(option = "plasma") +
  labs(title = "ESG Score vs Leverage Ratio",
       subtitle = "Relationship between ESG performance and leverage",
       x = "ESG Score", 
       y = "Leverage Ratio",
       color = "Bank Size\n(Log Assets)") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10, color = "darkgrey"))

ggsave("plots/esg_vs_leverage.jpg", plot = p11, width = 10, height = 6, dpi = 300)



# 8. ESG SCORE DISTRIBUTION
p13 <- ggplot(data_analysis_with_esg, aes(x = esg_score)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_density(aes(y = ..count.. * 3), color = "darkred", linewidth = 1) +
  labs(title = "Distribution of ESG Scores",
       subtitle = "Histogram with density overlay",
       x = "ESG Score", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10, color = "darkgrey"))

ggsave("plots/esg_score_distribution.jpg", plot = p13, width = 10, height = 6, dpi = 300)

# 9. EXAMINING RELATIONSHIP BETWEEN ESG AND CAPITAL BY BANK SIZE
# Split banks into size quartiles
data_analysis_with_esg$size_quartile <- ntile(data_analysis_with_esg$log_assets, 4)

# Plot ESG vs CET1 by size quartile
p14 <- ggplot(data_analysis_with_esg, 
              aes(x = esg_score, 
                  y = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition)) +
  geom_point(aes(color = factor(size_quartile)), alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  geom_smooth(aes(color = factor(size_quartile)), method = "lm", se = FALSE, linetype = "dashed") +
  scale_color_brewer(palette = "Set1", name = "Bank Size Quartile\n(1=Smallest, 4=Largest)") +
  labs(title = "ESG-Capital Relationship By Bank Size",
       subtitle = "Examining whether the relationship differs by bank size",
       x = "ESG Score", 
       y = "CET1 Ratio (%)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10, color = "darkgrey"))

ggsave("plots/esg_cet1_by_size.jpg", plot = p14, width = 10, height = 6, dpi = 300)

# 12. FINAL DIAGNOSTIC PLOTS
# Summary plot comparing key metrics across ESG quartiles
quartile_summary <- data_analysis_with_esg %>%
  group_by(esg_quartile) %>%
  summarise(
    CET1_Ratio = mean(common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition, na.rm = TRUE),
    Leverage_Ratio = mean(leverage_ratio_using_a_transitional_definition_of_tier_1_capital, na.rm = TRUE),
    Provisions_Ratio = mean(provisions_ratio, na.rm = TRUE) * 10, # Scale up for visibility
    Loan_to_Assets = mean(loan_to_assets, na.rm = TRUE),
    Bank_Size = mean(log_assets, na.rm = TRUE) / 15 # Scale down for comparability
  )

long_quartile_summary <- quartile_summary %>%
  pivot_longer(cols = -esg_quartile, names_to = "Metric", values_to = "Value")

p16 <- ggplot(long_quartile_summary, aes(x = factor(esg_quartile), y = Value, group = Metric, color = Metric)) +
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

ggsave("plots/metrics_by_esg_quartile.jpg", plot = p16, width = 10, height = 6, dpi = 300)
