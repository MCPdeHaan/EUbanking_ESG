library(dplyr); library(janitor); library(tidyverse); library(ggplot2)
library(corrplot); library(gridExtra); library(scales); library(grid)

# Summary statistics
data_analysis %>%
  summarise(
    total_banks = n_distinct(lei_code),
    total_years = n_distinct(year),
    total_obs = n(),
    missing_esg = sum(is.na(esg_score)),
    missing_cet1 = sum(is.na(cet1_ratio))
  )

data_analysis %>%
  group_by(year) %>%
  summarise(
    n_obs = n(),
    n_banks = n_distinct(lei_code)
  ) %>%
  arrange(year)

# Summary statistics by ESG category
data_analysis %>%
  group_by(esg_category) %>%
  summarise(
    n = n(),
    # Capital ratios
    cet1_ratio_mean = mean(cet1_ratio, na.rm = TRUE),
    cet1_ratio_sd = sd(cet1_ratio, na.rm = TRUE),
    tier1_ratio_mean = mean(tier1_ratio, na.rm = TRUE),
    tier1_ratio_sd = sd(tier1_ratio, na.rm = TRUE),
    total_capital_ratio_mean = mean(total_capital_ratio, na.rm = TRUE),
    total_capital_ratio_sd = sd(total_capital_ratio, na.rm = TRUE),
    
    # Leverage
    leverage_ratio_mean = mean(leverage_ratio, na.rm = TRUE),
    leverage_ratio_sd = sd(leverage_ratio, na.rm = TRUE),
    
    # Risk-weighted assets
    rwa_ratio_mean = mean(rwa_ratio, na.rm = TRUE),
    rwa_ratio_sd = sd(rwa_ratio, na.rm = TRUE),
    
    # Size
    log_size_mean = mean(log_size, na.rm = TRUE),
    log_size_sd = sd(log_size, na.rm = TRUE)
  )

# MAIN RELATIONSHIP: ESG Score vs CET1 Ratio
p1 <- ggplot(data_analysis, 
             aes(x = esg_score, y = cet1_ratio)) +
  # Size points by bank size to show additional dimension
  geom_point(aes(size = log_size, color = cet1_ratio), 
             alpha = 0.7) +
  # Use a heatmap color scale for capital ratio
  scale_color_viridis_c(option = "plasma") +
  # Better fit line with confidence interval
  geom_smooth(method = "lm", color = "black", linewidth = 1.2) +
  # Add correlation annotation
  annotate("text", x = max(data_analysis$esg_score, na.rm = TRUE) * 0.7, 
           y = max(data_analysis$cet1_ratio, na.rm = TRUE) * 0.95, 
           label = paste("Correlation:", 
                         round(cor(data_analysis$esg_score, 
                                   data_analysis$cet1_ratio, 
                                   use = "pairwise.complete.obs"), 2)),
           size = 4, fontface = "bold") +
  # labels
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

# ESG PILLARS vs CET1 RATIO
# Environmental pillar
p2 <- ggplot(data_analysis, 
             aes(x = environmental_pillar_score, y = cet1_ratio)) +
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
p2 <- p2 + annotate("text", x = max(data_analysis$environmental_pillar_score, na.rm = TRUE) * 0.9, 
                    y = 0.85, 
                    label = paste("r =", round(cor(data_analysis$environmental_pillar_score, 
                                                   data_analysis$cet1_ratio, 
                                                   use = "pairwise.complete.obs"), 3)),
                    size = 3.5)

# Social pillar
p3 <- ggplot(data_analysis, 
             aes(x = social_pillar_score, y = cet1_ratio)) +
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
p3 <- p3 + annotate("text", x = max(data_analysis$social_pillar_score, na.rm = TRUE) * 0.9, 
                    y = 0.85, 
                    label = paste("r =", round(cor(data_analysis$social_pillar_score, 
                                                   data_analysis$cet1_ratio, 
                                                   use = "pairwise.complete.obs"), 3)),
                    size = 3.5)

# Governance pillar
p4 <- ggplot(data_analysis, 
             aes(x = governance_pillar_score, y = cet1_ratio)) +
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
p4 <- p4 + annotate("text", x = max(data_analysis$governance_pillar_score, na.rm = TRUE) * 0.9, 
                    y = 0.85, 
                    label = paste("r =", round(cor(data_analysis$governance_pillar_score, 
                                                   data_analysis$cet1_ratio, 
                                                   use = "pairwise.complete.obs"), 3)),
                    size = 3.5)

# Combine with a descriptive title
p_combined <- grid.arrange(p2, p3, p4, ncol = 3,
                           top = textGrob("ESG Pillars and Their Relationship with Bank Capital Ratios", 
                                          gp = gpar(fontsize = 12, fontface = "bold")))

ggsave("plots/esg_pillars_cet1.jpg", plot = p_combined, width = 15, height = 5, dpi = 300)

# CET1 RATIO BY ESG QUARTILE
p5 <- ggplot(data_analysis, 
                      aes(x = factor(esg_quartile), y = cet1_ratio)) +
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

ggsave("plots/cet1_by_esg_quartile.jpg", plot = p5, width = 8, height = 6, dpi = 300)

# BANK SIZE vs ESG SCORE
p7 <- ggplot(data_analysis, aes(x = log_size, y = esg_score)) +
  # Add a subtle gradient color by bank size group
  geom_point(aes(color = ifelse(log_size > median(log_size, na.rm = TRUE), "Large Banks", "Small Banks")), 
             alpha = 0.8, size = 2.5) +
  geom_smooth(method = "lm", color = "darkred", linewidth = 1.5, se = TRUE) +
  # Add correlation annotation
  annotate("text", x = min(data_analysis$log_size, na.rm = TRUE) + 1, 
           y = max(data_analysis$esg_score, na.rm = TRUE) - 5, 
           label = paste("Correlation:", 
                         round(cor(data_analysis$log_size, 
                                   data_analysis$esg_score, 
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

ggsave("plots/size_vs_esg.jpg", plot = p7, width = 10, height = 6, dpi = 300)

# ADDITIONAL EXPLORATORY ANALYSIS
# Provisions ratio vs ESG
p10 <- ggplot(data_analysis, aes(x = esg_score, y = provisions_ratio)) +
  geom_point(aes(color = log_size), alpha = 0.7, size = 2.5) +
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

# Leverage ratio vs ESG
p11 <- ggplot(data_analysis, aes(x = esg_score, y = leverage_ratio)) +
  geom_point(aes(color = log_size), alpha = 0.7, size = 2.5) +
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

# ESG SCORE DISTRIBUTION
p13 <- ggplot(data_analysis, aes(x = esg_score)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_density(aes(y = after_stat(count) * 3), color = "darkred", linewidth = 1) +
  labs(title = "Distribution of ESG Scores",
       subtitle = "Histogram with density overlay",
       x = "ESG Score", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10, color = "darkgrey"))

ggsave("plots/esg_score_distribution.jpg", plot = p13, width = 10, height = 6, dpi = 300)

# EXAMINING RELATIONSHIP BETWEEN ESG AND CAPITAL BY BANK SIZE
# Split banks into size quartiles
data_analysis$size_quartile <- ntile(data_analysis$log_size, 4)

# Plot ESG vs CET1 by size quartile
p14 <- ggplot(data_analysis, 
              aes(x = esg_score, y = cet1_ratio)) +
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

# FINAL DIAGNOSTIC PLOTS
# Summary plot comparing key metrics across ESG quartiles
quartile_summary <- data_analysis %>%
  group_by(esg_quartile) %>%
  summarise(
    CET1_Ratio = mean(cet1_ratio, na.rm = TRUE),
    Leverage_Ratio = mean(leverage_ratio, na.rm = TRUE),
    Provisions_Ratio = mean(provisions_ratio, na.rm = TRUE) * 10, # Scale up for visibility
    Loan_to_Assets = mean(loan_to_assets, na.rm = TRUE),
    Bank_Size = mean(log_size, na.rm = TRUE) / 15 # Scale down for comparability
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