library(dplyr); library(janitor); library(tidyverse); library(ggplot2)
library(corrplot)

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

# 2.3 Bank Distribution (Top 10)
bank_distribution <- data_analysis %>%
  group_by(name) %>%
  summarise(n_years = n_distinct(year)) %>%
  arrange(desc(n_years))
print("Top 10 Banks by Number of Years in Dataset:")
print(head(bank_distribution, 10))



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

# Visualizations
p1 <- ggplot(data_analysis, 
             aes(x = esg_score, 
                 y = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition)) +
  geom_point(aes(color = country), alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "ESG Score vs CET1 Ratio",
       x = "ESG Score", 
       y = "CET1 Ratio (%)",
       caption = "Data source: ESG and financial data") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend for better visibility

# Export to jpg
ggsave("plots/esg_cet1_ratio.jpg", plot = p1, width = 10, height = 6, dpi = 300)

# 3.2 ESG pillars vs CET1 ratio
# Environmental pillar
p2 <- ggplot(data_analysis, 
             aes(x = environmental_pillar_score, 
                 y = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", color = "green4") +
  labs(title = "Environmental Score vs CET1 Ratio",
       x = "Environmental Score", 
       y = "CET1 Ratio (%)") +
  theme_minimal()

# Social pillar
p3 <- ggplot(data_analysis, 
             aes(x = social_pillar_score, 
                 y = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkblue") +
  labs(title = "Social Score vs CET1 Ratio",
       x = "Social Score", 
       y = "CET1 Ratio (%)") +
  theme_minimal()

# Governance pillar
p4 <- ggplot(data_analysis, 
             aes(x = governance_pillar_score, 
                 y = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition)) +
  geom_point(alpha = 0.7, color = "purple") +
  geom_smooth(method = "lm", color = "darkviolet") +
  labs(title = "Governance Score vs CET1 Ratio",
       x = "Governance Score", 
       y = "CET1 Ratio (%)") +
  theme_minimal()

# Combine and save
library(gridExtra)
p_combined <- grid.arrange(p2, p3, p4, ncol = 3)
ggsave("plots/esg_pillars_cet1.jpg", plot = p_combined, width = 15, height = 5, dpi = 300)

# 3.3 Boxplot of CET1 ratio by ESG quartile
p5 <- ggplot(data_analysis_with_esg, 
             aes(x = factor(esg_quartile), 
                 y = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition)) +
  geom_boxplot(aes(fill = factor(esg_quartile)), alpha = 0.7) +
  labs(title = "CET1 Ratio by ESG Quartile",
       x = "ESG Quartile (1 = Lowest, 4 = Highest)", 
       y = "CET1 Ratio (%)",
       fill = "ESG Quartile") +
  theme_minimal()
ggsave("plots/cet1_by_esg_quartile.jpg", plot = p5, width = 10, height = 6, dpi = 300)

# 4. CORRELATION ANALYSIS
# 4.1 Calculate correlations
esg_financial_cors <- data_analysis %>%
  select(esg_score, environmental_pillar_score, social_pillar_score, governance_pillar_score,
         common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition,
         leverage_ratio_using_a_transitional_definition_of_tier_1_capital,
         equity_to_assets, loan_to_assets, provisions_ratio) %>%
  cor(use = "pairwise.complete.obs")

# 4.2 Print correlation matrix
print("Correlation Matrix:")
print(round(esg_financial_cors, 3))

# 4.3 Export correlation to csv 
write.csv(esg_financial_cors, "results/esg_financial_cors.csv")

# 4.4 Visualize correlation matrix
pdf("plots/correlation_matrix.pdf", width = 10, height = 8)
corrplot(esg_financial_cors, method = "color", 
         type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", # Add correlation coefficients
         number.cex = 0.7,      # Size of correlation coefficients
         tl.cex = 0.7)          # Size of text labels
dev.off()

# 5. ADDITIONAL EXPLORATION
# 5.1 Time trends in ESG scores
p6 <- ggplot(data_analysis_with_esg, aes(x = year_factor, y = esg_score)) +
  geom_boxplot(aes(fill = year_factor), alpha = 0.7) +
  labs(title = "ESG Scores by Year",
       x = "Year", 
       y = "ESG Score") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("plots/esg_score_by_year.jpg", plot = p6, width = 10, height = 6, dpi = 300)

# 5.2 Bank size vs ESG score
p7 <- ggplot(data_analysis, aes(x = log_assets, y = esg_score)) +
  geom_point(aes(color = country), alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Bank Size vs ESG Score",
       x = "Log(Total Assets)", 
       y = "ESG Score") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("plots/size_vs_esg.jpg", plot = p7, width = 10, height = 6, dpi = 300)
