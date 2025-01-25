library(readr); library(tidyverse); library(zoo)

sovereign_debt <- read_csv("data/Sovereign debt exposures.csv")
other_templates <- read_csv("data/Other templates.csv")
credit_risk <- read_csv("data/Credit risk.csv")
market_risk <- read_csv("data/Market risk.csv")

# Credit risk
# Function to convert LEI codes to bank name
credit_risk <- convert_lei_to_bank_name(credit_risk)
credit_risk <- convert_status_code(credit_risk)
credit_risk <- convert_exposure_code(credit_risk)
