library(readr); library(readxl); library(dplyr)

sovereign_debt <- read_csv("data/Sovereign debt exposures.csv")
other_templates <- read_csv("data/Other templates.csv")
credit_risk <- read_csv("data/Credit risk.csv")
market_risk <- read_csv("data/Market risk.csv")

metadata <- read_excel("data/Metadata.xlsx", 
                       sheet = "List of Institutions",
                       skip = 1) %>%
  select(LEI_code = 3, Bank_Name = 4) %>% 
  rename(LEI_Code = LEI_code)

credit_risk <- credit_risk %>%
  left_join(metadata, by = "LEI_Code")

credit_risk <- credit_risk %>%
  relocate(Bank_Name, .after = LEI_Code)

credit_risk <- credit_risk %>%
  mutate(
    Year = year(Period),  # Extract year
    Month = month(Period) # Extract month
  )