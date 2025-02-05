library(readr); library(readxl); library(dplyr); library(lubridate)

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
  mutate(Period = as.character(Period)) %>%
  mutate(Period = case_when(
    Period == "202309" ~ "2023-09-30",
    Period == "202312" ~ "2023-12-31",
    Period == "202403" ~ "2024-03-31",
    Period == "202406" ~ "2024-06-30",
    TRUE ~ Period
  ))
  
# This deltetes all other banks, being 606922-598782 banks = 8140 banks
remove_invalid_lei <- function(data) {
  data %>% filter(LEI_Code != "XXXXXXXXXXXXXXXXXXXX")
}
credit_risk <- remove_invalid_lei(credit_risk)