library(tidyverse)

financial <- read_csv("data/Historical KM.csv")

financial_year <- read_csv("data/Historical KM.csv") %>%
  # Convert Period to character and extract Year as an integer
  mutate(
    Period = as.character(Period),
    Year = as.integer(str_sub(Period, 1, 4))
  ) %>%
  # Exclude periods starting with "2019" or "2024"
  filter(!str_starts(Period, "2019") & !str_starts(Period, "2024")) %>%
  # Aggregate to one row per bank-year-label
  group_by(LEI_code, Name, NSA, Country, Year, Label) %>%
  summarise(Amount = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
  # Pivot data so each label becomes a column
  pivot_wider(
    names_from = Label,
    values_from = Amount,
    values_fill = list(Amount = 0)
  ) %>%
  # Remove rows with invalid LEI_code and drop the NSA column
  filter(LEI_code != "XXXXXXXXXXXXXXXXXXXX") %>%
  select(-NSA) %>%
  # Clean up LEI_code by trimming whitespace
  mutate(LEI_code = trimws(LEI_code))

glimpse(financial_year)
colnames(financial_year)
summary(financial_year)