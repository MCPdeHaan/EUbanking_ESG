 library(tidyverse)

financial <- read_csv("data/Historical KM.csv")

financial <- financial %>% 
  mutate(Period = as.character(Period))

financial <- financial %>%
  filter(!str_starts(Period, "2019") & !str_starts(Period, "2024"))

financial_year <- financial %>%
  mutate(Year = str_sub(Period, 1, 4)) %>%  
  group_by(LEI_code, Name, NSA, Country, Year, Item, Label, Sheet) %>% 
  summarise(Amount = sum(Amount, na.rm = TRUE), .groups = "drop")

financial_year <- financial_year %>%
  mutate(Year = as.numeric(Year))

# Aggregate to one row per bank-year-label
financial_year <- financial_year %>%
  # Group by identifiers + Label 
  group_by(LEI_code, Name, NSA, Country, Year, Label) %>%
  summarise(Amount = sum(Amount, na.rm = TRUE), .groups = "drop")

financial_year %>%
  count(LEI_code, Year, Label) %>%
  filter(n > 1)  # Should return 0 rows -> correct


financial_year <- financial_year %>%
  pivot_wider(
    names_from = Label,
    values_from = Amount,
    values_fill = list(Amount = 0)
  )

# Delete LEI_code with "XXXXXXXXXXXXXXXXXXXX"
financial_year <- financial_year %>%
  filter(LEI_code != "XXXXXXXXXXXXXXXXXXXX") %>% 
  select(-c(NSA))


financial_year <- financial_year %>%
  mutate(
    LEI_code = trimws(LEI_code),
    Year = as.integer(Year)
  )