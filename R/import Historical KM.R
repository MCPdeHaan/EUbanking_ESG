library(readr); library(readxl); library(dplyr); library(lubridate); 
library(stringr); library(ggplot2); library(tidyverse)

EU_trans_ex <- read_csv("data/Historical KM.csv")

EU_trans_ex <- EU_trans_ex %>% 
  mutate(Period = as.character(Period))

EU_trans_ex_filtered <- EU_trans_ex %>%
  filter(!str_starts(Period, "2019") & !str_starts(Period, "2024"))

EU_trans_ex_yearly <- EU_trans_ex_filtered %>%
  mutate(Year = str_sub(Period, 1, 4)) %>%  
  group_by(LEI_code, Name, NSA, Country, Year, Item, Label, Sheet) %>% 
  summarise(Amount = sum(Amount, na.rm = TRUE), .groups = "drop")

EU_trans_ex_yearly <- EU_trans_ex_yearly %>%
  mutate(Year = as.numeric(Year))

# Aggregate to one row per bank-year-label
EU_trans_ex_clean <- EU_trans_ex_yearly %>%
  # Group by identifiers + Label 
  group_by(LEI_code, Name, NSA, Country, Year, Label) %>%
  summarise(Amount = sum(Amount, na.rm = TRUE), .groups = "drop")

EU_trans_ex_clean %>%
  count(LEI_code, Year, Label) %>%
  filter(n > 1)  # Should return 0 rows -> correct


EU_trans_ex_wide <- EU_trans_ex_clean %>%
  pivot_wider(
    names_from = Label,
    values_from = Amount,
    values_fill = list(Amount = 0)
  )

# FALSE, so might be mistakes in pivot wider
sum(EU_trans_ex_yearly$Amount) == sum(EU_trans_ex_wide %>% select(-c(LEI_code, Name, NSA, Country, Year)), na.rm = TRUE)

# TRUE, so tolerance-based comparison yields no mismatches
all.equal(sum(EU_trans_ex_yearly$Amount),
          sum(EU_trans_ex_wide %>% select(-c(LEI_code, Name, NSA, Country, Year)), na.rm = TRUE))

# Delete LEI_code with "XXXXXXXXXXXXXXXXXXXX"
EU_trans_ex_wide <- EU_trans_ex_wide %>%
  filter(LEI_code != "XXXXXXXXXXXXXXXXXXXX")

write_csv(EU_trans_ex_wide, "data/EU_trans_ex_wide.csv")

glimpse(EU_trans_ex_wide)
colnames(EU_trans_ex_wide)