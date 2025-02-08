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

# Understand negative values -> these are fincancial value changes and hedges
EU_trans_ex_yearly %>% filter(Amount < 0)

# By Year and Risk Type:
EU_trans_ex_yearly %>%
  group_by(Year, Label) %>%
  summarise(
    Total_Amount = sum(Amount, na.rm = TRUE),
    Avg_Amount = mean(Amount, na.rm = TRUE),
    .groups = "drop"
  )

# By Bank:
EU_trans_ex_yearly %>%
  group_by(LEI_code, Name) %>%
  summarise(Total_Exposure = sum(Amount), .groups = "drop") %>%
  arrange(desc(Total_Exposure))

EU_trans_ex_yearly %>%
  count(LEI_code, Name, NSA, Country, Year, Item, Sheet) %>%
  filter(n > 1)

EU_trans_ex_wide <- EU_trans_ex_yearly %>%
  group_by(LEI_code, Name, NSA, Country, Year, Item, Sheet, Label) %>%
  summarise(Amount = sum(Amount, na.rm = TRUE)) %>%  # Ensure all duplicates are summed
  ungroup() %>%
  pivot_wider(
    names_from = Label, 
    values_from = Amount, 
    values_fill = list(Amount = 0), 
    values_fn = sum  # Ensure proper summation in the pivoted format
  )

sum(EU_trans_ex_yearly$Amount, na.rm = TRUE) == 
  sum(EU_trans_ex_wide %>% select(-c(LEI_code, Name, NSA, Country, Year, Item, Sheet)), na.rm = TRUE)


yearly_sum <- EU_trans_ex_yearly %>%
  group_by(Label) %>%
  summarise(Total_Amount_Yearly = sum(Amount, na.rm = TRUE))

wide_sum <- EU_trans_ex_wide %>%
  summarise(across(-c(LEI_code, Name, NSA, Country, Year, Item, Sheet), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Label", values_to = "Total_Amount_Wide")

mismatch <- yearly_sum %>%
  left_join(wide_sum, by = "Label") %>%
  mutate(Difference = Total_Amount_Yearly - Total_Amount_Wide) %>%
  filter(Difference != 0)

print(mismatch)

EU_trans_ex_yearly %>%
  count(LEI_code, Name, NSA, Country, Year, Item, Sheet, Label) %>%
  filter(n > 1)


EU_trans_ex_yearly %>%
  group_by(LEI_code, Year, Label) %>%  # Reduce grouping
  summarise(Amount = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Label, 
    values_from = Amount, 
    values_fill = list(Amount = 0),
    values_fn = sum
  )

all.equal(total_before, total_after)


