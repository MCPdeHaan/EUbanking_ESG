library(readr); library(readxl); library(dplyr); library(lubridate); 
library(stringr); library(ggplot2)

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
# Check for missing values
olSums(is.na(EU_trans_ex_yearly))

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

# Example 1: Total risk exposure over time
EU_trans_ex_yearly %>%
  group_by(Year) %>%
  summarise(Total_Amount = sum(Amount)) %>%
  ggplot(aes(x = Year, y = Total_Amount)) +
  geom_line() +
  labs(title = "Total Risk Exposure Over Time")

# Example 2: Risk distribution by type
EU_trans_ex_yearly %>%
  group_by(Label) %>%
  summarise(Total_Amount = sum(Amount)) %>%
  ggplot(aes(x = reorder(Label, Total_Amount), y = Total_Amount)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Risk Exposure by Type")





