library(readr); library(readxl); library(dplyr); library(lubridate); library(stringr)

EU_trans_ex <- read_csv("data/Historical KM.csv")

EU_trans_ex <- EU_trans_ex %>% 
  mutate(Period = as.character(Period))

EU_trans_ex_filtered <- EU_trans_ex %>%
  filter(!str_starts(Period, "2019") & !str_starts(Period, "2024"))

EU_trans_ex_yearly <- EU_trans_ex_filtered %>%
  mutate(Year = str_sub(Period, 1, 4)) %>%  
  group_by(LEI_code, Name, NSA, Country, Year, Item, Label, Sheet) %>% 
  summarise(Amount = sum(Amount, na.rm = TRUE), .groups = "drop")





