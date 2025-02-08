library(readr); library(tidyverse)

ESG <- read_csv("data/ESG_Refinitiv_2.csv")
# Replace all "NULL" values with NA
ESG <- ESG %>% mutate(across(everything(), ~ na_if(.x, "NULL")))

# Replace the second row values
ESG[1, ] <- ESG[1, ] %>% mutate(across(everything(), ~ case_when(
  . == "FY2023" ~ "2023",
  . == "FY2022" ~ "2022",
  . == "FY2021" ~ "2021",
  . == "FY2020" ~ "2020",
  TRUE ~ .
)))

write_csv(ESG, "data/ESG.csv")

esg_long <- ESG %>%
  pivot_longer(
    cols = -c(Code, `Company Common Name`, `GICS Sub-Industry Name`, 
              `ICB Sector name`, `Country of Headquarters`, ISIN, 
              `Legal Entity ID (LEI)`),
    names_to = c(".value", "Year"),
    names_pattern = "([A-Za-z_]+)(\\d+)"
  ) %>%
  mutate(Year = as.numeric(Year))  # Convert the extracted year to numeric

