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



# Identify columns to keep fixed (these should not be reshaped)
fixed_columns <- c("Code", "Company Common Name", "GICS Sub-Industry Name", 
                   "ICB Sector name", "Country of Headquarters", "ISIN", 
                   "Legal Entity ID (LEI)")

# Identify the year columns to pivot (those that are year-suffixed)
year_columns <- grep("_20|_21|_22|_23", names(ESG), value = TRUE)

# Pivot data from wide to long format
ESG_long <- ESG %>%
  pivot_longer(cols = all_of(year_columns), 
               names_to = c("Metric", "Year"), 
               names_sep = "_", 
               values_to = "Value") %>%
  mutate(Year = case_when(
    Year == "2023" ~ "2023",
    Year == "2022" ~ "2022",
    Year == "2021" ~ "2021",
    Year == "2020" ~ "2020",
    TRUE ~ Year
  ))

