library(readr); library(tidyverse)

# import dataset and replace any "NULL" strings with NA.
ESG <- read_csv("data/ESG_Refinitiv_import.csv", show_col_types = FALSE) %>%
  mutate(across(everything(), ~ na_if(.x, "NULL"))) %>%
  { 
    # cleaning first row by removing the "FY" prefix.
    year_info <- slice(., 1) %>% 
      mutate(across(everything(), ~ case_when(
        . == "FY2023" ~ "2023",
        . == "FY2022" ~ "2022",
        . == "FY2021" ~ "2021",
        . == "FY2020" ~ "2020",
        TRUE ~ .
      )))
    
    # take the remaining rows as the actual firm data
    dat <- slice(., -1)
    
    # define the first 8 columns as identifier variables
    id_vars <- names(dat)[1:8]
    
    # I build a lookup table for the “value” columns (columns 9 onward)
    # using the cleaned year information from the header
    var_info <- tibble(
      col = names(dat)[9:ncol(dat)],
      year = as.character(unlist(year_info[9:ncol(year_info)])),
      var = str_match(names(dat)[9:ncol(dat)], "^(.*?)\\.\\.\\.")[,2]
    )
    
    # pivot the firm data longer so each measurement becomes its own row
    dat_long <- dat %>%
      pivot_longer(
        cols = 9:ncol(dat),
        names_to = "col",
        values_to = "value"
      ) %>%
      # joinlookup table to attach the correct year and variable name to each row
      left_join(var_info, by = "col")
    
    # I pivot wider so that I have one row per firm and year,
    # with each variable becoming its own column.
    dat_long %>%
      pivot_wider(
        id_cols = c(all_of(id_vars), year),
        names_from = var,
        values_from = value,
        values_fn = first  # In case of duplicate entries, take the first value.
      )
  } %>%
  rename(
    lei_code = `Legal Entity ID (LEI)`,
    country = `Country of Headquarters`,
    name = `Company Common Name`,
    year = year
  ) %>%
  mutate(
    lei_code = trimws(lei_code),
    year = as.integer(year)
  ) %>%
  # Convert all non-identifier columns to numeric
  mutate(across(-c(name, country, lei_code, year), as.numeric)) %>%
  # Remove unnecessary columns
  select(-`Code`, -`ICB Industry name`, -`GICS Sub-Industry Name`, -`ICB Sector name`, -`ISIN`)

# filter out rows where all non-identifier columns are NA.
ESG_good <- ESG %>%
  filter(!if_all(
    .cols = -c(name, country, lei_code, year),
    .fns = is.na
  ))

glimpse(ESG)
colnames(ESG)
summary(ESG)
summary(ESG_good)