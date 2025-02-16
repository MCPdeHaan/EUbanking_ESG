library(readr); library(tidyverse); library(tidyr)

# Import dataset
ESG <- read_csv("data/ESG_Refinitiv_import.csv")

# Cleaning
# Replace all "NULL" values with NA
ESG <- ESG %>% mutate(across(everything(), ~ na_if(.x, "NULL")))

# Delete FY to make ready for further analysis
ESG[1, ] <- ESG[1, ] %>% mutate(across(everything(), ~ case_when(
  . == "FY2023" ~ "2023",
  . == "FY2022" ~ "2022",
  . == "FY2021" ~ "2021",
  . == "FY2020" ~ "2020",
  TRUE ~ .
)))

# The first 8 columns are our identifiers:
id_vars <- names(ESG)[1:8]

# The remaining columns (columns 9 to 76) are “value” columns;
# note that for each variable (for example, “ESG Score”) there are 4 columns,
# one for each year. The year for each column is stored in row 1.
# I first separate that row (which we call 'year_info') from the data.
year_info <- ESG[1, ]
dat <- ESG[-1, ]  # now 'dat' has only the actual firm data

# Next, I build a lookup table for the variable columns.
# I used the fact that the column names are like "ESG Score...10", "ESG Score...11", etc.
# so I extract the “base” variable name from before the triple-dot.
var_info <- tibble(
  col = names(dat)[9:ncol(dat)],
  # get the year from the year_info row (as a character vector)
  year = as.character(unlist(year_info[9:ncol(year_info)])),
  # extract everything before the literal "..."
  var = str_match(col, "^(.*?)\\.\\.\\.")[,2]
)

# Now, pivot the data longer:
dat_long <- dat %>%
  pivot_longer(
    cols = 9:ncol(dat),
    names_to = "col",
    values_to = "value"
  ) %>%
  left_join(var_info, by = "col")

# I now want one row per firm and year. That is, for each combination
# of the id columns and the year we want one row, with one column for each variable.
# So we pivot wider using 'var' as the column names:
ESG_tidy <- dat_long %>% 
  pivot_wider(
    id_cols = c(all_of(id_vars), year),
    names_from = var,
    values_from = value, 
    values_fn = first  # take the first value in case of duplicates
  )

# Final cleaning
ESG_tidy <- ESG_tidy %>%
  mutate(
    `Legal Entity ID (LEI)` = trimws(`Legal Entity ID (LEI)`),
    year = as.integer(year),
    # Convert all numeric columns to numeric type, excluding identifier columns
    across(c(-`Company Common Name`, -`Country of Headquarters`, -`Legal Entity ID (LEI)`, -year), 
           ~as.numeric(.x))
  ) %>% 
  select(
    -`Code`, 
    -`ICB Industry name`, 
    -`GICS Sub-Industry Name`, 
    -`ICB Sector name`, 
    -`ISIN`
  )

ESG_good <- ESG_tidy %>%
  filter(!if_all(
    .cols = -c(`Company Common Name`, `Country of Headquarters`, `Legal Entity ID (LEI)`, year),
    .fns = is.na
  ))

glimpse(ESG_tidy)
colnames(ESG_tidy)
summary(ESG_tidy)
summary(ESG_good)
