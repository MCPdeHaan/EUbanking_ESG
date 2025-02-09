library(readr); library(tidyverse); library(tidyr)

ESG <- read_csv("data/ESG_Refinitiv_import.csv")
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

# The first 8 columns are our identifiers:
id_vars <- names(ESG)[1:8]

# The remaining columns (columns 9 to 76) are our “value” columns;
# note that for each variable (for example, “ESG Score”) there are 4 columns,
# one for each year.  The year for each column is stored in row 1.
# We first separate that row (which we call 'year_info') from the data.
year_info <- ESG[1, ]
dat <- ESG[-1, ]  # now 'dat' has only the actual firm data

# Next, we build a lookup table for the variable columns.
# We use the fact that the column names are like "ESG Score...10", "ESG Score...11", etc.
# so we extract the “base” variable name from before the triple-dot.
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
    names_to = "col",          # we will later join this to 'var_info'
    values_to = "value"
  ) %>%
  left_join(var_info, by = "col")

# At this point, each row of dat_long has:
#   - the identifier columns (columns 1-8),
#   - the original column name (col),
#   - the value,
#   - the corresponding year (from row1), and 
#   - the “base” variable name (var)
#
# We now want one row per firm–year. That is, for each combination
# of the id columns and the year we want one row, with one column for each variable.
# So we pivot wider using 'var' as the column names:
ESG_tidy <- dat_long %>% 
  pivot_wider(
    id_cols = c(all_of(id_vars), year),
    names_from = var,
    values_from = value, 
    values_fn = first  # take the first value in case of duplicates
  )

ESG_tidy$year <- as.numeric(ESG_tidy$year)
ESG_tidy <- ESG_tidy %>% 
  select(-Code, -`ICB Industry name`, -`GICS Sub-Industry Name`, 
         -`ICB Sector name`, -ISIN)

ESG_tidy <- ESG_tidy %>%
  rename(
    Name = `Company Common Name`,
    Country = `Country of Headquarters`,
    LEI_code = `Legal Entity ID (LEI)`
  )

glimpse(ESG)
glimpse(ESG_tidy)
colnames(ESG)
colnames(ESG_tidy)

