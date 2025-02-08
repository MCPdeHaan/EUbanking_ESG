library(readr); library(tidyverse); library(tidyr)

ESG_2 <- read_csv("data/ESG_2.csv")
# Replace all "NULL" values with NA
ESG_2 <- ESG_2 %>% mutate(across(everything(), ~ na_if(.x, "NULL")))

# Replace the second row values
ESG_2[1, ] <- ESG_2[1, ] %>% mutate(across(everything(), ~ case_when(
  . == "FY2023" ~ "2023",
  . == "FY2022" ~ "2022",
  . == "FY2021" ~ "2021",
  . == "FY2020" ~ "2020",
  TRUE ~ .
)))

ESG_2 <- ESG_2 %>%
  select(-`ICB Sector name...6`) %>%  # Remove 'ICB Sector name...6'
  rename(`ICB Sector name` = `ICB Sector name...5`)  # Rename 'ICB Sector name...5' to 'ICB Sector name'

# Extract the first row for years
years <- as.character(unlist(ESG_2[1, 9:ncol(ESG_2)]))
print(years)


head(ESG_2, 5)


# Remove the first row from the dataset
ESG_2 <- ESG_2[-1, ]




# Append the year to the respective columns
colnames(ESG_2)[9:ncol(ESG_2)] <- paste0(names(ESG_2)[9:ncol(ESG_2)], "_", as.character(unlist(years[9:ncol(ESG_2)])))

ESG_2_long <- ESG_2 %>%
  pivot_longer(
    cols = starts_with("ESG") | starts_with("Social") | starts_with("Governance") | 
      starts_with("Environmental") | starts_with("Resource") | 
      starts_with("Emissions") | starts_with("Workforce") | 
      starts_with("Human Rights") | starts_with("Community") | starts_with("Product") | 
      starts_with("Management") | starts_with("Shareholders") | starts_with("CSR"),
    names_to = c("Metric", "Year"),
    names_pattern = "^(.*)_(\\d{4})$",
    values_drop_na = TRUE
  ) %>%
  # Add an ID column to preserve original rows and prevent aggregation
  mutate(ID = paste(`Company Common Name`, Code, Metric, Year)) 

# Check if it looks correct
glimpse(ESG_2_long)



colnames(ESG_2)

print(years)



if(any(grepl("FY202", ESG_2[1, ]))) {
  ESG_2 <- ESG_2[-1, ]
}

# Refine column names using regular expressions
names(ESG_2) <- names(ESG_2) %>%
  str_replace_all("\\.\\.\\.", " ") %>%          # Replace '...' with a space
  str_replace_all("\\.\\.\\d+", "") %>%          # Remove trailing numbers (like ...10)
  str_replace_all("\\s{2,}", " ") %>%            # Replace multiple spaces with a single space
  str_trim()                                     # Trim leading/trailing spaces

# Check the cleaned column names
colnames(ESG_2)


# Define static columns (those that should not be pivoted)
static_cols <- c("Code", "Company Common Name", "ICB Industry name", 
                 "GICS Sub-Industry Name", "ICB Sector name", 
                 "Country of Headquarters", "ISIN", "Legal Entity ID (LEI)")

# Pivoting with regex to handle varying structures
ESG_2_long <- ESG_2 %>%
  pivot_longer(
    cols = -all_of(static_cols),
    names_to = c("Metric", "Identifier"),
    names_pattern = "^(.*) (\\d+)$",  # Match any number at the end
    values_drop_na = TRUE
  )

# View the reshaped data
glimpse(ESG_2_long)
glimpse(ESG_2)




# Check unique identifiers to understand what they represent
unique(ESG_2_long$Identifier)


# View the reshaped data
glimpse(ESG_2_long)

# View the reshaped data
glimpse(ESG_2_long)


# Identify columns that are static
static_cols <- c("Code", "Company Common Name", "ICB Industry name", 
                 "GICS Sub-Industry Name", "ICB Sector name", 
                 "Country of Headquarters", "ISIN", "Legal Entity ID (LEI)")

# Pivot the data to longer format
ESG_2_long <- ESG_2 %>%
  pivot_longer(
    cols = -all_of(static_cols),  # All columns except the static ones
    names_to = c(".value", "Year"), 
    names_sep = " "  # Assuming the year is separated by space (adjust if different)
  )