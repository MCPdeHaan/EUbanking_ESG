library(dplyr)

dataset <- merge(
  ESG_tidy, 
  EU_trans_ex_wide, 
  by.x = c("Legal Entity ID (LEI)", "year"), 
  by.y = c("LEI_code", "Year"),
  all = TRUE  # Full outer join to keep all rows
)

dataset <- dataset %>%
  mutate(
    `Company Common Name` = coalesce(`Company Common Name`, Name),
    `Country of Headquarters` = coalesce(`Country of Headquarters`, Country)
  )

# Ireland; Republic of and Ireland are the same country
unique(dataset$`Country of Headquarters`)

# Solution for above problem
dataset <- dataset %>%
  mutate(`Country of Headquarters` = ifelse(`Country of Headquarters` 
                                            == "Ireland; Republic of", "Ireland", 
                                            `Country of Headquarters`))


dataset <- dataset %>%
  select(-Country) %>%  # Remove the Country column
  relocate(Name, .after = `Company Common Name`)  # Move Name after Company Common Name


dataset <- dataset %>%
  select(-Name) %>%  # Drop the Name column
  rename(
    LEI = `Legal Entity ID (LEI)`, 
    Company = `Company Common Name`, 
    `Country of HQ` = `Country of Headquarters`
  ) %>%
  relocate(year, .after = `Country of HQ`)  # Move year after Country of HQ

glimpse(country_mismatches)
glimpse(name_mismatches)