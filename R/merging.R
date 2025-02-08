dataset <- merge(
  ESG_tidy, 
  EU_trans_ex_wide, 
  by.x = c("Legal Entity ID (LEI)", "year"), 
  by.y = c("LEI_code", "Year"),
  all = TRUE  # Use all=TRUE for a full (outer) join; use all.x=TRUE for a left join, etc.
)

dataset <- dataset %>%
  mutate(
    # Clean company names for comparison
    Name_clean             = tolower(trimws(Name)),
    Company_Common_clean   = tolower(trimws(`Company Common Name`)),
    # Clean country names for comparison
    Country_clean          = tolower(trimws(Country)),
    Headquarters_clean     = tolower(trimws(`Country of Headquarters`))
  )


# Create new columns with the desired logic
dataset <- dataset %>%
  mutate(
    Company_Name = if_else(
      Name_clean == Company_Common_clean,
      Name,  # They match – use one of them
      paste(Name, `Company Common Name`, sep = " / ")  # They differ – combine them
    ),
    Country_Name = if_else(
      Country_clean == Headquarters_clean,
      Country,  # They match
      paste(Country, `Country of Headquarters`, sep = " / ")  # They differ
    )
  ) %>%
  # Optionally, drop the intermediate clean columns
  select(-Name_clean, -Company_Common_clean, -Country_clean, -Headquarters_clean)

# Now merged_data has the new Company_Name and Country_Name columns
