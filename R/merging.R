dataset <- merge(
  ESG_tidy, 
  EU_trans_ex_wide, 
  by.x = c("Legal Entity ID (LEI)", "year"), 
  by.y = c("LEI_code", "Year"),
  all = TRUE  # Use all=TRUE for a full (outer) join; use all.x=TRUE for a left join, etc.
)




merged_data <- ESG_tidy %>% 
  left_join(EU_trans_ex_wide, by = c("Legal Entity ID (LEI)" = "LEI_code", "year" = "Year"))
