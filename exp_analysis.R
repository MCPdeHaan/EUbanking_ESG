# Example 1: Total risk exposure over time
EU_trans_ex_yearly %>%
  group_by(Year) %>%
  summarise(Total_Amount = sum(Amount)) %>%
  ggplot(aes(x = Year, y = Total_Amount)) +
  geom_line() +
  labs(title = "Total Risk Exposure Over Time")

# Example 2: Risk distribution by type
EU_trans_ex_yearly %>%
  group_by(Label) %>%
  summarise(Total_Amount = sum(Amount)) %>%
  ggplot(aes(x = reorder(Label, Total_Amount), y = Total_Amount)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Risk Exposure by Type")