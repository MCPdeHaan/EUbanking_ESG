library(tidyverse); library(janitor)

financial <- read_csv("data/Historical KM.csv") 

# Pivot data to one row per bank per quarter with each label as separate column
financial_wide <- financial %>%
  pivot_wider(
    names_from = Label,
    values_from = Amount
  )

# Aggregate quarterly data into annual totals for each bank by
financial_annual <- financial_wide %>%
  clean_names() %>%
  mutate(year = substr(period, 1, 4)) %>% 
  mutate(year = as.integer(year)) %>%
  group_by(lei_code, year) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    country = first(country),
    name = first(name),
    .groups = "drop"
  )

financial_annual <- financial_annual %>%
  filter(!str_starts(year, "2019") & !str_starts(year, "2024")) %>%
  filter(lei_code != "XXXXXXXXXXXXXXXXXXXX") %>%
  select(
    -period,
    -item,
    -item_hist,
    -perf_status,
    -common_equity_tier_1_cet1_capital_transitional_period_as_if_ifrs_9_or_analogous_ec_ls_transitional_arrangements_had_not_been_applied,
    -common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition_as_if_ifrs_9_or_analogous_ec_ls_transitional_arrangements_had_not_been_applied,
    -credit_risk_excluding_ccr_and_securitisations,
    -financial_assets_at_fair_value_through_other_comprehensive_income,
    -financial_assets_designated_at_fair_value_through_profit_or_loss,
    -financial_liabilities_designated_at_fair_value_through_profit_or_loss,
    -gross_carrying_amount_on_loans_and_advances_including_at_amortised_cost_and_fair_value,
    -leverage_ratio_total_exposure_measure_using_a_transitional_definition_of_tier_1_capital,
    -liabilities_included_in_disposal_groups_classified_as_held_for_sale,
    -non_trading_financial_assets_mandatorily_at_fair_value_through_profit_or_loss,
    -non_trading_non_derivative_financial_liabilities_measured_at_a_cost_based_method1,
    -other_assets,
    -other_liabilities,
    -other_risk_exposure_amounts,
    -position_foreign_exchange_and_commodities_risks_market_risk,
    -tier_1_as_a_percentage_of_risk_exposure_amount_as_if_ifrs_9_or_analogous_ec_ls_transitional_arrangements_had_not_been_applied,
    -tier_1_capital_as_if_ifrs_9_or_analogous_ec_ls_transitional_arrangements_had_not_been_applied_transitional_definition,
    -total_risk_exposure_amount_as_if_ifrs_9_or_analogous_ec_ls_transitional_arrangements_had_not_been_applied,
    -total_capital_transitional_period_as_if_ifrs_9_or_analogous_ec_ls_transitional_arrangements_had_not_been_applied,
    -total_capital_as_a_percentage_of_risk_exposure_amount_as_if_ifrs_9_or_analogous_ec_ls_transitional_arrangements_had_not_been_applied,
    -tier_1_capital_transitional_definition_numerator_of_leverage_ratio
  ) %>%
  select(lei_code, year, country, name, everything()) %>%
  rename(
    cash_balance = cash_cash_balances_at_central_banks_and_other_demand_deposits,
    cet1 = common_equity_tier_1_cet1_capital_transitional_period,
    credit_risk = counterparty_credit_risk_ccr_excluding_cva,
    credit_valuation = credit_valuation_adjustment_cva,
    fair_value = fair_value_changes_of_the_hedged_items_in_portfolio_hedge_of_interest_rate_risk,
    financial_liabilities_at_amortised_cost = financial_liabilities_measured_at_amortised_cost,
    gross_carrying_amount = gross_carrying_amount_on_cash_balances_at_central_banks_and_other_demand_deposits,
    haircuts = haircuts_for_trading_liabilities_at_fair_value1,
    securitisation_exposures = securitisation_exposures_in_the_banking_book_after_the_cap,
    cet1_risk_exposure = common_equity_tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
    tier1_risk_exposure = tier_1_as_a_percentage_of_risk_exposure_amount_transitional_definition,
    totalcap_risk_exposure = total_capital_as_a_percentage_of_risk_exposure_amount_transitional_definition,
    leverage_ratio = leverage_ratio_using_a_transitional_definition_of_tier_1_capital
  ) %>%
  mutate(
    # Financial ratios
    equity_to_assets = total_equity / total_assets,
    loan_to_assets = gross_carrying_amount / total_assets,
    provisions_ratio = provisions / gross_carrying_amount,
    liquidity_ratio = cash_balance / total_assets,
    # Log transformation of assets
    log_assets = log(total_assets),
    # Risk measures
    loan_quality = gross_carrying_amount / provisions,
    rwa_ratio = total_risk_exposure_amount / total_assets
  )