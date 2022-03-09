# Load required packages.
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(forcats)
library(RcppRoll)
library(dplyr)
library(tibble)
library(bit64)
library(exploratory)

# Steps to produce noncustomers
`noncustomers` <- exploratory::read_delim_file("/Users/saryamane/Downloads/noncustomers.csv", delim = ",", quote = "\"" , col_names = TRUE , na = c('') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles"), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame()

# Steps to produce customers_2
`customers_2` <- exploratory::read_delim_file("../committed_data/customers_2.csv", delim = ",", quote = "\"" , col_names = TRUE , na = c('') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles"), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame()

# Steps to produce branching_point_1
`branching_point_1` <- exploratory::read_delim_file("/Users/saryamane/Downloads/usage_actions.csv", delim = ",", quote = "\"" , col_names = TRUE , na = c('') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles"), trim_ws = TRUE, col_types = cols(id = "i", .default="?") , progress = FALSE) %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame() %>%
  left_join(customers_2, by = c("id" = "id")) %>%
  left_join(noncustomers, by = c("id" = "id")) %>%
  mutate(IS_CUSTOMER_TS = if_else(is.na(CLOSEDATE),"N", if_else(CLOSEDATE > WHEN_TIMESTAMP, "N", "Y")), IS_CUSTOMER_FINAL = if_else(is.na(CLOSEDATE),"N", "Y"), ALEXA_RANK_final = if_else(is.na(ALEXA_RANK.x), ALEXA_RANK.y, ALEXA_RANK.x), Employee_Range_Final = if_else(is.na(EMPLOYEE_RANGE.x), EMPLOYEE_RANGE.y, EMPLOYEE_RANGE.x), Industry_final = if_else(is.na(INDUSTRY.x), INDUSTRY.y, INDUSTRY.x)) %>%
  summarize_group(group_cols = c(`id` = "id",  `IS_CUSTOMER_TS` = "IS_CUSTOMER_TS",  `IS_CUSTOMER_FINAL` = "IS_CUSTOMER_FINAL"),group_funs = c("asint",  "none",  "none"),ACTIONS_CRM_CONTACTS = mean(ACTIONS_CRM_CONTACTS, na.rm = TRUE),ACTIONS_CRM_COMPANIES = mean(ACTIONS_CRM_COMPANIES, na.rm = TRUE),ACTIONS_CRM_DEALS = mean(ACTIONS_CRM_DEALS, na.rm = TRUE),ACTIONS_EMAIL = mean(ACTIONS_EMAIL, na.rm = TRUE),USERS_CRM_CONTACTS = mean(USERS_CRM_CONTACTS, na.rm = TRUE),USERS_CRM_COMPANIES = mean(USERS_CRM_COMPANIES, na.rm = TRUE),USERS_CRM_DEALS = mean(USERS_CRM_DEALS, na.rm = TRUE),USERS_EMAIL = mean(USERS_EMAIL, na.rm = TRUE),ALEXA_RANK_final = mean(ALEXA_RANK_final, na.rm = TRUE),Employee_Range_Final = get_mode(Employee_Range_Final, na.rm = TRUE),Industry_final = get_mode(Industry_final, na.rm = TRUE))

# Steps to produce non_customers_test_dataset
`non_customers_test_dataset` <- `branching_point_1`

# Steps to produce the output
`branching_point_1` %>%
  select(IS_CUSTOMER_TS, ACTIONS_CRM_CONTACTS, ACTIONS_CRM_COMPANIES, ACTIONS_CRM_DEALS, ACTIONS_EMAIL, USERS_CRM_CONTACTS, USERS_CRM_COMPANIES, USERS_CRM_DEALS, USERS_EMAIL, ALEXA_RANK_final, Employee_Range_Final, Industry_final) %>%
  build_model(model_func = xgboost_binary, formula = IS_CUSTOMER_TS ~ ACTIONS_CRM_CONTACTS + ACTIONS_CRM_DEALS + ACTIONS_EMAIL + ALEXA_RANK_final, na.action = na.pass, output_type = "logistic", booster = "gbtree", watchlist_rate = 0.10, test_rate = 0.2, seed = 20) %>%
  prediction_binary(data = "newdata", data_frame = non_customers_test_dataset) %>%
  evaluate_binary(predicted_probability, IS_CUSTOMER_TS, threshold = "f_score")