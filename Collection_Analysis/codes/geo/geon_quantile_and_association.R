#quantile by geo
library(tidyverse)
library(lubridate)
load("data/dataset.RData")
country_mapping <- read_csv("data/country_mapping.csv")
country_mapping_reduce <- country_mapping %>% select(`alpha-2`,larger_region)
#Choose last 1 month for calculation
#between  2017-10-07 to 2017-10-21
dataset_joined_region <- dataset %>%
    filter(contract_status == "Active",!is.na(product_name)) %>%
    left_join(country_mapping_reduce, by = c("iso_country_alpha2_code" = "alpha-2")) %>%
    mutate(larger_region = as.factor(larger_region))


#calculate quantile
product_montly_usage <- dataset_joined_region %>%
    filter(dt >= "2017-09-30") %>%
    group_by(user_device_composite_id,product_line_code,product_line_name,dt) %>% # features for grouping
    group_by(larger_region = first(larger_region),
             ) %>%
    summarise(weekly_seats = sum(purchased_seat_quantity),
              weekly_sessions = sum(sessions)) %>%
    ungroup()