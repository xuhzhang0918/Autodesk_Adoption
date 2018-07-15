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


#calculate quantile-------------
#usage information for last month
product_montly_usage <- dataset_joined_region %>%
    filter(dt >= "2017-09-30") %>%
    group_by(user_device_composite_id,product_line_code,product_line_name) %>% # features for grouping
    summarise(monthly_sessions = sum(sessions),
              larger_region = first(larger_region)) %>%
    ungroup()

#quantile by product_line_name --------------
percentile_by_products <- product_montly_usage %>% 
    group_by(product_line_name) %>% 
    nest() %>% 
    mutate(product_quantiles = map(data,~ as.tibble(t(quantile(.x$monthly_sessions,probs = seq(0,1,0.05)))))) %>%
    select(-data) %>%
    unnest()
write_csv(x = percentile_by_products,path = "output/charts/quantiles_info/monthly_device_sessions_percentiles_by_products.csv")
#quantile by collection and product_line_name --------------
percentitle_by_collection_products <- product_montly_usage %>% 
    group_by(product_line_code,product_line_name) %>% 
    nest() %>% 
    mutate(product_quantiles = map(data,~ as.tibble(t(quantile(.x$monthly_sessions,probs = seq(0,1,0.05)))))) %>%
    select(-data) %>%
    unnest() %>%
    arrange(product_line_code)
write_csv(x = percentitle_by_collection_products,path = "output/charts/quantiles_info/monthly_device_sessions_percentitle_by_collection_products.csv")
#quantile by geo and products-------------
percentitle_geo_products <- product_montly_usage %>% 
    group_by(larger_region,product_line_name) %>% 
    nest() %>% 
    mutate(product_quantiles = map(data,~ as.tibble(t(quantile(.x$monthly_sessions,probs = seq(0,1,0.05)))))) %>%
    select(-data) %>%
    unnest() %>%
    arrange(larger_region)
write_csv(x = percentitle_geo_products,path = "output/charts/quantiles_info/monthly_device_sessions_percentitle_geo_products.csv")
#quantile by geo and collection and products--------------
percentitle_geo_collection_products <- product_montly_usage %>% 
    group_by(larger_region,product_line_code,product_line_name) %>% 
    nest() %>% 
    mutate(product_quantiles = map(data,~ as.tibble(t(quantile(.x$monthly_sessions,probs = seq(0,1,0.05)))))) %>%
    select(-data) %>%
    unnest() %>%
    arrange(larger_region,product_line_code)
write_csv(x = percentitle_geo_collection_products,path = "output/charts/quantiles_info/monthly_device_sessions_percentitle_geo_collection_products.csv")

