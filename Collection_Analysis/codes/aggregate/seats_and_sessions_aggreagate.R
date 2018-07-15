#load library-----------
library(tidyverse)
#load data--------------
load("data/dataset.RData")
#aggregate on weekly level
weekly_data_company_level <- dataset %>%
    filter(!is.na(product_name)) %>%
    group_by(product_line_code,owner_id,entitlement_id,product_line_name,product_name,dt) %>%
    summarise(product_seats_entitlement_level = mean(purchased_seat_quantity),
              product_sessions_entitlement_level = sum(sessions),
              #choose column to keep use first()
              static_parent_industry_group = first(static_parent_industry_group),
              static_parent_industry_segment = first(static_parent_industry_segment),
              iso_country_alpha2_code = first(iso_country_alpha2_code)) %>%
    ungroup() %>%
    group_by(product_line_code,owner_id,product_line_name,product_name,dt) %>%
    summarise(prodcut_seats_company_level = sum(product_seats_entitlement_level),
              product_sessions_company_level = sum(product_sessions_entitlement_level),
              static_parent_industry_group = first(static_parent_industry_group),
              static_parent_industry_segment = first(static_parent_industry_segment),
              iso_country_alpha2_code = first(iso_country_alpha2_code)) %>%
    ungroup();weekly_data_company_level

write_csv(weekly_data_company_level,"data/weekly_data_company_level.csv")
