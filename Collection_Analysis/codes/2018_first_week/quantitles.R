load("dataset.RData")
library(tidyverse)
library(lubridate)
library(forcats)

total_seats_per_company <- dataset %>%
    filter(contract_status == "Active") %>%
    distinct(owner_id,entitlement_id,.keep_all = TRUE) %>%
    group_by(owner_id) %>%
    mutate(total_seats = sum(purchased_seat_quantity)) %>%
    select(owner_id,entitlement_id,total_seats,purchased_seat_quantity);total_seats_per_company


product_weekly_usage <- dataset %>%
    filter(!is.na(product_line_name)) %>%
    group_by(owner_id,product_line_name,dt) %>%
    summarise(weekly_seats = sum(purchased_seat_quantity),
              weekly_sessions = sum(sessions)) %>%
    ungroup();product_weekly_usage


observed_date <- ymd("2017-07-01")
data_after_2017_Jan <- product_weekly_usage %>%
    filter(dt > observed_date)

product_usage <- data_after_2017_Jan %>% 
    group_by(owner_id,product_line_name) %>%
    summarise(mean_seats = mean(weekly_seats),
              sum_sessions = sum(weekly_sessions)) %>%
    mutate(sessions_seats = sum_sessions/mean_seats);product_usage

num_months <- interval(observed_date,max(dataset$dt))/ months(1);num_months

product_popularity <- product_usage %>%
    group_by(product_line_name) %>%
    summarise(total_sessions = sum(sum_sessions)) %>%
    arrange(desc(total_sessions)) %>%
    mutate(rank = seq(1,n()));product_popularity

#quantiles
product_quantiles <- product_usage %>%
    group_by(product_line_name) %>%
    summarise(product_quantiles = list(as_tibble(as.list(round(quantile(sessions_seats/num_months,probs = seq(0,1,0.05)),2))))) %>%
    unnest() %>%
    left_join(product_popularity) %>%
    arrange(rank) %>%
    rename(sessions_9month = total_sessions);product_quantiles

#log quantiles
product_quantiles_log <- product_usage %>%
    group_by(product_line_name) %>%
    summarise(product_quantiles = list(as_tibble(as.list(round(quantile(log10(sessions_seats/num_months),probs = seq(0,1,0.05)),2))))) %>%
    unnest() %>%
    left_join(product_popularity) %>%
    arrange(rank) %>%
    rename(sessions_9month = total_sessions);product_quantiles_log

#violine plot
product_usage %>%
    #filter(product_line_name == "3DSMAX") %>%
    ggplot(mapping = aes(x =factor(product_line_name,levels = rev(levels(as_factor(as.character(product_popularity$product_line_name))))), y = sessions_seats/num_months)) +
    geom_violin(scale = "area") +
    coord_flip() +
    labs(x = "product") 

#log scale histogram
product_usage %>%
    #filter(product_line_name == "3DSMAX") %>%
    ggplot(mapping = aes(x = log10(sessions_seats/num_months))) +
    geom_histogram() +
    facet_wrap(~factor(product_line_name,levels = levels(as_factor(as.character(product_popularity$product_line_name)))),scales="free_y") +
    labs(x ="log10 of monthly sessions per seat")

#log scale violine plot
product_usage %>%
    #filter(product_line_name == "3DSMAX") %>%
    ggplot(mapping = aes(x =factor(product_line_name,levels = rev(levels(as_factor(as.character(product_popularity$product_line_name))))), y = log10(sessions_seats/num_months))) +
    geom_violin(scale = "area") +
    coord_flip() +
    geom_hline(yintercept =  -0.56, color = "red") +
    geom_text(mapping = aes(x = x,y = y,label = "*"),data = tibble(x = c("ACD","RVT","INVPROSA","CIV3D","AMECH_PP","MAYA","INVNTOR"),y = -3.5),color = "red") +
    labs(x = "product", y = "log10 of monthly sessions per seat") 
