library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(RColorBrewer)
Sys.setlocale("LC_ALL","English")
#----------------------------
all_versions <- dataset %>% 
    filter(!is.na(product_name)) %>%
    group_by(product_line_name) %>%
    summarise(products_list = list(unique(product_name)),
              products = map_chr(products_list,str_c,collapse = ","),
              num_versions = map_dbl(products_list,length),
              version_changed = if_else(num_versions >1,TRUE,FALSE)) %>%
    left_join(product_popularity) %>%
    arrange(desc(version_changed),rank);all_versions

versions_display <- all_versions %>% select(-products_list,total_sessions);versions_display

#--------------------------------
focus_product  <- all_versions %>%
    filter(rank <= 17) %>%
    pull(product_line_name) %>%
    as.character();focus_product
#--------------------------------
focus_dataset <- dataset %>%
    filter(product_line_name %in% focus_product & !is.na(product_name))
#----------------------------
#Determine product popularity
product_weekly_usage <- dataset %>%
    filter(!is.na(product_line_name)) %>%
    group_by(owner_id,product_line_name,dt,product_line_code) %>%
    summarise(weekly_seats = sum(purchased_seat_quantity),
              weekly_sessions = sum(sessions)) %>%
    ungroup();product_weekly_usage


observed_date <- ymd("2017-10-01")
data_after_2017_Jan <- product_weekly_usage %>%
    filter(dt > observed_date)

product_usage <- data_after_2017_Jan %>% 
    group_by(owner_id,product_line_name,product_line_code) %>%
    summarise(mean_seats = mean(weekly_seats),
              sum_sessions = sum(weekly_sessions)) %>%
    mutate(sessions_seats = sum_sessions/mean_seats) %>%
    ungroup();product_usage


product_popularity <- product_usage %>%
    group_by(product_line_code,product_line_name) %>%
    summarise(total_sessions = sum(sum_sessions)) %>%
    arrange(desc(total_sessions)) %>%
    mutate(rank = seq(1,n())) %>%
    arrange(product_line_code,rank) %>%
    ungroup();product_popularity

#Redo sessions/seats ratio
#Weekly data on company level
spTemp <- focus_dataset %>%
    group_by(product_line_code,owner_id,entitlement_id,product_line_name,product_name,dt) %>%
    summarise(product_seats_entitlement_level = max(purchased_seat_quantity),
              product_sessions_entitlement_level = sum(sessions)) %>%
    ungroup() %>%
    group_by(product_line_code,owner_id,product_line_name,product_name,dt) %>%
    summarise(prodcut_seats_company_level = sum(product_seats_entitlement_level),
              product_sessions_company_level = sum(product_sessions_entitlement_level)) %>%
    mutate(sess_seat_ratio = product_sessions_company_level / prodcut_seats_company_level) %>%
    ungroup();spTemp


# #-----------------------------------------------
# #Weekly data on company level
# grpEnt <- focus_dataset %>%
#     group_by(product_line_code,owner_id,entitlement_id,product_line_name,product_name,dt) %>%
#     summarise(product_seats_entitlement_level = max(purchased_seat_quantity),
#               product_sessions_entitlement_level = sum(sessions)) %>%
#     ungroup()
# 
# grpComp = grpEnt %>%
#     group_by(product_line_code,owner_id,product_line_name,product_name,dt) %>%
#     summarise(prodcut_seats_company_level = sum(product_seats_entitlement_level),
#               product_sessions_company_level = sum(product_sessions_entitlement_level)) %>%
#     mutate(sess_seat_ratio = product_sessions_company_level / prodcut_seats_company_level) %>%
#     ungroup();grpComp
# 
# 
# #------------------------------------------------
focus_dataset <- dataset %>%
    filter((product_line_name %in% focus_product) & (!is.na(product_name))) 

#Weekly seats, sessions and sessions/seats ratio
weekly_aggregate_data <- spTemp %>%
    group_by(product_line_code,product_line_name,product_name,dt) %>%
    summarise(agg_weekly_seats = sum(prodcut_seats_company_level),
              agg_weekly_sessions = sum(product_sessions_company_level),
              agg_weekly_sess_seats_ratio = agg_weekly_sessions/agg_weekly_seats) %>%
    ungroup();weekly_aggregate_data

plot_data <- weekly_aggregate_data %>%
    left_join(product_popularity,by = c("product_line_code","product_line_name")) %>%
    select(-total_sessions) %>%
    #only choose top three product from each collection
    filter(rank <= 3);plot_data

#sessions, product_line_name level
sessions_dt <- plot_data %>%
    group_by(product_line_code,product_line_name,dt) %>%
    summarise(seats = sum(agg_weekly_seats),
              sessions = sum(agg_weekly_sessions),
              sessions_seats_ratio = sessions/seats) %>%
    ungroup();sessions_dt

plot_top3_products <- function(product_line_code,data,plot_column){
    data %>%
        ggplot(aes_string(x = "dt", y = plot_column,color = "product_line_name")) +
        geom_line() +
        geom_point() +
        theme_light() +
        labs(title = product_line_code) +
        scale_x_date(breaks = date_breaks("months"), labels = date_format("%b%y")) +
        theme(legend.position = "top") +
        guides(colour = guide_legend(nrow = 1))
}

table_plots <- sessions_dt %>% 
    group_by(product_line_code) %>% 
    nest() %>%
    mutate(plot_seats = pmap(list(as.character(product_line_code),data,rep("seats",3)),plot_top3_products),
           plot_sessions = pmap(list(as.character(product_line_code),data,rep("sessions",3)),plot_top3_products));table_plots

walk2(as.character(table_plots$product_line_code),table_plots$plot_seats,
      ~ ggsave(str_c("plots/",.x,"_top3_seats.png"),plot = .y,units = "cm",width = 20, height = 20 * 0.618))
walk2(as.character(table_plots$product_line_code),table_plots$plot_sessions,
      ~ ggsave(str_c("plots/",.x,"_top3_sessions.png"),plot = .y,units = "cm",width = 20, height = 20 * 0.618))

#Calculate ratio for different versions
plot_data_version_comp <- weekly_aggregate_data %>%
    left_join(product_popularity,by = c("product_line_code","product_line_name")) %>%
    select(-total_sessions) %>%
    #only choose top three product from each collection
    filter(rank <= 3) %>%
    group_by(product_line_code,product_line_name,product_name,dt) %>%
    summarise(seats = sum(agg_weekly_seats),
              sessions = sum(agg_weekly_sessions)) %>%
    ungroup() %>%
    group_by(product_line_code,product_line_name,dt) %>%
    mutate(seats_ratio = seats/sum(seats),
           sessions_ratio = sessions/sum(sessions),
           seesions_per_seat = sessions/seats) %>%
    ungroup();plot_data_version_comp

make_plot_version_ratio <- function(product_line_code,data,column_name){
    data %>% 
        ggplot(aes_string(x = "dt",y = column_name,fill = "product_name")) +
        geom_area(position = "stack",alpha = 0.7) +
        labs(title = product_line_code) +
        scale_x_date(breaks = date_breaks("months"), labels = date_format("%b%y")) +
        facet_grid(product_line_name ~ .) +
        theme_light() +
        theme(legend.position = "top") +
        guides(colour = guide_legend(nrow = 1))
}

make_plot_version_takeover <- function(product_line_code,data){
    data %>% 
        ggplot(aes(x = dt,y = seesions_per_seat,color = product_name)) +
        geom_point() +
        geom_line() +
        labs(title = product_line_code) +
        scale_x_date(breaks = date_breaks("months"), labels = date_format("%b%y")) +
        facet_grid(product_line_name ~ .) +
        theme_light() +
        scale_color_brewer(palette = "Dark2") +
        theme(legend.position = "top") +
        guides(colour = guide_legend(nrow = 1))
}

plot_version_ratio <- plot_data_version_comp %>% 
    group_by(product_line_code) %>%
    nest() %>%
    mutate(plot_seats_ratio = pmap(list(as.character(product_line_code),data,rep("seats_ratio",3)),make_plot_version_ratio),
           plot_sessions_ratio = pmap(list(as.character(product_line_code),data,rep("sessions_ratio",3)),make_plot_version_ratio),
           plot_takeover = map2(as.character(product_line_code),data,make_plot_version_takeover));plot_version_ratio

walk2(as.character(plot_version_ratio$product_line_code),plot_version_ratio$plot_seats_ratio,
      ~ ggsave(str_c("plots/",.x,"_top3_plot_seats_ratio.png"),plot = .y,units = "cm",width = 20, height = 20 * 0.618))
walk2(as.character(plot_version_ratio$product_line_code),plot_version_ratio$plot_sessions_ratio,
      ~ ggsave(str_c("plots/",.x,"_top3_plot_sessions_ratio.png"),plot = .y,units = "cm",width = 20, height = 20 * 0.618))
walk2(as.character(plot_version_ratio$product_line_code),plot_version_ratio$plot_takeover,
      ~ ggsave(str_c("plots/",.x,"_top3_plot_takeover.png"),plot = .y,units = "cm",width = 20, height = 20 * 0.618))


#Monthly data on company level(quantitles)
#Aggregate montly
monthly_aggregate_data <- spTemp %>% 
    mutate(year_month = ymd(str_c(year(dt),"-",month(dt),"-",01))) %>%
    group_by(product_line_code,owner_id,product_line_name,year_month) %>%
    summarise(monthly_seats = mean(prodcut_seats_company_level),
              monthly_session = sum(product_sessions_company_level)) %>%
    ungroup() %>%
    left_join(product_popularity,by = c("product_line_code","product_line_name")) %>%
    select(-total_sessions) %>%
    #only choose top three product from each collection
    filter(rank <= 3);monthly_aggregate_data

quantitles <- monthly_aggregate_data %>%
    filter(year_month == "2017-09-01") %>%
    group_by(product_line_code,product_line_name) %>%
    summarise(product_quantiles = list(as_tibble(as.list(round(quantile(monthly_session/monthly_seats,probs = seq(0,1,0.1))))))) %>%
    unnest() %>%
    left_join(product_popularity) %>%
    arrange(product_line_code,rank) %>%
    select(-total_sessions);quantitles

write_csv(quantitles,path = "charts/quantitles_top3_products.csv")

#
product_popularity_all_coll <- 
    product_popularity %>%
    group_by(product_line_name) %>%
    summarise(all_sessions = sum(total_sessions)) %>%
    mutate(rank = min_rank(desc(all_sessions))) %>%
    arrange(rank);product_popularity_all_coll



