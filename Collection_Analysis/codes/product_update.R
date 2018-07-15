setwd('C:\\Users\\sprot\\Documents\\GitHub\\yTest\\Autodesk_Adoption-yifu_test\\Jan_8')
#setwd to 2018_first_week
load("dataset.RData")
library(tidyverse)
library(lubridate)
library(forcats)
library(scales)
library(purrr)
library(RColorBrewer)
library(stringr)
Sys.setlocale("LC_ALL","English")
#----------------------------
#product popularity
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

#sessions
sessions_dt <- focus_dataset %>%
    group_by(product_line_name,product_name,dt) %>%
    summarise(sessions = sum(sessions)) %>%
    ungroup();sessions_dt

graph_list_sessions <- list()
for(i in 1 : length(focus_product)){
    graph_list_sessions[[i]] <- sessions_dt %>%
        filter(product_line_name == focus_product[i]) %>%
        ggplot(mapping = aes(x = dt,y = sessions,color = product_name)) +
        geom_line()+
        geom_point() +
        scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) +
        labs(title = focus_product[i])
}

#number of products
#update of product in terms of number of products
seats_dt <- focus_dataset %>% 
    group_by(entitlement_id,owner_id,product_line_name,product_name,dt) %>%
    summarise(product_seats = sum(purchased_seat_quantity)) %>%
    ungroup() %>%
    group_by(product_line_name,product_name,dt) %>%
    summarise(product_seats = sum(product_seats)) %>%
    ungroup()

typeof(dataset$dt)

#start spyros testing
max(focus_dataset$purchased_seat_quantity) #750

spTemp <- focus_dataset %>% 
  group_by(owner_id,product_line_name,dt)%>%
  summarise(product_seats = mean(purchased_seat_quantity),
            owner_sessions_that_week = sum(sessions)) %>%
  mutate(sess_seat_ratio = owner_sessions_that_week / product_seats)

nrow(spTemp)
sum_of_all_sessions = sum(spTemp$owner_sessions_that_week) #just testing
sum_of_all_seats = sum(spTemp$product_seats)
sum_of_all_sessions
sum_of_all_seats
sum_of_all_sessions/sum_of_all_seats

spTemp2 = spTemp %>%
  filter(product_line_name == 'CIV3D')

quantile(spTemp2$sess_seat_ratio,prob = seq(0, 1, length = 11), type = 5)

#validate manually: pick a random product_line_code, owner ID and dt
validateSeats = focus_dataset %>%
  filter(owner_id == 'C6A59A7B-EAFF-44a9-827C-E6F4F9000FCE' & as.character(dt) == '2017-08-05' & product_line_name == 'CIV3D')

testRatio = sum(validateSeats$sessions) / mean(validateSeats$purchased_seat_quantity)
testRatio  
spTempRatio = spTemp2 %>% 
  filter(owner_id == 'C6A59A7B-EAFF-44a9-827C-E6F4F9000FCE' & as.character(dt) == '2017-08-05' & product_line_name == 'CIV3D')
  
near(x = spTempRatio$sess_seat_ratio, y = testRatio , tol = .Machine$double.eps^0.5) #needs to be TRUE (otherwise mistake)
#end Spyros code


graph_list_seats <- list()
for(i in 1 : length(focus_product)){
    graph_list_seats[[i]] <- seats_dt %>%
        filter(product_line_name == focus_product[i]) %>%
        ggplot(mapping = aes(x = dt,y = product_seats,color = product_name)) +
        geom_line()+
        geom_point() +
        scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y")) +
        labs(title = focus_product[i])
}
print(graph_list_seats)
#sessions per seat
per_dt <- focus_dataset %>% 
    group_by(entitlement_id,owner_id,product_line_name,product_name,dt) %>%
    summarise(product_seats = sum(purchased_seat_quantity),
              product_sessions = sum(sessions)) %>%
    ungroup() %>%
    group_by(product_line_name,product_name,dt) %>%
    summarise(product_seats = sum(product_seats),
              product_sessions = sum(product_sessions)) %>%
    ungroup() %>%
    mutate(sessions_per_seat = product_sessions/product_seats)

graph_list_per <- list()
for(i in 1 : length(focus_product)){
    graph_list_per[[i]] <- per_dt %>%
        filter(product_line_name == focus_product[i]) %>%
        ggplot(mapping = aes(x = dt,y = sessions_per_seat,color = product_name)) +
        geom_line()+
        geom_point() +
        scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y")) +
        labs(title = focus_product[i])
}
print(graph_list_per)

#product seats ratio
seats_ratio_dt <- dataset %>% 
    group_by(entitlement_id,owner_id,product_line_name,product_name,dt) %>%
    summarise(product_seats = sum(purchased_seat_quantity)) %>%
    ungroup() %>%
    group_by(product_line_name,product_name,dt) %>%
    summarise(product_seats = sum(product_seats)) %>%
    ungroup() %>%
    group_by(product_line_name, dt) %>%
    mutate(total_seats_all_versions = sum(product_seats)) %>%
    ungroup() %>%
    mutate(percent_seats = product_seats/total_seats_all_versions)

graph_list_ratio <- list()
for(i in 1 : length(focus_product)){
    graph_list_ratio[[i]] <- seats_ratio_dt %>%
        filter(product_line_name == focus_product[i]) %>%
        ggplot(mapping = aes(x = dt,y = percent_seats,fill = product_name)) +
        geom_col(position = "stack") +
        scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y")) +
        labs(title = focus_product[i])
}
print(graph_list_ratio)

#Do people use same product but of different versions?
diff_version <- focus_dataset %>%
    distinct(account_uuid,dt,product_line_name,product_name,purchased_seat_quantity) %>%
    group_by(account_uuid,product_line_name,dt,purchased_seat_quantity) %>%
    summarise(num_versions = n()) %>%
    ungroup() %>%
    mutate(use_diff_version = if_else(num_versions > 1,TRUE,FALSE)) %>%
    group_by(product_line_name,dt) %>%
    summarise(percent_use_diff_version = sum(purchased_seat_quantity*use_diff_version)/sum(purchased_seat_quantity))
    

graph_diff_version <- list()
for(i in 1 : length(focus_product)){
    graph_diff_version[[i]] <- diff_version %>%
        filter(product_line_name == focus_product[i]) %>%
        ggplot(mapping = aes(x = dt,y = percent_use_diff_version)) +
        geom_line()+
        geom_point() +
        scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y")) +
        labs(title = focus_product[i])
}
print(graph_diff_version)

#----------------------------------------------------------------------
compared_dataset <- focus_dataset %>%
    distinct(account_uuid,dt,product_line_name,product_name,purchased_seat_quantity,product_line_code)

diff_version1 <- compared_dataset %>%
    group_by(account_uuid,product_line_name,dt,purchased_seat_quantity,product_line_code) %>%
    summarise(is_diff_same_collection = if_else(n() > 1,TRUE,FALSE)) %>%
    ungroup() %>%
    group_by(account_uuid,product_line_name,dt,purchased_seat_quantity) %>%
    summarise(is_diff_same_collection = any(is_diff_same_collection)) %>%
    ungroup() ;diff_version1
#---------
diff_version2_prep <- compared_dataset %>%
    group_by(account_uuid,product_line_name,dt,purchased_seat_quantity) %>%
    summarise(num_collections = n()) %>%
    ungroup() %>%
    filter(num_collections > 1);diff_version2_prep

diff_version2 <- diff_version2_prep %>%
    left_join(compared_dataset) %>%
    group_by(account_uuid,product_line_name,dt,purchased_seat_quantity,product_line_code) %>%
    summarise(products = list(product_name)) %>%
    spread(key = product_line_code,value = products) %>%
    ungroup() %>%
    mutate(x1 = !map_lgl(AECCOL,is_null),
           x2 = !map_lgl(MECOLL,is_null),
           x3 = !map_lgl(PDCOLL,is_null),
           count = x1 + x2 + x3) %>%
    filter(count >= 2) %>%
    mutate(is_diff_diff_collection = if_else(x1 & x2,!map2_dbl(AECCOL,MECOLL,setequal),
                              if_else(x1 & x3,!map2_dbl(AECCOL,PDCOLL,setequal),
                                      if_else(x2 & x3,!map2_dbl(MECOLL,PDCOLL,setequal),
                                              if_else(x1 & x2 & x3,any(!map2_dbl(AECCOL,MECOLL,setequal),
                                                                       !map2_dbl(AECCOL,PDCOLL,setequal),
                                                                       !map2_dbl(MECOLL,PDCOLL,setequal)),FALSE))))) %>%
    select(account_uuid,product_line_name,dt,purchased_seat_quantity,is_diff_diff_collection)
    

result <- diff_version1 %>% 
    left_join(diff_version2) %>%
    mutate(is_diff_diff_collection = if_else(is.na(is_diff_diff_collection),FALSE,is_diff_diff_collection),
           both_situation = all(is_diff_diff_collection,is_diff_same_collection),
           only_diff_same_collection = is_diff_same_collection & !is_diff_diff_collection,
           only_diff_diff_collection = !is_diff_same_collection & is_diff_diff_collection,
           not_diff = !is_diff_same_collection & !is_diff_diff_collection) %>%
    group_by(product_line_name,dt) %>%
    summarise(same_collection_difference = sum(purchased_seat_quantity*only_diff_same_collection)/sum(purchased_seat_quantity),
              diff_collection_difference = sum(purchased_seat_quantity*only_diff_diff_collection)/sum(purchased_seat_quantity),
              diff_both_situation = sum(purchased_seat_quantity*both_situation)/sum(purchased_seat_quantity),
              not_diff = sum(purchased_seat_quantity*not_diff)/sum(purchased_seat_quantity)) %>%
    gather(key = "type", value = "percent",3 : 6)
    


graph_result <- list()
for(i in 1 : length(focus_product)){
    graph_result[[i]] <- result %>%
        filter(product_line_name == focus_product[i] &
               type != "not_diff") %>%
        ggplot() +
        geom_col(mapping = aes(x = dt, y = percent,fill = type)) +
        scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y")) +
        labs(title = focus_product[i])
}
print(graph_result)



