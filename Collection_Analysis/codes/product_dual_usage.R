library(lubridate)
library(scales)
library(tidyverse)
Sys.setlocale("LC_TIME", "English")
load("data/dataset.RData")
#new device dt--------------
first_week_appearance <- dataset %>% 
    filter(!is.na(product_name)) %>%
    distinct(user_device_composite_id,dt) %>%
    group_by(user_device_composite_id) %>%
    filter(dt == min(dt)) %>%
    ungroup()
    
new_device_dt <-first_week_appearance %>%
    group_by(dt) %>%
    summarise(num_of_device = n())
write_csv(new_device_dt,"output/new_device_dt.csv")
ggplot(data = new_device_dt,aes(x = dt, y = num_of_device)) + geom_point() + geom_line() + scale_x_date(date_labels = "%b%Y",date_breaks = "month")
ggsave(filename = "output/plots/num_of_device_dt.png",units = "cm",width = 30, height = 30 * 0.6186)


#------------
first_week_appearance <- first_week_appearance %>%
    #the first 2 months of a subscriptipn, it is not safe to assume that someone is new to a product. 
    #Maybe they just took a break from it and returned. That's why we add 2 months of grace period
    mutate(add_two_months = dt + months(2));first_week_appearance

#-----------------
first_appearance_product_by_device <- dataset %>% 
    filter(!is.na(product_name)) %>%
    distinct(user_device_composite_id,product_line_name,product_name,product_release_id,dt) %>%
    group_by(user_device_composite_id,product_line_name,product_release_id,product_name) %>%
    filter(dt == min(dt)) %>%
    arrange(user_device_composite_id,dt) %>%
    ungroup()

first_appearance_product_by_device <- 
    first_appearance_product_by_device %>%
    arrange(user_device_composite_id,dt) %>%
    rename(first_usage_date = dt)
write_csv(first_appearance_product_by_device,"data/first_appearance_product_by_device.csv")
#Find candidates-----------
same_prod_diff_version <- 
    first_appearance_product_by_device %>%
    group_by(user_device_composite_id,product_line_name) %>%
    summarise(product_together = n()) %>%
    ungroup()
    
    
candidates_for_dual <- same_prod_diff_version %>%
    filter(product_together > 1)
write_csv(candidates_for_dual,path = "data/candidates_dual_short.csv")
#Join candidates table--------------------------
candidates2 <- dataset %>%
    #find candidates
    right_join(candidates_for_dual, by = c("user_device_composite_id","product_line_name")) %>%
    #join first_appearance_product_by_device
    left_join(first_appearance_product_by_device)



temp_table1 <- first_appearance_product_by_device %>%
    inner_join(candidates_for_dual)

nested_table <- temp_table1 %>% 
    group_by(user_device_composite_id,product_line_name) %>% 
    nest() 

temp_function <- function(data){
    all_versions <- data$product_release_id
    newest_version <- max(all_versions)
    for(version in all_versions){
        if(version < newest_version){
            if(data %>% filter(product_release_id == version) %>% select(first_usage_date) %>% pull() > 
               data %>% filter(product_release_id == newest_version) %>% select(first_usage_date) %>% pull()){
                flag = TRUE
                break()
            }
        } else {
            flag = FALSE
        }
    }
    return(flag)
}

nested_table2 <- nested_table %>% 
    mutate(older_version_date = map_lgl(data,temp_function))

#How many devices use older version after they install new version------------------
#devices_reverting <- nested_table2 %>% group_by(older_version_date) %>% summarise(n = n())
#write_csv(devices_reverting,path = "output/charts/devices_reverting.csv")



#how many weeks of dual usage---------------
candidates_dual_wide <- dataset %>% 
    filter(!is.na(product_name)) %>%
    inner_join(candidates_for_dual)

weeks_dual_usage <- candidates_dual_wide %>%
    group_by(user_device_composite_id,dt,product_line_name) %>%
    summarise(version_count = length(unique(product_release_id))) %>%
    ungroup() %>%
    group_by(user_device_composite_id,product_line_name) %>%
    summarise(num_of_weeks = sum(version_count >= 2)) %>%
    ungroup()

dual_usage_all <- weeks_dual_usage %>%
    group_by(num_of_weeks) %>%
    summarise(number_of_devices = n())

write_csv(dual_usage_all,path = "output/charts/dual_usage_all.csv")

product_wise_dual_usage <- weeks_dual_usage %>%
    group_by(product_line_name,num_of_weeks) %>%
    summarise(number_of_devices = n()) %>%
    ungroup()

write_csv(x = product_wise_dual_usage, path = "output/charts/product_wise_dual_usage.csv")

devices_ever_used_product <- dataset %>% 
    distinct(user_device_composite_id,product_line_name) %>%
    group_by(product_line_name) %>%
    summarise(num_of_devices = n()) 


dual_usage_weeks <- product_base_dual_usage %>%
    left_join(devices_ever_used_product)

write_csv(x = dual_usage_weeks,path = "output/charts/dual_usage_weeks.csv")

dual_usage_score <- product_base_dual_usage %>%
    left_join(devices_ever_used_product) %>%
    group_by(product_line_name) %>%
    summarise(score = sum(num_of_weeks * number_of_devices)/first(num_of_devices)) %>%
    mutate(score = round(100 * score,3)) %>%
    arrange(desc(score))

write_csv(x = dual_usage_score,path = "output/charts/dual_usage_score.csv")

#Calculate product dual usage using another sum
library(tidyverse)
table_data <- read_csv("output/charts/dual_usage_weeks.csv")

dual_usage_score2 <-
    table_data %>%
    group_by(product_line_name) %>%
    mutate(number_of_devices_with_more_version = sum(number_of_devices)) %>%
    summarise(`socre_all_devices(*100)` = round(100 *sum(num_of_weeks * number_of_devices)/first(num_of_devices),2),
              socre_dual_usage_devices = round(sum(num_of_weeks * number_of_devices)/first(number_of_devices_with_more_version),2)) %>%
    arrange(desc(`socre_all_devices(*100)`))

write_csv(x = dual_usage_score2,path = "output/charts/dual_usage_score_2_versions.csv")


#calculate number of dual usage weeks by minus weeks
# candidates_dual_short <- read_csv("data/candidates_dual_short.csv")
# candidates_dual_wide <- dataset %>% 
#     filter(!is.na(product_name)) %>%
#     inner_join(candidates_for_dual)
# 
# 
# first_appearance_product_by_device <- read_csv("data/first_appearance_product_by_device.csv")
# temp <- first_appearance_product_by_device %>% inner_join(candidates_dual_short)
# group_by(user_device_composite_id,product_line_name) %>% filter(product_release_id == max(product_release_id))
# 
# candidates_dual_wide %>%
#     group_by(user_device_composite_id,dt,product_line_name) %>%
#     summarise(version_count = length(unique(product_release_id))) %>%
#     ungroup() %>% left_join(first_appearance_product_by_device, by = c("user_device_composite_id",))
# 
# 
# 
# weeks_dual_usage <- candidates_dual_wide %>%
#     group_by(user_device_composite_id,dt,product_line_name) %>%
#     summarise(version_count = length(unique(product_release_id))) %>%
#     ungroup() %>%
#     group_by(user_device_composite_id,product_line_name) %>%
#     summarise(num_of_weeks = sum(version_count >= 2)) %>%
#     ungroup()
# 
# dual_usage_all1 <- weeks_dual_usage1 %>%
#     group_by(num_of_weeks) %>%
#     summarise(number_of_devices = n())
# dual_usage_all



