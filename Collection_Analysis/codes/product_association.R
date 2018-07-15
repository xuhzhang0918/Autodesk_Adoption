library(lubridate)
library(scales)
Sys.setlocale("LC_TIME", "English")
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

#Find candidates-----------
same_prod_diff_version <- 
    first_appearance_product_by_device %>%
    group_by(user_device_composite_id,product_line_name) %>%
    summarise(product_together = n()) %>%
    ungroup()
    
    
candidates_for_dual <- same_prod_diff_version %>%
    filter(product_together > 1)
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


nested_table2 %>% group_by(older_version_date) %>% summarise(n = n())
