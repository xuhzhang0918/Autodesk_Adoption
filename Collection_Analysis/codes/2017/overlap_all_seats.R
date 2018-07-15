#overlap_all_seats
#delete filter
all_dates <- unique(dataset$dt)
all_product_line_names <- as.character(na.omit(unique(dataset$product_line_name)))
all_overlap_seats <- tibble()
for(i_date in 1 : length(all_dates)){
    for(set_product_line_name in all_product_line_names){
        overlap_devices <- dataset %>%
            filter(!is.na(product_line_name),dt == all_dates[i_date]) %>%
            filter(contract_status == "Active") %>%
            filter(product_line_name == set_product_line_name) %>%
            group_by(user_device_composite_id,product_line_code) %>%
            summarise(total_seats = max(purchased_seat_quantity)) %>%
            group_by(user_device_composite_id) %>%
            #filter(n() >= 2) %>%
            ungroup()
        
        n_overlap_devices <- length(unique(overlap_devices$user_device_composite_id))
        
        
        overlap_seats <- overlap_devices %>%
            group_by(product_line_code) %>%
            summarise(total_seats = sum(total_seats))
        
        
        before_add_row <- overlap_seats
        add_which_row <- which(!(collection_names %in% overlap_seats$product_line_code))
        if(length(add_which_row) >0) {
            for(row in collection_names[add_which_row]){
                before_add_row <- add_row(before_add_row,product_line_code = factor(row,levels = collection_names),total_seats = 0)
            }
        } 
        
        after_add_row <- before_add_row %>% arrange() %>% spread(product_line_code,total_seats)
        complete_row <- bind_cols(tibble(product_line_name = set_product_line_name),
                                  tibble(date = all_dates[i_date]),
                                  tibble(all_devices = n_overlap_devices),
                                  after_add_row)
        complete_row <- rename(complete_row,all_AECCOL_seats = AECCOL,all_MECOLL_seats = MECOLL,all_PDCOLL_seats = PDCOLL)
        all_overlap_seats <- bind_rows(all_overlap_seats,complete_row)
        
    }
}


result_all_seats <- all_overlap_seats %>%
    filter(all_devices > 0) %>%
    arrange(product_line_name,date)