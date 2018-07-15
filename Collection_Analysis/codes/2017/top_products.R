#Top products used
setup(desired_working_file_name = "first_week_report",
      required_packages = c("tidyverse","stringr","lubridate","xlsx","forcats","RColorBrewer"))


#In each collection, which product are most popular? (Include version)
plot_data <- dataset %>%
    filter(contract_status == "Active",!is.na(product_name),dt == "2017-10-21") %>%
    group_by(entitlement_id,owner_id) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    group_by(feature_name,product_line_name) %>%
    summarise(total_users = sum(purchased_seat_quantity)) %>%
    ungroup() %>%
    group_by(feature_name) %>%
    mutate(rankings = min_rank(desc(total_users))) %>%
    arrange(feature_name,rankings);plot_data

write_csv(plot_data,"slide9_data.csv")

collection_names <- as.character(unique(plot_data$feature_name))
for(i in 1 :length(collection_names)) {
    current_plot <- plot_data %>%
        filter(!is.na(product_name),
               feature_name == collection_names[i]) %>%
        ggplot() +
        geom_col(mapping = aes(x = fct_reorder(product_name,rankings), y = total_users)) +
        geom_text(mapping = aes(x = Inf, y = Inf, label = str_c("number of missing product name is ",
                                                                plot_data %>%
                                                                    filter(is.na(product_name),
                                                                           feature_name == collection_names[i]) %>%
                                                                    pull(total_users))),vjust = "top", hjust = "right") +
        coord_flip() +
        labs(x = "Product Name", title = collection_names[i]) +
        theme_light()
    ggsave(filename = str_c("total user ",collection_names[i],".jpeg"),path = "slide_page_5")
}

#In each collection, which product are most popular? (Do not include version)
#In each collection, which product are most popular? (Include version)
plot_data <- dataset %>%
    filter(contract_status == "Active") %>%
    group_by(entitlement_id,owner_id) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    group_by(feature_name,product_line_name) %>%
    summarise(total_users = sum(purchased_seat_quantity)) %>%
    ungroup() %>%
    group_by(feature_name) %>%
    mutate(rankings = min_rank(desc(total_users))) %>%
    arrange(feature_name,rankings);plot_data

collection_names <- as.character(unique(plot_data$feature_name))
for(i in 1 :length(collection_names)) {
    current_plot <- plot_data %>%
        filter(!is.na(product_line_name),
               feature_name == collection_names[i]) %>%
        ggplot() +
        geom_col(mapping = aes(x = fct_reorder(product_line_name,rankings), y = total_users)) +
        geom_text(mapping = aes(x = Inf, y = Inf, label = str_c("number of missing product line name is ",
                                                                plot_data %>%
                                                                    filter(is.na(product_line_name),
                                                                           feature_name == collection_names[i]) %>%
                                                                    pull(total_users))),vjust = "top", hjust = "right") +
        coord_flip() +
        labs(x = "Product Line Name", title = collection_names[i]) +
        theme_light()
    ggsave(filename = str_c("total product line user ",collection_names[i],".jpeg"),path = "slide_page_5")
}

