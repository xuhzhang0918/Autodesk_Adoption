data <- dataset %>%
    filter(!is.na(product_name) & dt == "2017-10-21") %>%
    group_by(entitlement_model,product_line_code,owner_id) %>%
    summarise(count = length(unique(product_name))) %>%
    ungroup() %>%
    mutate(product_owned = if_else(count == 1,1,
                                  if_else(count == 2,2,3))) %>%
    group_by(entitlement_model,product_line_code,product_owned) %>%
    summarise(count_all = n()) %>%
    mutate(total_users = sum(count_all), pct_users = count_all/total_users) %>%
    ungroup() %>%
    mutate(product_line_code = fct_recode(product_line_code,AEC = "AECCOL", "M&E" = "MECOLL", "PDM" = "PDCOLL")) %>%
    select(entitlement_model,product_line_code,product_owned,pct_users) %>%
    ungroup();data

final_results <- data %>% 
    mutate(pct_char = str_c(format(100*pct_users,digits = 0),"%"),
           product_owned = fct_recode(as.factor(product_owned),"1" = "1", "2" = "2", "3+" = "3"));final_results

plot_data <- final_results %>%
    mutate(length_y = 1) %>%
    mutate(product_owned = fct_rev(product_owned),
           y_text = if_else(product_owned == "1","1 Product",
                            if_else(product_owned == "2","2 Product","3+ Product")),
           y_position = if_else(product_owned == "1", 0.5,
                                if_else(product_owned == "2", 1.5, 2.5)));plot_data

#Plot single user
plot_data %>%
    filter(entitlement_model == "Subscription Single User") %>%
    ggplot() +
    geom_col(mapping = aes(x = product_line_code, y = length_y,fill = product_owned),position = "stack") +
    geom_text(mapping = aes(x = product_line_code, y = y_position, label = pct_char)) + 
    geom_text(mapping = aes(x = 0.5, y = y_position, label = y_text)) +
    coord_flip() +
    labs(title = "Subscription Single User") +
    #edit theme
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position="none") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_blank()) +
    theme(panel.background = element_rect(fill = "white", colour = "white"))

#Plot multi user
plot_data %>%
    filter(entitlement_model == "Subscription Multi User") %>%
    ggplot() +
    geom_col(mapping = aes(x = product_line_code, y = length_y,fill = product_owned),position = "stack") +
    geom_text(mapping = aes(x = product_line_code, y = y_position, label = pct_char)) + 
    geom_text(mapping = aes(x = 0.5, y = y_position, label = y_text)) +
    coord_flip() +
    labs(title = "Subscription Multi User") +
    #edit theme
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position="none") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_blank()) +
    theme(panel.background = element_rect(fill = "white", colour = "white"))


#replot---------------
plot_data1 <- final_results %>%
    ;plot_data1
