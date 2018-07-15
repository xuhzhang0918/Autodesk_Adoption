#basic summary
#results are adjusted

plot_data <- dataset %>%
    filter(contract_status == "Active") %>%
    group_by(entitlement_id,owner_id) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    group_by(feature_name) %>%
    summarise(total_users = sum(purchased_seat_quantity))

plot_data %>%
    ggplot() +
    geom_col(mapping = aes(x = feature_name, y = total_users)) +coord_flip()
ggsave(filename = "plot1.jpeg", path = "slide_page_5",width = 30,height = 30*0.618)

plot_data %>%
    mutate(percentage = total_users/sum(total_users)) %>%
    ggplot() +
    geom_col(mapping = aes(x = feature_name, y = percentage)) +coord_flip()
ggsave(filename = "plot2.jpeg", path = "slide_page_5",width = 30,height = 30*0.618)