#Companies 
#Define what is VSB(Very small Business),SMB(Small to Medium Business) and ENT(Enterprise)

business_size_seats <- dataset %>%
    filter(contract_status == "Active") %>%
    group_by(account_uuid) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    group_by(owner_id,feature_name) %>%
    summarise(total_seats = sum(purchased_seat_quantity)) %>% #How to define what is VSB,SMB AND ENT?
    ungroup() %>%
    #(0,2]-VSB, (10,100]-SMB, [100,Inf) - ENT
    mutate(business_size = cut(total_seats,breaks = c(0,2,100,Inf),labels = c("VSB","SMB","ENT"))) %>%
    group_by(feature_name,business_size) %>%
    summarise(total_seats = sum(total_seats)) %>%
    ungroup();business_size_seats
#Since company names are deleted, we can only infer business size with how many seats the company has.
#Howerver it's not accurate because, for example, a big company may only has a small department that needs
# the product collection.

data_for_plot <- business_size_seats %>%
    mutate(abbr_feature_name = fct_recode(feature_name,
                                          "AEC" = "Architecture Engineering Construction Collection",
                                          "PD" = "Autodesk Product Design & Manufacturing Collection",
                                          "M&E" = "Media and Entertainment Collection")) %>%
    group_by(abbr_feature_name) %>%
    mutate(percentage_seats = total_seats/sum(total_seats)) %>%
    ungroup() %>%
    #Add text for text
    mutate(x = 0, y = if_else(business_size == "VSB",(1 + (1-percentage_seats))/2,
                              if_else(business_size == "SMB",((1-lag(percentage_seats))+lead(percentage_seats))/2,
                                      (percentage_seats/2))),
           text = str_c(business_size, " ",round(percentage_seats,2)*100,"%")) %>%
    group_by(abbr_feature_name) %>%
    mutate(label = str_c("n=",sum(total_seats)));xlab_for_plot;data_for_plot



data_for_plot %>%
    ggplot() +
    geom_col(mapping = aes(x = 0, y = percentage_seats, fill = business_size),position = "stack") + 
    geom_text(mapping = aes(x = 0, y = y, label = text)) +
    facet_grid(~abbr_feature_name) +
    labs(title = "SMBs comprise the majority of Industry Collection Customers",subtitle = "How to determine business size is the real problem") +
    geom_text(mapping = aes(x = 0, y = -0.05, label = label)) +
    theme_void() +
    theme(legend.position="none") +
    scale_fill_brewer( palette="Pastel1")

