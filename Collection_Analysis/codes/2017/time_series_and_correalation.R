#----------------------------------------------
#seats and sessions, but not time series
seats_and_sessions <- dataset %>%
    filter(product_line_code == "AECCOL") %>%
    group_by(owner_id,dt) %>%
    summarise(company_seats = max(purchased_seat_quantity),company_sessions = sum(sessions))

seats_and_sessions %>%
    ggplot(mapping = aes(x = company_seats,y = company_sessions)) +
    geom_point() +
    coord_cartesian(xlim = c(0,250), ylim = c(0,1000)) +
    geom_smooth(method = "lm")

cor(seats_and_sessions$company_seats,seats_and_sessions$company_sessions,use = "complete.obs")

#-----------------------------------------------
#Time series analysis
#How users use ACD
all_data <- list()
graph_list <- list()
#
all_product_names <- unique(dataset$product_line_name)[-1]
dir.create("time_series_sessions")

for (i in 1 : length(all_product_names)) {
    all_data[[i]] <- dataset %>% 
        filter(product_line_name == all_product_names[i]) %>%
        group_by(dt) %>%
        summarise(sessions = sum(sessions))
    
    graph_list[i] <- all_data[[i]] %>%
        ggplot(mapping = aes(x = dt,y = sessions)) +
        geom_line()+
        geom_point() +
        scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y")) +
        labs(title = all_product_names[i])
    ggsave(str_c("time_series_sessions/","plot",i,".png"),width = 30,height = 30*0.618,units = "cm")
}


