library(tidyverse)
library(lubridate)
library(xlsx)
library(arules)
library(stringr)

load("data/dataset.RData")
country_mapping <- read_csv("data/country_mapping.csv") %>% select("alpha-2",larger_region)
#time scope last 6 month
select_time <- ymd(max(unique(dataset$dt))) - months(6)

train <- dataset %>% 
    filter(dt >= select_time,
           !is.na(product_name)) %>%
    left_join(country_mapping, by = c("iso_country_alpha2_code" = "alpha-2")) %>%
    distinct(user_device_composite_id,product_line_code,product_line_name,larger_region) %>%
    #filter out NAs
    filter(!is.na(larger_region),!is.na(product_line_name))

#overall_association_rule-------------
train1 <- train %>% distinct(user_device_composite_id,product_line_name)

temp_tr_table <- train1 %>% 
    group_by(user_device_composite_id) %>% 
    nest() %>% 
    mutate(data = map(data,~ as_tibble(t(.x)))) %>% 
    unnest()

write_csv(temp_tr_table %>% select(2:ncol(temp_tr_table)),"temp.csv",col_names = FALSE,na = "")
#read transaction
temp_tr <- read.transactions(file = "temp.csv",format = "basket",sep = ",")
#calculate rules
product.rule <- apriori(temp_tr,parameter = list(support = 0.005,confidence = 0.3, minlen = 2))

#save output1
sorted_rule <- sort(product.rule,by = "lift")
df <- as(sorted_rule,"data.frame") #output1
row.names(df) = NULL

#save output2
tr_summary <- summary(temp_tr) #output 2
item_distribution <- t(as.data.frame(tr_summary@lengths))
text1 <- t(c("number of products per device distribution:",rep("",ncol(item_distribution)-1)))
text2 <- t(c("number of products per device summary:",rep("",ncol(item_distribution)-1)))
text3 <- t(c(c("min","1st quantile","median","mean","3rd quantile","max"),rep("",ncol(item_distribution)-6)))
item_distribution_summary <- t(c(tr_summary@lengthSummary,rep("",ncol(item_distribution)-6)));item_distribution_summary
colnames(item_distribution_summary) <- colnames(item_distribution)
output2 <- rbind(text1,item_distribution,text2,text3,item_distribution_summary)
rownames(output2) = NULL

#save output3
frequency_item <- as.data.frame(round(t(t(sort(itemFrequency(temp_tr),decreasing = TRUE))),5))  
colnames(frequency_item) <- "frequency"
#write to xlsx
write.xlsx(x = df,file = "output/charts/association_rules/association_rules_all_products.xlsx",sheetName = "rules")
write.xlsx(x = output2,file = "output/charts/association_rules/association_rules_all_products.xlsx",
           sheetName = "products summary",append = TRUE,col.names = FALSE,row.names = FALSE)
write.xlsx(x = frequency_item,file = "output/charts/association_rules/association_rules_all_products.xlsx",
           sheetName = "product frequency",append = TRUE)
file.remove("temp.csv")

#rules by geo---------------
all_geos <- unique(train$larger_region)
for(geo in all_geos){
    train1 <- train %>%
        filter(larger_region == geo) %>%
        distinct(user_device_composite_id,product_line_name)
    
    temp_tr_table <- train1 %>% 
        group_by(user_device_composite_id) %>% 
        nest() %>% 
        mutate(data = map(data,~ as_tibble(t(.x)))) %>% 
        unnest()
    
    write_csv(temp_tr_table %>% select(2:ncol(temp_tr_table)),"temp.csv",col_names = FALSE,na = "")
    #read transaction
    temp_tr <- read.transactions(file = "temp.csv",format = "basket",sep = ",")
    #calculate rules
    product.rule <- apriori(temp_tr,parameter = list(support = 0.005,confidence = 0.3, minlen = 2))
    
    #save output1
    sorted_rule <- sort(product.rule,by = "lift")
    df <- as(sorted_rule,"data.frame") #output1
    row.names(df) = NULL
    
    #save output2
    tr_summary <- summary(temp_tr) #output 2
    item_distribution <- t(as.data.frame(tr_summary@lengths))
    text1 <- t(c("number of products per device distribution:",rep("",ncol(item_distribution)-1)))
    text2 <- t(c("number of products per device summary:",rep("",ncol(item_distribution)-1)))
    text3 <- t(c(c("min","1st quantile","median","mean","3rd quantile","max"),rep("",ncol(item_distribution)-6)))
    item_distribution_summary <- t(c(tr_summary@lengthSummary,rep("",ncol(item_distribution)-6)));item_distribution_summary
    colnames(item_distribution_summary) <- colnames(item_distribution)
    output2 <- rbind(text1,item_distribution,text2,text3,item_distribution_summary)
    rownames(output2) = NULL
    
    #save output3
    frequency_item <- as.data.frame(round(t(t(sort(itemFrequency(temp_tr),decreasing = TRUE))),5))  
    colnames(frequency_item) <- "frequency"
    #write to xlsx
    file_name <- str_c("output/charts/association_rules/geo/",geo,"_association_rules.xlsx")
    write.xlsx(x = df,file = file_name,sheetName = "rules")
    write.xlsx(x = output2,file = file_name,
               sheetName = "products summary",append = TRUE,col.names = FALSE,row.names = FALSE)
    write.xlsx(x = frequency_item,file = file_name,
               sheetName = "product frequency",append = TRUE)
    file.remove("temp.csv")
    print(geo)
}

#rules by collection-------------
all_collections <- unique(train$product_line_code)
for(collection in all_collections){
    train1 <- train %>%
        filter(product_line_code == collection) %>%
        distinct(user_device_composite_id,product_line_name)
    
    temp_tr_table <- train1 %>% 
        group_by(user_device_composite_id) %>% 
        nest() %>% 
        mutate(data = map(data,~ as_tibble(t(.x)))) %>% 
        unnest()
    
    write_csv(temp_tr_table %>% select(2:ncol(temp_tr_table)),"temp.csv",col_names = FALSE,na = "")
    #read transaction
    temp_tr <- read.transactions(file = "temp.csv",format = "basket",sep = ",")
    #calculate rules
    product.rule <- apriori(temp_tr,parameter = list(support = 0.005,confidence = 0.3, minlen = 2))
    
    #save output1
    sorted_rule <- sort(product.rule,by = "lift")
    df <- as(sorted_rule,"data.frame") #output1
    row.names(df) = NULL
    
    #save output2
    target_length <- 20
    tr_summary <- summary(temp_tr) #output 2
    item_distribution <- t(as.data.frame(tr_summary@lengths))
    item_distribution <- t(as.data.frame(c(tr_summary@lengths,rep("",20-ncol(item_distribution)))))
    text1 <- t(c("number of products per device distribution:",rep("",20-1)))
    text2 <- t(c("number of products per device summary:",rep("",20-1)))
    text3 <- t(c(c("min","1st quantile","median","mean","3rd quantile","max"),rep("",20-6)))
    item_distribution_summary <- t(c(tr_summary@lengthSummary,rep("",20-6)));item_distribution_summary
    colnames(item_distribution_summary) <- colnames(item_distribution)
    output2 <- rbind(text1,item_distribution,text2,text3,item_distribution_summary)
    rownames(output2) = NULL
    
    #save output3
    frequency_item <- as.data.frame(round(t(t(sort(itemFrequency(temp_tr),decreasing = TRUE))),5))  
    colnames(frequency_item) <- "frequency"
    #write to xlsx
    file_name <- str_c("output/charts/association_rules/collection/",collection,"_association_rules.xlsx")
    write.xlsx(x = df,file = file_name,sheetName = "rules")
    write.xlsx(x = output2,file = file_name,
               sheetName = "products summary",append = TRUE,col.names = FALSE,row.names = FALSE)
    write.xlsx(x = frequency_item,file = file_name,
               sheetName = "product frequency",append = TRUE)
    file.remove("temp.csv")
    print(collection)
}
#rules by geo and collection-----------------
for(geo in all_geos){
    for(collection in all_collections){
        train1 <- train %>%
            filter(product_line_code == collection,
                   larger_region == geo) %>%
            distinct(user_device_composite_id,product_line_name)
        
        temp_tr_table <- train1 %>% 
            group_by(user_device_composite_id) %>% 
            nest() %>% 
            mutate(data = map(data,~ as_tibble(t(.x)))) %>% 
            unnest()
        
        write_csv(temp_tr_table %>% select(2:ncol(temp_tr_table)),"temp.csv",col_names = FALSE,na = "")
        #read transaction
        temp_tr <- read.transactions(file = "temp.csv",format = "basket",sep = ",")
        #calculate rules
        product.rule <- apriori(temp_tr,parameter = list(support = 0.005,confidence = 0.3, minlen = 2))
        
        #save output1
        sorted_rule <- sort(product.rule,by = "lift")
        df <- as(sorted_rule,"data.frame") #output1
        row.names(df) = NULL
        
        #save output2
        target_length <- 20
        tr_summary <- summary(temp_tr) #output 2
        item_distribution <- t(as.data.frame(tr_summary@lengths))
        item_distribution <- t(as.data.frame(c(tr_summary@lengths,rep("",20-ncol(item_distribution)))))
        text1 <- t(c("number of products per device distribution:",rep("",20-1)))
        text2 <- t(c("number of products per device summary:",rep("",20-1)))
        text3 <- t(c(c("min","1st quantile","median","mean","3rd quantile","max"),rep("",20-6)))
        item_distribution_summary <- t(c(tr_summary@lengthSummary,rep("",20-6)));item_distribution_summary
        colnames(item_distribution_summary) <- colnames(item_distribution)
        output2 <- rbind(text1,item_distribution,text2,text3,item_distribution_summary)
        rownames(output2) = NULL
        
        #save output3
        frequency_item <- as.data.frame(round(t(t(sort(itemFrequency(temp_tr),decreasing = TRUE))),5))  
        colnames(frequency_item) <- "frequency"
        #write to xlsx
        file_name <- str_c("output/charts/association_rules/collection_geo_intersection/",geo,"_",collection,"_association_rules.xlsx")
        write.xlsx(x = df,file = file_name,sheetName = "rules")
        write.xlsx(x = output2,file = file_name,
                   sheetName = "products summary",append = TRUE,col.names = FALSE,row.names = FALSE)
        write.xlsx(x = frequency_item,file = file_name,
                   sheetName = "product frequency",append = TRUE)
        file.remove("temp.csv")
        print(geo)
        print(collection)
    }
}
