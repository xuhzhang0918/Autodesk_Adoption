load("data/dataset.RData")
library(arules)
#For date 2017-10-21, get device and its products
device_product_table <- dataset %>% 
    filter(!is.na(product_name),dt > "2017-09-15") %>%
    distinct(user_device_composite_id,product_line_name)

tr <- as(split(device_product_table$product_line_name, device_product_table$user_device_composite_id), "transactions")    
#save(tr,file = "data/transaction_2017-10-21.RData")
load("data/transaction_2017-10-21.RData")
summary(tr)
itemFrequencyPlot(tr,topN = 10) 

product.rule <- apriori(tr,parameter = list(support = 0.01,confidence = 0.3, minlen = 2))

summary(product.rule)

inspect(product.rule)