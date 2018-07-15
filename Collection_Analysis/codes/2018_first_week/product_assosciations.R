setwd('C:/wdir')
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

library(arules)
library(arulesViz)
require(arulesSequences)

load('modded.RData')
all_collection <- unique(dataset$product_line_code)
product_mon <- list()
all_collection

#Kaki's code------------------------------------------------
#/association rule mining for product usage pattern.ipynb
for(i in 1: length(all_collection)){
  product_mon[[i]] <- dataset%>%
    filter(product_line_code == all_collection[i])%>%
    filter(!is.na(product_line_name))%>%
    mutate(year_month = str_c(year(dt),"-",month(dt))) %>%
    group_by(owner_id,product_line_name,year_month)%>%
    summarise(use_of_product = sum(sessions))%>%
    ungroup()%>%
    group_by(owner_id, year_month)%>%
    summarise(products = list(unique(product_line_name)))%>%
    mutate(num_product = str_c("{",map_chr(products,str_c,collapse = ","),"}"),
           size = map_dbl(products,length))%>%
    ungroup()%>%
    group_by(owner_id)%>%
    mutate(eid = 1:n())  
}

aec_tran<-as(product_mon[[1]]$products, "transactions")
inspect(head(aec_tran,3))

pdc_tran<-as(product_mon[[2]]$products, "transactions")
inspect(head(pdc_tran,3))

mec_tran<-as(product_mon[[3]]$products, "transactions")
inspect(head(mec_tran,3))


frequentItems<-eclat(aec_tran, parameter = list(support = 0.01, maxlen = 15))
inspect(frequentItems)

itemFrequencyPlot(aec_tran, topN=10, support = 0.01, main="Product Combo Frequency")



frequentItems<-eclat(pdc_tran, parameter = list(support = 0.01, maxlen = 15))
inspect(frequentItems)

itemFrequencyPlot(pdc_tran, topN=10, support = 0.01, main="Product Combo Frequency")


frequentItems<-eclat(mec_tran, parameter = list(support = 0.01, maxlen = 15))
inspect(frequentItems)

itemFrequencyPlot(mec_tran, topN=10, support = 0.01, main="Product Combo Frequency")



aec_rules<-apriori(aec_tran,
                   parameter = list(support= 0.01, confidence = 0.5))

aec_rules_conf <- sort (aec_rules, by="confidence", decreasing=TRUE)
inspect(aec_rules_conf)

aec_rules_lift <- sort (aec_rules, by="lift", decreasing=TRUE)
inspect(head(aec_rules_lift,10))

pdc_rules<-apriori(pdc_tran,
                   parameter = list(support= 0.01, confidence = 0.5))

pdc_rules_conf <- sort (pdc_rules, by="confidence", decreasing=TRUE)
inspect(pdc_rules_conf)

pdc_rules_lift <- sort (pdc_rules, by="lift", decreasing=TRUE)
inspect(head(pdc_rules_lift,10))

mec_rules<-apriori(mec_tran,
                   parameter = list(support= 0.01, confidence = 0.5))
mec_rules_conf <- sort (mec_rules, by="confidence", decreasing=TRUE)
inspect(mec_rules_conf)

mec_rules_lift <- sort (mec_rules, by="lift", decreasing=TRUE)
inspect(head(mec_rules_lift,10))


#Spyros' code----------------------------


devIds = unique(dataset$user_device_composite_id)

for (id in devIds) {
  print(id)
}
