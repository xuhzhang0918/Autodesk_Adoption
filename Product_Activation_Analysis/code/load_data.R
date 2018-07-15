library(tidyverse)
library(lubridate)
library(stringr)

initial_load <- read_csv("data/autodesk_data.csv",col_types = list("order_number"="c",
                                                           "agreement_number"="c",
                                                           "channel_partner_csn"="c"))


#type converson functions:
convert_to_lgl <- function(x){
  #check if column is only consists of "Y","N" and "NA"
  unique_values <- unique(x)[complete.cases(unique(x))]
  lenth_unique <- length(unique_values)
  if(lenth_unique == 2){
    if (all(sort(unique_values) == sort(c("Y","N")))){
      y <- ifelse(x == "Y",TRUE,
                   ifelse(x == "N",FALSE,NA))
    } else {
      y <- x
    }
  } else {
    y <- x
  }
}

#
dataset <- initial_load %>% 
  mutate_all(convert_to_lgl)

dataset <- dataset %>%
  mutate(percentage_reg_seats_activated = if_else(percentage_reg_seats_activated == 100,1,percentage_reg_seats_activated))
rm(initial_load,convert_to_lgl)

