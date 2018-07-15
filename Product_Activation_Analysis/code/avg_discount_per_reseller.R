library(stringr)
library(dplyr)
#install.packages('MASS')
library(MASS)
#install.packages("aod")
library(aod)

source("code/data_cleaning.R")
select <- dplyr::select

# try calculate promotion--- 

#1. extract from prom_type--

t='NAMER GFP FY17Q3 Legacy trade in 3Y 60% off Group 1'

promo <-function(str){
  per=str_extract(string=str, pattern = "([0-9]+)%")
  res = 1-as.integer(gsub("%","", per))/100
  return(res)
}

promo_discount1<-dataset%>%
  select(promo_type)%>%
  mutate(promotion = promo(promo_type))
#2. calculate promo discount by column
promo_discount2<-dataset%>%
  select(promo_type, total_billed_usd, srp_billed_usd)%>%
  mutate(promotion = total_billed_usd/srp_billed_usd)



dataset_selected <- dataset %>% 
  dplyr::filter(agreement_status == "Active",order_date >= "2017-04-01")%>%
  mutate(promotion = round(total_billed_usd/srp_billed_usd, 2))%>%
  filter(!is.na(promotion), promotion != Inf)%>%
  left_join(reseller_size)

