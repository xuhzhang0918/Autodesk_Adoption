library(stringr)
library(dplyr)
#install.packages('MASS')
library(MASS)
#install.packages("aod")
library(aod)


select <- dplyr::select

# try calculate promotion 

t='NAMER GFP FY17Q3 Legacy trade in 3Y 60% off Group 1'

promo <-function(str){
  per=str_extract(string=str, pattern = "([0-9]+)%")
  res = 1-as.integer(gsub("%","", per))/100
  return(res)
}

promo(t)

car<-dataset%>%
  select(promo_type)%>%
  mutate(promotion = promo(promo_type))

tmp<-dataset%>%
  select(promo_type, total_billed_usd, srp_billed_usd)%>%
  mutate(promotion = total_billed_usd/srp_billed_usd)


# try stepAIC to pick variables

dataset_selected <- dataset %>% 
  dplyr::filter(agreement_status == "Active",order_date >= "2017-04-01")

#filter columns ------------
filtered <- dataset_selected %>%
  dplyr::select(-active_and_registered_seats_activated) %>%
  dplyr::select_if(~ !all(is.na(.x))) %>% #drop columns that contain only NAs
  dplyr::select_if(~ !(n_distinct(.x)==1)) %>% # drop columns that only have one distinct value
  dplyr::select(-matches("_score$"))  #remove all scores because they are normalized on time and regions


filtered<-filtered%>%
  mutate(promotion = round(total_billed_usd/srp_billed_usd, 2))%>%
  filter(!is.na(promotion), promotion != Inf)

filtered<-filtered%>%
  left_join(reseller_size)

model1 = lm(data = filtered, percentage_reg_seats_activated ~ promotion + partner_size + srp_billed_usd + order_source  + order_type + bmt_product_group+ bmt_market + bmt_channel + bmt_billing_term  + contract_term +  billing_behavior + country_code+ hier_industry_group_name+ customer_size_code + total_billed_usd + sf_subscription_seats_billed + renewal_rate + uuid_seats_on_active_subs + parent_owner_uuid_count + active_registered_seat_sum + pns_days_since_agreement_start + pns_days_used_since_agreement_start )
model1_step <- stepAIC(model1, direction = c("both"))
summary(model1_step)

filtered2<-filtered%>%
  mutate(is.order_typeRE = if_else(order_type == "RE", 1, 0),
         is.specific_country = if_else(country_code %in% c("AR", "CN", "ID", "LV", "MT", "PT", "SZ"), 1, 0),
         is.VSB = if_esle(customer_size_code == "VSB", 1,0))

model2 = lm(data = filtered2, percentage_reg_seats_activated ~is.order_typeRE + srp_billed_usd + bmt_product_group  + is.specific_country + hier_industry_group_name+ is.VSB + total_billed_usd + sf_subscription_seats_billed + renewal_rate + pns_days_since_agreement_start )
summary(model2)

model2_step <-stepAIC(model2,direction = c("backward"))
summary(model2_step)

plot(filtered2$percentage_reg_seats_activated)


model3=glm(data = filtered2, percentage_reg_seats_activated ~ is.order_typeRE  + srp_billed_usd + bmt_product_group  + is.specific_country + hier_industry_group_name+ is.VSB + sf_subscription_seats_billed + pns_days_since_agreement_start)
summary(model3)

model3_step <- stepAIC(model3)
summary(model3_step)


anova(model3, test="Chisq")
