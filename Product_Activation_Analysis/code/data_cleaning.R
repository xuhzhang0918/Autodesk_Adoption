source("code/load_data.R")

#choose effective dates and contract type----------------
dataset_selected <- dataset %>% 
    dplyr::filter(agreement_status == "Active",order_date >= "2017-04-01")
#  select_if(~ (n_distinct(.x) < 2000) & !is.character(.x))

#drop columns based on metrics or standards-----------
dropped <- dataset_selected %>%
    select(-active_and_registered_seats_activated) %>% # -percentage_reg_seats_activated
    select_if(~ !all(is.na(.x))) %>% #drop columns that contain only NAs
    select_if(~ !(n_distinct(.x)==1)) %>% # drop columns that only have one distinct value
    select(-matches("_score$"))  %>% #drop all scores because they are normalized on time and regions
    select_if(~ !(is.character(.x) & n_distinct(.x) > 500)) %>% # drop out id-like columns
    mutate(uuid_age_days = as.integer(ymd("2018-03-01")-uuid_age)) %>% #convert uuid_age to days(change hard-coded date in future analysis)
    select_if(~!is.Date(.x)) %>% #drop  dates
    #select_if(~!is.logical(.x)) %>%# drop logical columns
    #select(-matches("_risk$")) %>% #drop all risks
    #select_if(~!(n_distinct(.x)==2 & any(is.na(.x)))) %>% #drop column with only NA and another value
    select_if(~sum(is.na(.x))/length(.x) < 0.3) %>% #drop columns have more than 10% NAs
    select(-country_name,-promo_type_code,-promo_type,
           -total_billed_net_p4p_incentives_usd,-srp_billed_ccf,
           -total_billed_ccf,-total_billed_net_p4p_ccf_incentives) %>% #dropped duplicate columns(maybe also drop promo_type)
    select(-matches("(^pns_)|(^dap)|(^dts)")) %>% #only use cars data
    select(-matches("segment")) #Don't use segment data
#select(-matches("(car)|(dap)|(pns)"))

#get distinct values for categorical(ordinal) -------
distinct_list <- map(dropped %>%
                         select_if(~is.character(.x)),~unique(.x))

#turn ordinal variable into integer------
# segment_dict <- seq(1,9) %>%
#     setNames(c("Disengaging alarming","No use","Dormant","Slowing","Stable low","Growing","Stable mid","Stable high","Highly engaged"))
health_dict <- seq(1:3) %>%
    setNames(c("Negative","Neutral","Positive"))
onboarding_dict <- seq(1:3) %>%
    setNames(c("At Risk","Monitor","Good"))
# size_dict <- seq(1:3) %>%
#     setNames(c("VSB","SMB","ENT"))
year_dict <- seq(1:3) %>%
    setNames(c("1 Year","2 Year","3 Year"))
year_dict1 <- seq(1:3) %>%
    setNames(c("1-Year","2-Year","3-Year"))
unique_fiscal <- unique(dropped$fiscal_quarter_and_year)
fiscal_dict <- seq(1:length(unique_fiscal)) %>%
    setNames(sort(unique_fiscal))
dicts <- list(health_dict,onboarding_dict,year_dict,year_dict1,fiscal_dict)
cate_2_ordi <- function(x,dict_list){
    for (dict in dict_list) {
        if(all(names(dict) %in% unique(x))){
            return(dict[x])
        }
    }
    return(x)
}

#drop again(drop columns that contain too many columns) and duplicated columns -------
transformed <- dropped %>%
    mutate_if(~is.character(.x),~cate_2_ordi(.x,dict_list = dicts)) %>%
    select_if(~sum(is.na(.x))/length(.x) < 0.1) %>%
    select(-channel_partner_uuid)


#feature engineering --------------------
#calculate discount rate(also grouped version by country and reseller)-----
featured1 <- transformed %>%
    #dplyr::filter(srp_billed_usd != 0) %>% #one row have 0 srp_billed_usd(infinite)
    mutate(discount_rate = 1-ifelse(srp_billed_usd == 0,NA,total_billed_usd/srp_billed_usd))
#calculate aggregated features(aggregated car pings) -----------
#calculate means -------------------
aggregate_by <- c("channel_partner_csn","country_code")
aggregate_on <- c(colnames(featured1)[str_detect(colnames(featured1),"^car_")],"percentage_reg_seats_activated","discount_rate")
featured2 <- featured1
for(x in aggregate_by){
    featured2 <- featured2 %>%
        group_by(UQ(rlang::sym(x))) %>%
        mutate_at(vars(one_of(aggregate_on)),funs(UQ(rlang::sym(x)):=mean(.,na.rm = T))) %>%
        ungroup()
}

#calculate sums ---------
# aggregate_by <- c("channel_partner_csn","country_code")
# aggregate_on <- c("active_registered_seat_sum")
# featured2 <- featured1
# for(x in aggregate_by){
#     featured2 <- featured2 %>%
#         group_by(UQ(rlang::sym(x))) %>%
#         mutate_at(vars(one_of(aggregate_on)),funs(UQ(rlang::sym(x)):=sum(.,na.rm = T))) %>%
#         ungroup()
# }
featured2 <- featured2 %>%
    group_by(country_code) %>%
    mutate(active_registered_seat_sum_country = sum(active_registered_seat_sum,na.rm = T)) %>%
    ungroup() %>%
    group_by(channel_partner_csn) %>%
    mutate(active_registered_seat_sum_partner = sum(active_registered_seat_sum,na.rm = T)) %>%
    ungroup()
    
drop_additional_columns <- colnames(featured1)[str_detect(colnames(featured1),"^car_")]
featured2 <- featured2 %>%
    select(-one_of(drop_additional_columns))
# drop channel partner csn because we already aggregated on it-----------
featured_complete <- featured2 %>%
    select(-channel_partner_csn)

#Use one of the following three for future analysis:post_feat_eng,complete_df or one_hot.

#post_feat_eng is dataset after cleaning and feature engineering -----------
post_feat_eng <- featured_complete %>%
    mutate_if(~is.logical(.x),~as.integer(.x)) %>% #convert logical into integer
    mutate(index = row_number()) 
#drop infinite ----------------
#one row have 0 srp_billed_usd(infinite)
infinite_dropped <- post_feat_eng %>%
    dplyr::filter_at(vars(starts_with("discount")),any_vars(!is.infinite(.)))
#complete_df drop all observations with any missing values, 2000 of 17417 observations are dropped------------
complete_df <- infinite_dropped[complete.cases(infinite_dropped),] 
df_index <- complete_df$index
#one_hot converts complete_df into one hot encoded version----------------
one_hot <- as.data.frame(model.matrix(index ~ .,data=complete_df)[,-1])