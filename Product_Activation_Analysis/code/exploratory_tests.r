filter <- dplyr::filter

dataset_raw <- dataset
dataset_17q2_after = dataset_raw %>%
  dplyr::filter(order_date >= ymd("2017-04-01") )

sptest2 = dataset_17q2_after %>%
  filter(percentage_reg_seats_activated==0,
         car_pings_in_previous_third_month>0) #1457 obs with no activation but have pings. Clarify------------------

#general data summary, correlations and other EDA-----------------------------------
hist(dataset_17q2_after$pns_days_since_agreement_start, breaks=100) #histogram of distribution. SOme spikes, could indicate data collection patterns/issues
summary(dataset_17q2_after)
summary(dataset_17q2_after$percentage_reg_seats_activated) #mean 41.78%
glimpse(dataset_17q2_after)
numeric_only = dataset_17q2_after %>%
  select_if(is.numeric)
our_cor =  cor(numeric_only, use = 'complete.obs') #any other way to handle NA? complete will drop the whole column even for 1 NA.
corrplot::corrplot(our_cor) #huge and chaotic
#we want the corr of percentage_reg_seats to find the highest (by absolute value) correlation
cor_of_act = our_cor['percentage_reg_seats_activated',]
cor_of_act_abs = abs(cor_of_act) #Absolute values of correlation scores
cor_of_act_abs = sort(cor_of_act_abs, decreasing = TRUE)
cor_of_act_abs #highest one 0.35, not very strong

dataset_17q2_after %>%
  group_by(onboarding_health) %>%
    summarise(mean_activation = mean(percentage_reg_seats_activated))

by_chan_part_id = dataset_17q2_after %>%
  group_by(channel_partner_uuid)

#by reseller------------------
activation_by_reseller = by_chan_part_id %>% #! See the large range. Not all resellers are created equal. WHY?
  summarise(mean_activation = mean(percentage_reg_seats_activated), #company basis 
            reseller_tot_seats = sum(active_registered_seat_sum), 
            country = first(country_name), #tenative country
            reseller_tot_pings_one_month = sum(dap_pings_in_previous_one_month, car_pings_in_previous_one_month,
                                               pns_days_used_in_previous_one_month),
            pings_per_seat = reseller_tot_pings_one_month / reseller_tot_seats) %>% 
    arrange(desc(mean_activation))

cor(activation_by_reseller$mean_activation, activation_by_reseller$pings_per_seat)

#pings summary by reseller------------------
reseller_by_pings = activation_by_reseller %>%
  filter(reseller_tot_seats >= 100) %>%
    arrange(desc(pings_per_seat))

reseller_by_pings 

res_ping_countrysumm = reseller_by_pings %>%
  group_by(country) %>%
    summarise(max_for_country = max(pings_per_seat),
              min_for_country = min(pings_per_seat))

res_ping_countrysumm = res_ping_countrysumm %>%
  mutate(ping_diff_intracountry = max_for_country - min_for_country)
res_ping_countrysumm



#by country-------------------
country_sum = activation_by_reseller %>% #!!! Very significant differences. China seems to perform by far the worst of all the large markets. WHY?
  group_by(country) %>%
    summarise(country_mean = mean(mean_activation),
              num_records = sum(reseller_tot_seats)) %>%
      arrange(desc(country_mean))
head(country_sum)
tail(country_sum)


#by channel partner type---------------
by_chan_part_type = dataset_17q2_after %>% #no significant correlation
  group_by(channel_partner_type) %>%
    summarise(mean_activation = mean(percentage_reg_seats_activated))


dataset2 = dataset_17q2_after %>%
  mutate(quarter1 = quarter(order_date,with_year = T))

#by channel partner tier--------------

by_chan_part_tier = dataset2 %>% 
  group_by(partner_tier) %>%
  summarise(mean_activation = mean(percentage_reg_seats_activated)) %>%
  ungroup()

#by customer size-------------------
by_cust_size = dataset_17q2_after %>% #SMB and ENT do better than VSB. UNK do badly!
  group_by(customer_size_code) %>%
    summarise(mean_activation = mean(percentage_reg_seats_activated))

#by quarter-----------------------                                   
by_quarter = dataset_17q2_after %>%
  group_by(fiscal_quarter_and_year) %>%
    summarise(mean_activation = mean(percentage_reg_seats_activated))

active_only = dataset_17q2_after %>%
  filter(agreement_status == 'Active')

non_zero_seats = dataset_17q2_after %>%
  filter(active_registered_seat_sum > 0)

     
across_userbase_activation = non_zero_seats %>%
  group_by(fiscal_quarter_and_year) %>%
    mutate(on_seats_basis_mean_activation = sum(active_and_registered_seats_activated)/sum(active_registered_seat_sum)) %>% # there are some cases of 0 seats
  ungroup()

#rates on COMPANY basis---------------------
mean_act_by_quarter_compbasis_p = ggplot(data = by_quarter, mapping = aes(x=fiscal_quarter_and_year, y=mean_activation, group =1)) + 
  geom_line(color = 'red') + geom_point()
mean_act_by_quarter_compbasis_p #why is the trend so upward? seems very data collection related, hard to believe that the actual activations change so much.
#why we have future data? Billing?
mean_act_by_part_tier_p = ggplot(data = by_chan_part_tier, mapping = aes(x=partner_tier, y=mean_activation)) + geom_col()
mean_act_by_part_tier_p 

#rates across the whole userbase (On SEATS basis, NOT company)-----------------
mean_act_by_quarter_userbase_p = ggplot(data = across_userbase_activation, mapping = aes(x=fiscal_quarter_and_year, y=on_seats_basis_mean_activation, group=1)) + 
  geom_line(color = 'red') + geom_point()
mean_act_by_quarter_userbase_p #above 1???

sum(dataset_17q2_after$active_registered_seat_sum)
#[1] 192198
sum(dataset_17q2_after$active_and_registered_seats_activated)
#[1] 192712
#unexpected small difference. Parent / owner? 1956AC7C-C016-4a39-83BD-A0057BC972E5 has 7 activated but 0 total seats???

#some rows have more activated than registered. We clarified this means multi-activations on different machines, thus 100% is used
problematic_rows <- dataset_17q2_after %>%
  dplyr::filter(active_and_registered_seats_activated > active_registered_seat_sum)


#group by quater and redo----------------
dataset_17q2_after %>%
  dplyr::filter(order_date > ymd("2015-03-15")) %>%
  mutate(quarter1 = quarter(order_date,with_year = T)) %>%
  group_by(quarter1) %>%
  summarise(mean_activation = mean(percentage_reg_seats_activated)) %>%
  ggplot(mapping = aes(x=quarter1, y=mean_activation, group =1)) + 
  geom_line(color = 'red') + geom_point()


relationship <- dataset_17q2_after %>% #what's that for?
  distinct(channel_partner_csn,channel_partner_uuid) %>%
  group_by(channel_partner_csn) %>%
  mutate(one_to_many = n_distinct(channel_partner_uuid)) %>%
  ungroup()
all(relationship$one_to_many == 1)


dataset_17q2_after %>%
  group_by(customer_size_code,partner_tier) %>%
  summarise(mean_activation = mean(percentage_reg_seats_activated)) %>%
  ungroup() %>%
  ggplot(aes(x = customer_size_code,y=partner_tier,fill=mean_activation)) +
  geom_tile()



#compare resellers intra-country------------------
#China--------
act_res_china = activation_by_reseller %>%
  filter(country=='China') %>%
    arrange(desc(reseller_tot_seats))
act_res_china # 3 resellers, means: 22 , 38 , 14, all above 150 seats
#low act, high Pings per seat. Reason?


activation_by_reseller %>%
  arrange(country)

by_res_over_100_seats = activation_by_reseller %>%
  filter(reseller_tot_seats >= 100) %>%
    arrange(country)

intra_country_res_diff_mean_act = by_res_over_100_seats %>%
  group_by(country) %>%
  summarise(diff = max(mean_activation)-min(mean_activation)) %>%
    arrange(desc(diff))

intra_country_res_diff_mean_act
#write_csv(intra_country_res_diff_mean_act, 'intra_country_compabsis.csv')
#re-do on a seat basis accross clients (previous was on a COMPANY-CLIENT basis)---------------------

# intra_country_res_diff_mean_act_seats_basis = by_res_over_100_seats %>%
#   group_by(country) #%>%
  # summarise(diff_seat_basis = max(mean_activation)-min(mean_activation)) %>%
  # arrange(desc(diff))

#distribution of mean activation rates by age of contract----------------------------
dataset_with_datebins = dataset_17q2_after %>%
  mutate(order_date_conv = ymd(order_date), # <date> variables
         days_since_agr = Sys.Date() - order_date_conv) # <time> variables

#group by week----------------
activation_percentile <- quantile(dataset_17q2_after$percentage_reg_seats_activated,probs = seq(0,1,0.01))

dataset_weekly = dataset_17q2_after %>%
  dplyr::filter(order_date > ymd("2015-03-15")) %>%
  mutate(year_week_order = str_c(year(order_date),".",week(order_date)),
         activation_group = if_else(percentage_reg_seats_activated <= 0.5,"low","high")) %>%
  group_by(year_week_order,activation_group) %>%
  summarise(mean_activation = mean(percentage_reg_seats_activated),
            count = n()) %>%
  ungroup()

dataset_weekly %>%
  ggplot(mapping = aes(x=year_week_order, y=mean_activation, group=activation_group)) + 
  geom_line(aes(color = activation_group)) + geom_point() +
  geom_text(aes(label=count)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


dataset_weekly = dataset_17q2_after %>% 
  dplyr::filter(order_date > ymd("2015-03-15")) %>% 
  mutate(week_agr_start_date = week(order_date), year_agr_start_date= year(order_date), 
         year_week_order =paste0(year_agr_start_date,"-", week_agr_start_date)) %>% 
  group_by(year_week_order) %>% 
  summarise(week_agr_start_date = first(week_agr_start_date),mean_activation = mean(percentage_reg_seats_activated), 
            count = n()) 

dataset_weekly %>% 
  ggplot(mapping = aes(x=year_week_order, y=mean_activation, group =1)) +  
  geom_line(color = 'red') + geom_point() + 
  geom_text(aes(label = count)) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


ggplot(dataset_weekly) + geom_col( aes(x=week_agr_start_date , y=mean_activation)) 


#PASTE of all Yifu's code from data_explore_yifu.R starts here----------------------

source("code/load_data.R",local = T)
#Based on starting quater of contract, compare activation rate/(or on boarding health)--------
#percentage_reg_seats_activated
dataset %>%
  dplyr::filter((order_date >= ymd("2016-01-01")) & (order_date <= ymd("2018-01-15")),
                agreement_status == "Active") %>%
  mutate(start_quarter=factor(quarter(order_date,with_year = T)),
         activity_group = case_when(
           percentage_reg_seats_activated < 25 ~ "low",
           percentage_reg_seats_activated > 75 ~ "high",
           (percentage_reg_seats_activated >= 25) && (percentage_reg_seats_activated <= 75) ~ "medium",
           T ~ "other"
         )) %>%
  group_by(activity_group,start_quarter) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(start_quarter) %>%
  mutate(low_percentage = count/sum(count)) %>%
  ungroup() %>%
  dplyr::filter(activity_group == "low") %>%
  ggplot(aes(x = start_quarter,y = low_percentage)) +
  geom_col() +
  theme_light()

#onboarding_health
dataset %>%
  dplyr::filter((order_date >= ymd("2016-01-01")) & (order_date <= ymd("2018-01-15"))) %>%
  dplyr::filter(!is.na(onboarding_health),
                agreement_status == "Active") %>%
  mutate(start_quarter=factor(quarter(order_date,with_year = T)),
         onboarding_health=factor(onboarding_health,levels = rev(c("At Risk","Monitor","Good")))) %>%
  group_by(onboarding_health,start_quarter) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(start_quarter) %>%
  mutate(percentage = count/sum(count)) %>%
  ungroup() %>%
  ggplot(aes(x = start_quarter,y = percentage,fill=onboarding_health)) +
  geom_col(position = "stack") +
  theme_light()

# for big companies with many seats -------------------
#quantile
#0%  25%  50%  75% 100% 
#0    0    1    3 1124 
dataset %>%
  dplyr::filter(agreement_status == "Active") %>%
  pull(active_and_registered_seats_activated) %>%
  quantile()

#how do different partner do ----------------
partner_sale <- dataset %>%
  dplyr::filter(active_and_registered_seats_activated >= 2,
                agreement_status == "Active") %>%
  mutate(activity_group = case_when(
    percentage_reg_seats_activated < 25 ~ "low",
    percentage_reg_seats_activated > 75 ~ "high",
    (percentage_reg_seats_activated >= 25) && (percentage_reg_seats_activated <= 75) ~ "medium",
    T ~ "other"
  )) %>%
  group_by(channel_partner_csn,activity_group) %>%
  summarise(count = n()) %>%
  ungroup()

#sale quantile
# 0%  25%  50%  75% 100% 
# 1    2    7   40 1484 
partner_sale_quantile <- partner_sale %>%
  group_by(channel_partner_csn) %>%
  summarise(all_sold = sum(count)) %>% 
  pull(all_sold) %>%
  quantile()
#partner sale and activation rate comparision
partner_sale %>%
  group_by(channel_partner_csn) %>%
  mutate(all_sold = sum(count)) %>%
  dplyr::filter(all_sold > 7) %>%
  ungroup() %>%
  dplyr::filter(activity_group == "low") %>%
  mutate(low_act_rate = count/all_sold) %>%
  arrange(desc(low_act_rate)) 


# How are big and medium partner different? -------------
partner_sale <- partner_sale %>%
  group_by(channel_partner_csn) %>%
  mutate(all_sold = sum(count)) %>%
  ungroup() %>%
  mutate(partner_size=case_when(
    (all_sold > partner_sale_quantile[3]) & (all_sold <= partner_sale_quantile[4]) ~ "medium",
    all_sold > partner_sale_quantile[4] ~ "large",
    T ~ "small"))

# calculate low activation rate for partners of different size------------------
partner_performance<-partner_sale %>%
  dplyr::filter(activity_group == "low") %>%
  mutate(low_act_rate = count/all_sold) %>%
  arrange(partner_size,desc(low_act_rate))


# activation rate and onboarding health comparision -----------
dataset %>%
  mutate(activity_group = case_when(
    percentage_reg_seats_activated < 25 ~ "low",
    percentage_reg_seats_activated > 75 ~ "high",
    (percentage_reg_seats_activated >= 25) && (percentage_reg_seats_activated <= 75) ~ "medium",
    T ~ "other"
  ))  %>%
  group_by(activity_group,onboarding_health) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=onboarding_health,y=count)) +
  geom_col() +
  facet_wrap(~activity_group)






