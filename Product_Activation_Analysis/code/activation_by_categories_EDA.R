#activation rate EDA by various categories-------

#split single seat / multi seat contracts---------
source("code/load_data.R")
library(RColorBrewer)
#choose effective dates and contract type
dataset_selected <- dataset %>% 
  dplyr::filter(agreement_status == "Active",order_date >= "2017-04-01")


dataset_selected %>%
  mutate(is_single_seat = ifelse(active_registered_seat_sum==1,T,F)) %>%
  group_by(is_single_seat) %>%
  mutate(activation = cut(percentage_reg_seats_activated,breaks = c(-Inf,0.2,0.4,0.6,0.8,1))) %>%
  group_by(is_single_seat,activation) %>%
  summarise(company_count = n()) %>%
  group_by(is_single_seat) %>%
  mutate(pct = company_count/sum(company_count)) %>%
  ungroup() %>%
  mutate(is_single_seat = ifelse(is_single_seat,"single seat","multiple seat")) %>%
  ggplot() +
  geom_col(aes(x = activation, y = pct,fill = is_single_seat)) +
  facet_wrap(~is_single_seat) +
  theme_light() +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  guides(fill=FALSE) +
  scale_fill_brewer(palette="Paired")


dataset_selected %>%
  mutate(is_single_seat = ifelse(active_registered_seat_sum==1,T,F)) %>%
  group_by(is_single_seat) %>%
  mutate(activation = cut(percentage_reg_seats_activated,breaks = c(-Inf,0.2,0.4,0.6,0.8,1))) %>%
  group_by(is_single_seat,activation) %>%
  summarise(company_count = n()) %>%
  group_by(is_single_seat) %>%
  mutate(pct = company_count/sum(company_count)) %>%
  ungroup() %>%
  mutate(is_single_seat = ifelse(is_single_seat,"single seat","multiple seat")) %>% 
  group_by(is_single_seat) %>% 
  mutate(count=sum(company_count))


#by partner size (reseller's total number of seats sold)-------------------
source("code/load_data.R")
library(RColorBrewer)
#choose effective dates and contract type
dataset_selected <- dataset %>% 
  dplyr::filter(agreement_status == "Active",order_date >= "2017-04-01")

#number of seats sold by partner_csn
seats_sold_percentile <- dataset_selected %>%
  group_by(channel_partner_csn) %>%
  summarise(total_seats_sold = sum(active_registered_seat_sum)) %>%
  pull(total_seats_sold) %>%
  quantile(probs = seq(0, 1, 0.01))

#find abnormal activation:
dataset_selected %>%
  #group_by(channel_partner_csn) %>%
  mutate(mean_activation = active_and_registered_seats_activated/active_registered_seat_sum) %>%
  ggplot() +
  geom_boxplot(aes(x=1,y=mean_activation)) +
  scale_y_continuous(breaks = seq(0,80,1)) 


partner_with_size_label <- dataset_selected %>%
  group_by(channel_partner_csn) %>%
  mutate(total_seats_sold = sum(active_registered_seat_sum)) %>%
  #cut point
  mutate(partner_size = case_when(total_seats_sold >=seats_sold_percentile[75] ~ "large",
                                  total_seats_sold < seats_sold_percentile[42] ~ "small",
                                  T ~ "medium")) %>%
  ungroup()

partner_with_size_label %>%
  group_by(channel_partner_csn) %>%
  dplyr::filter(total_seats_sold > 2) %>%
  summarise(mean_activation = sum(active_registered_seat_sum*percentage_reg_seats_activated)/sum(active_registered_seat_sum),
            partner_size = first(partner_size)) %>%
  ggplot() +
  geom_boxplot(aes(x=partner_size,y=mean_activation))

#low activation group count
partner_with_label %>%
  mutate(activation_rate = cut(percentage_reg_seats_activated,breaks = c(-Inf,0.2,0.4,0.6,0.8,1))) %>%
  group_by(channel_partner_csn) %>%
  dplyr::filter(total_seats_sold > 2) %>% # filter more than 2 seats
  mutate(total_contract = n()) %>%
  dplyr::filter(activation_rate == "(-Inf,0.2]") %>%
  summarise(low_activation_rate = n()/first(total_contract),
            partner_size = first(partner_size)) %>%
  ggplot() +
  geom_boxplot(aes(x=fct_rev(partner_size),y=low_activation_rate,fill=partner_size)) +
  scale_fill_brewer(palette = "Set3") +
  theme_light() +
  guides(fill=FALSE) +
  labs(x="partner_size",y="percentage",title = "Percentage of Seats of Low Activation")

#by industry----------------
source("code/load_data.R")
library(RColorBrewer)
#choose effective dates and contract type
dataset_selected <- dataset %>% 
  dplyr::filter(agreement_status == "Active",order_date >= "2017-04-01")


#no separation between multiple and single seat:
industry_activation <- dataset_selected %>%
  group_by(hier_industry_segment_name) %>%
  mutate(activation = cut(percentage_reg_seats_activated,breaks = c(-Inf,0.2,0.4,0.6,0.8,1))) %>%
  group_by(hier_industry_segment_name,activation) %>%
  summarise(number_seats = sum(active_registered_seat_sum)) %>%
  group_by(hier_industry_segment_name) %>%
  mutate(industry_total_seats = sum(number_seats)) %>%
  ungroup() %>%
  #filter out not important industries(less than 25% percentile of the seats)
  #dplyr::filter(industry_total_seats >= quantile(industry_total_seats)[2]) %>%
  #calculate percentages
  mutate(pct=number_seats/industry_total_seats) %>%
  #create ranking order
  group_by(hier_industry_segment_name) %>%
  mutate(rank_col = ifelse(activation == "(-Inf,0.2]",pct,0)) %>%
  mutate(rank_col=sum(rank_col)) %>%
  ungroup() %>%
  mutate(rank_order = min_rank(rank_col))
industry_activation

# Top industries with low activation rate
industry_activation %>%
  dplyr::filter(!is.na(hier_industry_segment_name)) %>% # filter out NA
  dplyr::filter(rank_order >= sort(unique(rank_order),decreasing = T)[5]) %>%
  ggplot() +
  geom_col(aes(x=fct_reorder(hier_industry_segment_name,desc(rank_order)),
               y=pct,
               fill=fct_rev(activation)),
           alpha = 0.7)+
  geom_text(aes(x=hier_industry_segment_name,y=1.02,label=industry_total_seats)) +
  coord_flip() +
  labs(x="industry",y="percentage",fill="activation",title = "Activation Rate in Idustries",subtitle = "top 5 industries with low activation rate") +
  scale_fill_brewer(palette = "Accent") +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  theme_light()

# Top 10 industries with most seats
industry_activation %>%
  dplyr::filter(!is.na(hier_industry_segment_name)) %>% # filter out NA
  dplyr::filter(industry_total_seats >= sort(unique(industry_total_seats),decreasing = T)[5]) %>%
  ggplot() +
  geom_col(aes(x=fct_reorder(hier_industry_segment_name,desc(rank_order)),
               y=pct,
               fill=fct_rev(activation)),
           alpha = 0.7)+
  geom_text(aes(x=hier_industry_segment_name,y=1.02,label=industry_total_seats)) +
  coord_flip() +
  labs(x="industry",y="percentage",fill="activation",title = "Activation Rate in Idustries",subtitle = "top 5 industries with most seats") +
  scale_fill_brewer(palette = "Accent") +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  theme_light()

#by product group-----------------

source("code/load_data.R")
library(RColorBrewer)
#choose effective dates and contract type
dataset_selected <- dataset %>% 
  dplyr::filter(agreement_status == "Active",order_date >= "2017-04-01")

product_activation <- dataset_selected %>%
  group_by(bmt_product_group) %>%
  mutate(activation = cut(percentage_reg_seats_activated,breaks = c(-Inf,0.2,0.4,0.6,0.8,1))) %>%
  group_by(bmt_product_group,activation) %>%
  summarise(number_seats = sum(active_registered_seat_sum)) %>%
  group_by(bmt_product_group) %>%
  mutate(industry_total_seats = sum(number_seats)) %>%
  ungroup() %>%
  #filter out not important industries(less than 25% percentile of the seats)
  #dplyr::filter(industry_total_seats >= quantile(industry_total_seats)[2]) %>%
  #calculate percentages
  mutate(pct=number_seats/industry_total_seats) %>%
  #create ranking order
  group_by(bmt_product_group) %>%
  mutate(rank_col = ifelse(activation == "(-Inf,0.2]",pct,0)) %>%
  mutate(rank_col=sum(rank_col)) %>%
  ungroup() %>%
  mutate(rank_order = min_rank(rank_col))


product_activation %>%
  ggplot() +
  geom_col(aes(x=fct_reorder(bmt_product_group,desc(rank_order)),
               y=pct,
               fill=fct_rev(activation)),
           alpha = 0.7)+
  geom_text(aes(x=bmt_product_group,y=1.02,label=industry_total_seats)) +
  coord_flip() +
  labs(x="industry",y="percentage",fill="activation",title = "activation rate in product groups") +
  scale_fill_brewer(palette = "Accent") +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  theme_light()
