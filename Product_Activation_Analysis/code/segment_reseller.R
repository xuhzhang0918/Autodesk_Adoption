
source("code/load_data.R",local = T)

#calculate percentage of low activation accounts for each reseller and each size group---
reseller_segment<-dataset%>%
  group_by(channel_partner_csn) %>%
  mutate(sum_sale = n()) %>%
  ungroup() %>%
  mutate(partner_size=case_when(
    (sum_sale > 7) & (sum_sale <= 64) ~ "medium",
    sum_sale > 64 ~ "large", T ~ "small"))%>%
  group_by(partner_size)%>%
  mutate(partner_group_sum_sale = sum(sum_sale))%>%
  ungroup()%>%
  dplyr::filter(active_registered_seat_sum >= 2,
                agreement_status == "Active") %>%
  mutate(activity_group = case_when(
    percentage_reg_seats_activated < 25 ~ "low",
    percentage_reg_seats_activated > 75 ~ "high",
    (percentage_reg_seats_activated >= 25) && (percentage_reg_seats_activated <= 75) ~ "medium",
    T ~ "other"
  ))%>%
  group_by(partner_size,partner_group_sum_sale, channel_partner_csn, sum_sale, activity_group, partner_tier) %>%
  summarise(count = n()) %>%
  dplyr::filter(activity_group == "low") %>%
  mutate(low_act_rate = count/sum_sale)%>%
  ungroup()%>%
  group_by(partner_size)%>%
  mutate(avg_low_act_rate = mean(low_act_rate))%>%
  ungroup()


# generate table for reseller and country----
reseller_country<-dataset %>%
  distinct(country_name,country_code,channel_partner_csn)

#calculate multi country amount for each reseller----
reseller_segment%>%
  left_join(x= reseller_segment, y = reseller_country, by ='channel_partner_csn')%>%
  dplyr::filter(!is.na(country_name))%>%
  select(channel_partner_csn, country_name)%>%
  group_by(channel_partner_csn)%>%
  mutate(multi_country = n())%>%
  arrange(desc(multi_country))%>%
  ungroup()

# percentile
  #  0%  25%  50%  75% 100% 
  # 1    1    1    1   25 
  




