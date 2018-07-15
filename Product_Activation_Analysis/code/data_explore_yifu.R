source("code/load_data.R",local = T)
#Based on starting quater of contract, compare activation rate/(or on boarding health)--------
#percentage_reg_seats_activated
dataset %>%
  dplyr::filter((agreement_start_date >= ymd("2016-01-01")) & (agreement_start_date <= ymd("2018-01-15")),
                agreement_status == "Active") %>%
  mutate(start_quarter=factor(quarter(agreement_start_date,with_year = T)),
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
  dplyr::filter((agreement_start_date >= ymd("2016-01-01")) & (agreement_start_date <= ymd("2018-01-15"))) %>%
  dplyr::filter(!is.na(onboarding_health),
                agreement_status == "Active") %>%
  mutate(start_quarter=factor(quarter(agreement_start_date,with_year = T)),
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

