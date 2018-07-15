source("code/data_cleaning.R")

post_feat_eng %>%
  mutate(discount_group = cut(discount_rate,breaks = seq(0,1,0.2))) %>%
  group_by(discount_group) %>%
  summarise(mean_activation = mean(percentage_reg_seats_activated,na.rm = T),
            count=n()) %>%
  dplyr::filter_all(all_vars(!is.na(.))) %>%
  ggplot(aes(x=discount_group,y=mean_activation)) +
  geom_col()



post_feat_eng %>%
  ggplot(aes(x=discount_rate ,y=percentage_reg_seats_activated)) +
  geom_point() +
  geom_smooth(method = "lm")


fit <- lm(discount_rate~percentage_reg_seats_activated,data = post_feat_eng)
summary(fit)

#correlation between discount rate and activation rate
with(data = post_feat_eng,cor(discount_rate,percentage_reg_seats_activated,use = "complete.obs"))
