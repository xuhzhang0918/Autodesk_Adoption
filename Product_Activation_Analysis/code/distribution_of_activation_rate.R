source("code/load_data.R")
library(RColorBrewer)
library(StatMeasures)
#choose effective dates and contract type----------------
dataset_selected <- dataset %>% 
  dplyr::filter(agreement_status == "Active",order_date >= "2017-04-01")


dataset_act_calculated <- dataset_selected %>%
  mutate(activation_rate = active_and_registered_seats_activated/active_registered_seat_sum)

dataset_act_calculated %>%
  ggplot() +
  geom_freqpoly(aes(x=activation_rate)) +
  theme_light()


dataset_selected %>%
  mutate(activation_rate = percentage_reg_seats_activated) %>%
  ggplot() +
  geom_freqpoly(aes(x=activation_rate)) +
  theme_light()

outlier_obj <- StatMeasures::outliers(dataset_act_calculated$activation_rate)
outlier_obj$idxOutliers
