library(tidyverse)
library(lubridate)
source(code/data_cleaning.R)

reseller_data <- transformed %>%
  dplyr::filter(srp_billed_usd != 0) %>%
  dplyr::mutate(promotion = round(total_billed_usd / srp_billed_usd,2)) %>%
  dplyr::group_by(channel_partner_csn) %>%
  dplyr::summarize(avg_pct = mean(percentage_reg_seats_activated), avg_promo = mean(promotion), avg_car = mean(car_pings_since_agreement_start), total_seat = sum(active_registered_seat_sum))

reseller_data$avg_pct <- scale(test_data$avg_pct)
reseller_data$avg_promo <- scale(test_data$avg_promo)
reseller_data$avg_car <- scale(test_data$avg_car)
reseller_data$total_seat <- scale(test_data$total_seat)
