#Create country mapping
library(tidyverse)

country_map <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")

country_map <- country_map %>% mutate(larger_region = fct_collapse(region,
                                                    APAC = c("Asia","Oceania"),
                                                    EMEA = c("Europe","Africa"),
                                                    AMER = "Americas"))
write_csv(country_map,"data/country_mapping.csv")


