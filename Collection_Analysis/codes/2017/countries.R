#Customer distribution on three major markets
setup(desired_working_file_name = "first_week_report",
      required_packages = c("tidyverse","stringr","lubridate","xlsx","forcats"))
load("dataset.RData")
country_codes <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")

#APAC = Asia-Pacific(Asia + Oceania), EMEA = Europe, the Middle East and Africa(Europe + Africa), AMER =Americas(Africa)
#AEC = Architecture Engineering Construction Collection, PDM =Autodesk Product Design & Manufacturing Collection
#M&E = Media and Entertainment Collection, PD = Product Design Collection

region_seats_data <- dataset %>%
    filter(contract_status == "Active") %>%
    mutate(iso_country_alpha2_code = as.character(iso_country_alpha2_code)) %>%
    group_by(entitlement_id,owner_id) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    group_by(iso_country_alpha2_code,feature_name) %>%
    summarise(total_seats = sum(purchased_seat_quantity)) %>%
    left_join(country_codes,by = c("iso_country_alpha2_code" = "alpha-2")) %>%
    group_by(region,feature_name) %>%
    summarise(region_seats = sum(total_seats)) %>%
    filter(!is.na(region)) %>%
    ungroup() %>%
    mutate(region = as.factor(region)) %>%
    mutate(larger_region = fct_collapse(region,
                                        APAC = c("Asia","Oceania"),
                                        EMEA = c("Europe","Africa"),
                                        AMER = "Americas")) %>%
    mutate(abbr_feature_name = fct_recode(feature_name,
                                          "AEC" = "Architecture Engineering Construction Collection",
                                          "PD" = "Autodesk Product Design & Manufacturing Collection",
                                          "M&E" = "Media and Entertainment Collection")) %>%
    group_by(larger_region,abbr_feature_name) %>%
    select(larger_region, abbr_feature_name,region_seats) %>%
    group_by(larger_region,abbr_feature_name) %>%
    summarise(region_seats = sum(region_seats)) %>%
    ungroup();region_seats_data

collection_name <- as.character(region_seats_data %>% distinct(abbr_feature_name) %>% pull(abbr_feature_name))

#seats distribution for four different collections

AEC_seats_distribution <- region_seats_data %>%
        filter(abbr_feature_name == collection_name[1]) %>%
        mutate(all_seats = sum(region_seats)) %>%
        mutate(percentage = str_c(round(region_seats/all_seats,2)*100,"%"))

MnE_seats_distribution <- region_seats_data %>%
    filter(abbr_feature_name == collection_name[3]) %>%
    mutate(all_seats = sum(region_seats)) %>%
    mutate(percentage = str_c(round(region_seats/all_seats,2)*100,"%"))

PD_seats_distribution <- region_seats_data %>%
    filter(abbr_feature_name == collection_name[2]) %>%
    mutate(all_seats = sum(region_seats)) %>%
    mutate(percentage = str_c(round(region_seats/all_seats,2)*100,"%"))


#total seats for all collections
total_seats <- region_seats_data %>%
    group_by(abbr_feature_name) %>%
    summarise(total_seats = sum(region_seats)) %>%
    mutate(total_seats = str_c(round(total_seats/1000,1),"k"));total_seats
    
dir.create("slide_page_5")


write.xlsx(AEC_seats_distribution %>%
               select(larger_region,region_seats,percentage,abbr_feature_name),
           file ="slide_page_5/seats_and_distribution.xlsx",
           sheetName = "AEC_seats_distribution")
write.xlsx(MnE_seats_distribution %>%
               select(larger_region,region_seats,percentage,abbr_feature_name),
           file = "slide_page_5/seats_and_distribution.xlsx",
           sheetName = "MnE_seats_distribution",
           append=TRUE)
write.xlsx(PD_seats_distribution %>%
               select(larger_region,region_seats,percentage,abbr_feature_name),
           file = "slide_page_5/seats_and_distribution.xlsx",
           sheetName = "PD_seats_distribution",
           append=TRUE)
write.xlsx(total_seats,
           file = "slide_page_5/seats_and_distribution.xlsx",
           sheetName = "total_seats",
           append=TRUE)

