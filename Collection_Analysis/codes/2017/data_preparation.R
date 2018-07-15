setup(desired_working_file_name = "first_week_report",
      required_packages = c("tidyverse","stringr","lubridate","xlsx"))
#
data_files <- list.files()[!is.na(str_extract(list.files(),"^entitlement_.*.tsv"))]

for(i in 1 : length(data_files)) {
    variable_name <- str_extract(data_files[i],"^.*(?=\\.)")
    assign(variable_name,read_entitlement(data_files[i]))
}
original_dataset <- bind_rows(entitlement_2016,entitlement_2017,entitlement_2017a,entitlement_2017b)
remove(entitlement_2016,entitlement_2017,entitlement_2017a,entitlement_2017b)
#convert column to ideal class
dataset <- original_dataset %>%
    mutate(offering_type = as.factor(offering_type),
           offering_name = as.factor(offering_name),
           entitlement_model = as.factor(entitlement_model),
           contract_status = as.factor(contract_status),
           feature_name = as.factor(feature_name),
           product_line_code = as.factor(product_line_code),
           product_version = as.factor(product_version),
           entitlement_registration_status = as.factor(entitlement_registration_status),
           entitlement_deployment_type = as.factor(entitlement_deployment_type),
           billing_behavior_type = as.factor(billing_behavior_type),
           contract_term = as.factor(contract_term),
           product_line_name = as.factor(product_line_name),
           product_name = as.factor(product_name),
           static_parent_industry_segment = as.factor(static_parent_industry_segment),
           static_parent_industry_group = as.factor(static_parent_industry_group),
           iso_country_alpha2_code = as.factor(iso_country_alpha2_code),
           county_name = as.factor(county_name),
           dt = ymd(dt)
           )

#Starting from 2017-08-05 ,"Product Design Collection" is renamed as "Autodesk Product Design & Manufacturing Collection"
#For integrity, change all names to latter.

levels(dataset$offering_name)[4] <-  "Autodesk Product Design & Manufacturing Collection"
levels(dataset$feature_name)[4] <- "Autodesk Product Design & Manufacturing Collection"


save(dataset,file = "dataset.RData")
