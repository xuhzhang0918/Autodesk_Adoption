#read_entitlement reads entitlement data with less erros
read_entitlement <- function(dataset){
    result <- read_tsv(dataset,col_types = cols(
        entitlement_id = col_character(),
        offering_type = col_character(),
        offering_name = col_character(),
        owner_id = col_character(),
        entitlement_model = col_character(),
        contract_id = col_character(),
        contract_status = col_character(),
        contract_start_date = col_date(),
        contract_end_date = col_date(),
        feature_name = col_character(),
        product_line_code = col_character(),
        product_version = col_character(),
        purchased_seat_quantity = col_integer(),
        entitlement_registration_status = col_character(),
        entitlement_deployment_type = col_character(),
        billing_behavior_type = col_character(),
        contract_term = col_character(),
        user_device_composite_id = col_character(),
        product_line_name = col_character(),
        product_name = col_character(),
        product_release_id = col_integer(),
        sessions = col_integer(),
        account_uuid = col_character(),
        static_parent_industry_segment = col_character(),
        static_parent_industry_group = col_character(),
        iso_country_alpha2_code = col_character(),
        county_name = col_character(),
        is_individual = col_logical(),
        dt = col_character()
    ))
    return(result)
}