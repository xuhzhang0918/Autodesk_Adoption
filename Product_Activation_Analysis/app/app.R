library(tidyverse)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(stringr)
library(mapview)
#pre input ------
country_code <- suppressMessages(read_csv("country_code.csv"))
country <- geojsonio::geojson_read("countries.geo.json", what = "sp")
country_init <- country@data
#1.calculate data metadata ------
dataset<-readRDS("mydata.rds")
col_types <- map_chr(dataset,~ class(.x))
col_n_distinct <- map_dbl(dataset,~ length(unique(.x)))
meta <- tibble(column = names(col_types),col_type = col_types,n_distinct = col_n_distinct)
dataset <- structure(dataset,meta=meta)
remove(col_types,col_n_distinct,meta)
#2. filter on any columns -------------
columns_filterble <- attributes(dataset)$meta %>%
  dplyr::filter((col_type %in% c("Date","numeric","integer","factor")) |
                  ((col_type == "character") & n_distinct <= 200)) %>%
  select(-n_distinct)
#ui_create function ----
ui_create <- function(i,parent_input){
  renderUI({
    input <- parent_input
    input_id <- str_c("selector",i)
    input_value <- input[[input_id]]
    if(input_value== "Nothing"){
      return()
    } else {
      selected_col_type <- columns_filterble$col_type[columns_filterble$column == input_value]
      if(selected_col_type %in% c("character","factor")){
        all_variables <- sort(unique(as.character(dataset[[input_value]])),na.last = T)
        pickerInput(inputId = str_c("uiset1_selector",i),label = input_value,choices = all_variables,selected = all_variables,multiple = T,
                    options = list(
                      `actions-box` = TRUE, 
                      size = 10,
                      `selected-text-format` = "count > 3"
                    ))
      } else if(selected_col_type == "Date"){
        date_range <- range(dataset[[input_value]],na.rm = T)
        dateRangeInput(inputId =  str_c("uiset1_selector",i),label = input_value,start = ymd(date_range[1]),end = ymd(date_range[2]))
      } else if(selected_col_type %in% c("numeric","integer")){
        number_range <- range(dataset[[input_value]],na.rm = T)
        sliderInput(inputId = str_c("uiset1_selector",i),label = input_value,min = number_range[1],max = number_range[2],value = number_range)
      }
    }
  })
}

#filter on n columns function ----
number_of_filters <- 6
multi_filter <- function(dataset,column_name,column_value,n_filter = number_of_filters){
  temp_data <- dataset
  for(i in 1 : n_filter){
    variable <- rlang::sym(as.character(column_name[i]))
    value = column_value[[i]]
    if(as.character(column_name[i]) == "Nothing"){
      next()
    } else {
      if(class(value) %in% c("Date","integer","numeric")){
        temp_data <- dplyr::filter(temp_data,(UQ(variable) >= value[1]) & (UQ(variable) <= value[2]))
      } else if(class(value) %in% c("factor","character")){
        temp_data <- dplyr::filter(temp_data,UQ(variable) %in% value)
      }else if(is.null(class(value))) {
        next()
      } else {
        # stop(glue::glue("{column_name} is {the_class}, which is unsolvable.",
        #                 the_class = class(value)))
      }
    }
  }
  return(temp_data)
}
#group by selected variables -----------
targets <- c("percentage_reg_seats_activated","onboarding_health")
group_agg <- function(dataset){
  dataset %>%
    mutate(onboarding_health_num = case_when(onboarding_health == "At Risk" ~ 1.0,
                                             onboarding_health == "Monitor" ~ 2.0,
                                             onboarding_health == "Good" ~ 3.0,
                                             T ~ NA_real_)) %>%
    group_by(country_code) %>%
    #SHOULD I USE WEIGHTED AVERAGE?
    summarise(country_name = first(country_name),
              total_seats = sum(active_registered_seat_sum),
              percentage_reg_seats_activated = sum(active_registered_seat_sum*percentage_reg_seats_activated,na.rm = T)/
                sum(ifelse(is.na(percentage_reg_seats_activated),0,active_registered_seat_sum)),
              onboarding_health = sum(onboarding_health_num*active_registered_seat_sum,na.rm = T)/
                sum(ifelse(is.na(onboarding_health_num),0,active_registered_seat_sum)))
}
#filter by number of seats -----------------------
create_seats_dist <- function(dataset){
  num <- quantile(dataset$total_seats,probs = seq(0,1,0.1))
  return(tibble(percentile=str_c(seq(0,100,10),"%"),value=num))
}

#ui---------
ui <- fluidPage(
  fluidRow(column(12,tags$h1("Country-wise Analysis"),tags$h4("This app only works with R version lower than 3.5.0 now"))),
  fluidRow(column(2,wellPanel(
    #left filters
    map(1:number_of_filters, ~ selectInput(str_c('selector', .x), paste0('Filter', .x),choices = c("Nothing",columns_filterble$column),selected = "Nothing"))
  )),
  column(2,
         actionButton(inputId = "update_filter",label = "Update"),
         lapply(1:number_of_filters, function(i) {
           uiOutput(str_c('uiset1', i))
         })),
  column(2,uiOutput(outputId = "ui_seats_filter_max"),
         uiOutput(outputId = "ui_seats_filter_min"),
         tags$h5("Number of Seats Percentile of Countries:"),
         tableOutput(outputId = "seats_dist")),
  column(6,
  mainPanel(
    tabsetPanel(
      tabPanel(str_c("mean_",targets[1]),
               htmlOutput(outputId = "mean_score1"),
               leafletOutput(outputId = "map1")),
      tabPanel(str_c("mean_",targets[2]),
               htmlOutput(outputId = "mean_score2"),
               leafletOutput(outputId = "map2")),
      tabPanel("country data",
               downloadButton(outputId = "download",label = "Download"),
               dataTableOutput(outputId = "country_data")),
      tabPanel("filtered data",htmlOutput(outputId = "filtered_data_text"),
               downloadButton(outputId = "download_filtered",label = "Download"),
               tableOutput("filtered_data"))
    )
  )))
  #tags$style(HTML("hr {border-top: 1px solid #000000;}")),
)

# Server logic-------
server <- function(input, output, session) {
  lapply(1:number_of_filters,function(i){
    parent_input <- input
    output[[str_c("uiset1",i)]]<- ui_create(i,parent_input)
  })
  #filter on the first selectors ----------
  #observeEvent(input$update_filter,{print("update selector_output_names & selector_output_values")})
  selector_output_names <- eventReactive(input$update_filter,map(1:number_of_filters,~input[[str_c("selector",.x)]]))
  selector_output_values <- eventReactive(input$update_filter,map(1:number_of_filters,~input[[str_c("uiset1_selector",.x)]]))
  dataset1 <- reactive({multi_filter(dataset = dataset,column_name = selector_output_names(),column_value = selector_output_values())})
  #tab4 --------------
  #create a download tab for filtered data ---------
  output$filtered_data_text <- renderUI(str_c("Display first 10 of <strong>",nrow(dataset1()),"</strong> rows of Data:") %>% HTML())
  output$filtered_data <- renderTable(head(dataset1(),10),spacing = "xs")
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste('data-filtered-', Sys.time(), '.csv', sep='')
    },
    content = function(con) {
      write_csv(dataset1(), con)
    },
    contentType = "csv"
  )
  #create data for mapping-----
  dataset2 <- eventReactive(input$update_filter,group_agg(dataset1()))
  #filter on dataset1 with count of observations ----------
  output$ui_seats_filter_max <- renderUI(numericInput(inputId = "seats_max",label = "Max Seats",
                                                      value = max(dataset2()$total_seats,na.rm = T)))
  output$ui_seats_filter_min <- renderUI(numericInput(inputId = "seats_min",label = "Min Seats",
                                                      value = min(dataset2()$total_seats,na.rm = T)))
  output$seats_dist <- renderTable(create_seats_dist(dataset2()))
  #use filters with ui_seats_filter_min and ui_seats_filter_max
  # set default value when null 
  min_seats <- reactive({if(is.null(input$seats_min)){-Inf}else{input$seats_min}})
  max_seats <- reactive({if(is.null(input$seats_max)){Inf}else{input$seats_max}})
  dataset3 <- reactive({
    dataset2() %>%
      dplyr::filter(total_seats >= min_seats() & total_seats <= max_seats())
  })
  #tab 3---------
  #print the table to a tab ---------
  output$country_data <- renderDataTable(dataset3())
  #create the map-------
  activation_aggregate_filtered_map <- reactive({
    suppressWarnings(
    dataset3() %>%
      left_join(country_code,by=c("country_code"="alpha-2")) %>%
      select(-c(country_code,`iso_3166-2`,`region-code`,`sub-region-code`,`intermediate-region-code`,name)))
  })
  #data used for plot map's attributes
  fianl_country_data <- reactive(suppressWarnings(
    country_init %>%
       left_join(activation_aggregate_filtered_map(),by = c("id"="alpha-3")))
    )
  output$download <- downloadHandler(
    filename = function() {
        paste('data-', Sys.time(), '.csv', sep='')
      },
      content = function(con) {
        write_csv(fianl_country_data(), con)
      },
    contentType = "csv"
    )
  #For all tabs ----
  total_seats_sum_country <- reactive(fianl_country_data() %>%
                             summarise(all = sum(total_seats,na.rm = T)) %>%
                             pull())
  #tab 1 -----------------
  #mean_score ------
  mean_score1 <- reactive(fianl_country_data() %>%
                            summarise(all = sum(UQ(rlang::sym(targets[1]))*total_seats,na.rm = T)/sum(total_seats,na.rm = T)) %>%
                            pull())
  output$mean_score1 <- renderUI(str_c("<strong>Total Seats:</strong>:",as.character(total_seats_sum_country()),"<br>",
                                       "<strong>Mean ",targets[1],":</strong>",as.character(round(mean_score1(),2)))%>%HTML())
  #map -----
  #text content
  country_text <- reactive(as.character(fianl_country_data()$name))
  stat_num <- reactive(as.character(round(fianl_country_data()$percentage_reg_seats_activated,2)))
  seats_num <- reactive(as.character(round(fianl_country_data()$total_seats)))
  labels <- reactive(str_c("<strong>",country_text(),"</strong><br/>mean_activation:", stat_num(),"<br><strong>Total Seats:</strong><br>",seats_num()))
  labels_html <- reactive(labels() %>% lapply(htmltools::HTML))
  #create map
  mymap1 <- reactive({leaflet(country) %>%
      setView(-55,0,1) %>%
      addTiles()})
  colorpal <- reactive({colorNumeric(
    palette = RColorBrewer::brewer.pal(11,"RdYlGn"),
    domain = 0:1)})
  output$map1 <- renderLeaflet({mymap1()})
  #add polygon
  observe({
    pal <- colorpal()
    leafletProxy("map1", data = country) %>%
      clearShapes() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
                color = ~pal(fianl_country_data()$percentage_reg_seats_activated),
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels_html(),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))
  })
  observe({
    pal <- colorpal()
    leafletProxy("map1", data = country) %>%
      clearControls() %>%
      addLegend(pal = pal,
                values = ~fianl_country_data()$percentage_reg_seats_activated, 
                opacity = 0.7, title = NULL,
                position = "bottomright"
    )
  })
  
  #tab 2 -----------------
  #mean_score ------
  mean_score2 <- reactive(fianl_country_data() %>%
                            summarise(all = sum(UQ(rlang::sym(targets[2]))*total_seats,na.rm = T)/sum(total_seats,na.rm = T)) %>%
                            pull())
  output$mean_score2 <- renderUI(str_c("<strong>Total Seats:</strong>:",as.character(total_seats_sum_country()),"<br>"
                                       ,"<strong>Mean ",targets[2],":</strong>",as.character(round(mean_score2(),2)))%>%HTML())
  #map
  country_text2 <- reactive(as.character(fianl_country_data()$name))
  stat_num2 <- reactive(as.character(round(fianl_country_data()$onboarding_health,2)))
  seats_num2 <- reactive(as.character(round(fianl_country_data()$total_seats)))
  labels2 <- reactive(str_c("<strong>",country_text2(),"</strong><br/>mean_health:", stat_num2(),"<br><strong>Total Seats:</strong><br>",seats_num()))
  labels_html2 <- reactive(labels2() %>% lapply(htmltools::HTML))
  output$map2 <- renderLeaflet({
    leaflet(country) %>%
      setView(-55,0,1) %>%
      addTiles()})
  colorpal2 <- reactive({colorNumeric(
    palette = RColorBrewer::brewer.pal(11,"RdYlGn"),
    domain = 1:3)})
  #add polygon
  observe({
    pal2 <- colorpal2()
    leafletProxy("map2", data = country) %>%
      clearShapes() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
                  color = ~pal2(fianl_country_data()$onboarding_health),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels_html2(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
  })
  observe({
    pal2 <- colorpal2()
    leafletProxy("map2", data = country) %>%
      clearControls() %>%
      addLegend(pal = pal2,
                values = ~fianl_country_data()$onboarding_health, 
                opacity = 0.7, title = NULL,
                position = "bottomright"
      )
  })
}

# Complete app with UI and server components
shinyApp(ui, server)
