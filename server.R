library(jsonlite)
library(shiny)
library(httr)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)

theme_set(theme_gray(base_size = 14))

source("creds.R")



################## Core functions #############


PingSolarAPI <- function(state, city, raw_id, system_capacity, losses, azimuth, tilt) {
  format = 'json'
  base_pv_watts_url = 'https://developer.nrel.gov/api/pvwatts/v5'
  api_key = 'T0RJUJSzPFyxSXHZXEVlF1kAqmxiFtkNYzKWzn8S' 
  timeframe = 'hourly'
  file_id = paste0("1-", raw_id)
  module_type = 0
  array_type = 1
  timeframe = 'hourly'
  result <- GET(base_pv_watts_url, 
                query = list(format = format, api_key=api_key, 
                             timeframe = timeframe, system_capacity = system_capacity,
                             module_type = module_type, losses = losses,
                             array_type = array_type, tilt = tilt, azimuth = azimuth,
                             file_id = file_id))
  result <- content(result)
  print (result$station_info)
  return (result$outputs$ac)
}


PingRatesAPI <- function(state, city) {
  base_rates_url = 'https://api.data.gov/nrel/utility_rates/v3.json'
  api_key = 'T0RJUJSzPFyxSXHZXEVlF1kAqmxiFtkNYzKWzn8S' 
  address = paste(city, state,sep=", ")
  result <- GET(base_rates_url, query = list(api_key = api_key, address = address))
  result <- content(result)
  utility <- result$outputs$utility_name
  rate <- result$outputs$residential
  return (c(utility, rate))
}


PingDemandDB <- function(table_name, state, city) {
  con <- dbConnect(RMySQL::MySQL(),
                  dbname='solarexport',
                   username=username,
                   password=password,
                   host='stolemarch.cskdhnmb36bo.us-east-1.rds.amazonaws.com',
                   port=3306)
  query <- sprintf("Select * from %s where state='%s' and city='%s'", table_name, state, city)
  selected_frame <- dbGetQuery(con, query)
  dbDisconnect(con)
  selected_frame$gas_total <- NULL
  selected_frame$gas_heating <- NULL
  selected_frame$electric_heating <- NULL
  selected_frame$electric_cooling <- NULL
  selected_frame$row_id <- NULL
  return (selected_frame)
}

ModelWithStorage <- function(net_export_vector, storage_limit) {
  vector_length <- length(net_export_vector)
  
  storage_vector <- vector()
  export_vector <- vector()
  grid_usage_vector <-  vector()
  
  for (row_count in 1:vector_length) {
    net_export <-  net_export_vector[row_count]
    
    if (row_count == 1) {
      grid_usage <-  net_export * -1
      storage <-  0
      export <-  0
    }
    else {
      prior_storage <-  storage_vector[row_count-1]
      
      if (net_export > 0) {
        grid_usage <-  0
        storage <-  min(prior_storage + net_export, storage_limit)
        export <-  max(net_export - (storage_limit - prior_storage), 0)
      }
      else {
        draw <-  net_export * -1
        grid_usage <-  max(draw - prior_storage, 0)
        storage <- max(prior_storage - draw, 0)
        export <- 0
      }
    }
    grid_usage_vector <-  c(grid_usage_vector, grid_usage)
    storage_vector <- c(storage_vector, storage)
    export_vector <- c(export_vector, export)
  }
  result_frame <- as.data.frame(cbind(grid_usage_vector,storage_vector,export_vector))
  names(result_frame) <- c("grid_usage","storage","solar_export")
  return (result_frame)
}


CollectHourlyData <- function(table_name, state, city, system_capacity, losses, azimuth, tilt, storage_limit) {
  
  # Invoke two main data sources
  city_frame <- PingDemandDB(table_name, state, city)
  raw_id <- city_frame$raw_id[1]
  city_frame$solar_ac <- PingSolarAPI(state, city, raw_id, system_capacity, losses, azimuth, tilt)
  
  # New columns,units conversions, calculations
  city_frame$solar_ac <- as.numeric(city_frame$solar_ac)
  city_frame$solar_ac <- city_frame$solar_ac / 1000
  city_frame$net_export <- city_frame$solar_ac - city_frame$load
  city_frame$datetime <- as.POSIXct(city_frame$datetime, tz="GMT", format="%Y-%m-%d %H:%M:%S")
  city_frame <- city_frame[city_frame$datetime <= as.POSIXct("2016-12-31 23:00:00",tz="GMT", format="%Y-%m-%d %H:%M:%S"),]
  city_frame$date <- as.Date(city_frame$datetime)
  city_frame$month <- month(city_frame$datetime)
  city_frame$hour <- hour(city_frame$datetime)
  city_frame$month_start <- city_frame$date
  day(city_frame$month_start) <- 1

  # based on battery, do one calculation or the other
  if (storage_limit > 0) {
    storage_result <- ModelWithStorage(city_frame$net_export, storage_limit)
    city_frame <- cbind(city_frame, storage_result)
    city_frame$solar_onsite <- city_frame$solar_ac - city_frame$solar_export
  }
  else {city_frame <- city_frame %>% mutate(solar_export = ifelse(net_export > 0, net_export, 0), grid_usage = ifelse(net_export < 0, net_export * -1, 0), solar_onsite = solar_ac - solar_export)}
  return (city_frame)
}




################## Minor functions #############



GetCities <- function(state) {
  con <- dbConnect(RMySQL::MySQL(),
                   dbname='solarexport',
                   username=username,
                   password=password,
                   host='stolemarch.cskdhnmb36bo.us-east-1.rds.amazonaws.com',
                   port=3306)
  query <- sprintf("Select city from demand_base where state='%s' group by city", state)
  selected_frame <- dbGetQuery(con, query)
  dbDisconnect(con)
  return (selected_frame$city)
}


GetAnnualExportShare <- function(city_month_frame) {
  annual_export_share <- sum(city_month_frame$solar_export) / sum(city_month_frame$solar_gen)
  annual_export_share <- annual_export_share * 100
  annual_export_share <-  format(round(annual_export_share, 0), nsmall=0)
  annual_export_share <- sprintf("Exporting %s%% percent of solar generation. The rest is used onsite.", annual_export_share)
  return (annual_export_share)
}

GetNetGridUse <- function(city_month_frame) {
  net_grid_use <- sum(city_month_frame$grid_usage) - sum(city_month_frame$solar_export)
  if (net_grid_use > 0) {net_grid_use <- sprintf("Total grid electricity usage is greater than total solar exports. *Net grid usage* of %s kWh.", format(round(net_grid_use, 0), nsmall=0, big.mark=","))}
  else if (net_grid_use < 0) {net_grid_use <- sprintf("Total solar exports are greater than total grid electricity usage. *Net solar export* of %s kWh.", format(round(-net_grid_use, 0), nsmall=0, big.mark=","))}
  return (net_grid_use)
}

GetSolarShare <- function(city_month_frame) {
  solar_share <- sum(city_month_frame$solar_onsite) / sum(city_month_frame$load) 
  solar_share <- solar_share * 100
  solar_share <- format(round(solar_share, 0), nsmall=0)
  solar_share <- sprintf("Solar covers %s%% of household electricity consumption. The rest comes from the grid.", solar_share)
  return (solar_share)
}

GetTotalElectricUse <- function(city_month_frame) {
  total_electric <- sum(city_month_frame$load)
  total_electric <- format(round(total_electric, 0), nsmall=0, big.mark=",")
  total_electric <- sprintf("* Consuming %s kWh total electricity *", total_electric)
  return (total_electric)
}

GetTotalSolarGen <- function(city_month_frame) {
  total_solar <- sum(city_month_frame$solar_gen)
  total_solar <- format(round(total_solar, 0), nsmall=0, big.mark=",")
  total_solar <- sprintf("* Generating %s kWh total solar *", total_solar)
  return (total_solar)
}


CheckInputs <- function(state, city) {
  con <- dbConnect(RMySQL::MySQL(),
                   dbname='solarexport',
                   username=username,
                   password=password,
                   host='stolemarch.cskdhnmb36bo.us-east-1.rds.amazonaws.com',
                   port=3306)
  query <- sprintf("Select city from demand_high where state='%s' group by city", state)
  cities_frame <- dbGetQuery(con, query)
  dbDisconnect(con)

  cities <- cities_frame$city
  if (city %in% cities) {return ("Ok")}
  else {return (NULL)}
}

GetUtilityandRate <- function(state, city) {
  results <- PingRatesAPI(city, state) 
  utility <- results[1]
  rate <- results[2]
  utility_rate_summary <- sprintf("%s @ estimated retail rate of $%s/kWh.", utility, rate)
  return (utility_rate_summary)
}



################## Shiny #############


shinyServer(
  
  function(input, output, session) {

    ### Important data translations

    table_name <- reactive({
      if (input$load_profile == "Light") {"demand_low"}
      else if (input$load_profile == "Typical") {"demand_base"}
      else if (input$load_profile == "Heavy") {"demand_high"}
    })
    
    state <- reactive({state.abb[grep(input$state, state.name)]})
    
    output$city_ui <- renderUI({
      shiny::validate(need(state(), "Loading..."))
      selectizeInput("city", "Choose Nearest Location: ", GetCities(state()), multiple=F)
    })
    
    
    ### Collect all data after input requirements are satisfied
    
    city_frame <- reactive({
      shiny::validate(need(state(), "Loading..."))
      shiny::validate(need(input$city, "Loading..."))
      shiny::validate(need(input$system_capacity, "Loading..."))
      shiny::validate(need(input$load_profile, "Loading..."))
      shiny::validate(need(CheckInputs(state(),input$city), "Loading..."))
      CollectHourlyData(table_name(), state(), input$city, input$system_capacity, input$losses, input$azimuth, input$tilt, input$storage_limit)})
  

    ### Create summary frames by month and day

    city_month_frame <- reactive({

        city_frame() %>% group_by(month) %>%
        summarise(solar_export = sum(solar_export),
                  grid_usage = sum(grid_usage),
                  load = sum(load),
                  solar_gen = sum(solar_ac),
                  solar_onsite = sum(solar_onsite),
                  export_share_of_solar = solar_export / solar_gen)
    })
    
    city_day_frame <- reactive({city_frame() %>% filter(date == input$date)})


    ### Set graph and message colors 

    custom_colors <- setNames(c("#04B562","#0C6199","#57595C"), c("solar_onsite", "solar_export","grid_usage"))


    ### Primary graphs

    solar_gen_graph <- function() {
      solar_gen_frame <- city_month_frame()[,c("month","solar_onsite","solar_export")]
      solar_gen_melted <- melt(solar_gen_frame, id.vars = "month")
      solar_gen_melted$month_abb <- month.abb[solar_gen_melted$month]
      ggplot(aes(x=reorder(month_abb, -month), y=value, fill=variable), data=solar_gen_melted) +
        geom_bar(stat = "identity", width = 0.75) +
        coord_flip() +
        scale_fill_manual(values=custom_colors) +
        xlab("Month") +
        ylab("Electricity (kWh)") +
        ggtitle("Where the solar ends up: onsite versus export") +
        theme(legend.title=element_blank()) +
        theme(plot.title = element_text(size=18, face="bold", color="#4d3a7d", margin = margin(10, 0, 10, 0)))
    }
    
    load_graph <- function () {
      load_frame <- city_month_frame()[,c("month","solar_onsite","grid_usage")]
      load_melted <- melt(load_frame, id.vars = "month")
      load_melted$month_abb <- month.abb[load_melted$month]
      ggplot(aes(x=reorder(month_abb, -month), y=value, fill=variable), data=load_melted) +
        geom_bar(stat = "identity", width = 0.75) +
        coord_flip() +
        scale_fill_manual(values=custom_colors) +
        xlab("Month") +
        ylab("Electricity (kWh)") +
        ggtitle("Sources of electricity: solar versus grid") +
        theme(legend.title=element_blank()) +
        theme(plot.title = element_text(size=18, face="bold", color="#4d3a7d", margin = margin(10, 0, 10, 0))) 
        
    }
    
    nem_graph <- function() {
      nem_frame <- city_month_frame()[,c("month","grid_usage","solar_export")]
      nem_frame$grid_usage <- nem_frame$grid_usage * -1
      nem_melted <- melt(nem_frame, id.vars = "month")
      nem_melted$month_abb <- month.abb[nem_melted$month]
      ggplot(aes(x=reorder(month_abb, -month), y=value, fill=variable), 
             data=nem_melted) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() +
        scale_fill_manual(values=custom_colors) +
        xlab("Month") +
        ylab("Electricity (kWh)") +
        ggtitle("Net metering balance, by month") +
        theme(legend.title=element_blank()) +
        theme(plot.title = element_text(size=18, face="bold", color="#4d3a7d", margin = margin(10, 0, 10, 0)))
    }
    
    day_graph <- function() {
      ggplot(data=city_day_frame()) +
        geom_line(aes(x=hour, y=solar_ac, color="total_solar"), size=1.8) +
        geom_line(aes(x=hour, y=load, color="total_load"), size=1.8) +
        scale_colour_manual(name="", values = c(total_solar = "#EB8A0C", total_load = "#2A2936")) +
        xlab("Hour of Day") +
        ylab("Electricity (kWh)") +
        ggtitle("Total solar generation and total electric load, by hour") +
        theme(plot.title = element_text(size=18, face="bold", color="#4d3a7d", margin = margin(10, 0, 10, 0))) 
    }

    ### Connect graphs to shiny output
    
    output$solar_gen <- renderPlot({solar_gen_graph()})
    output$load <- renderPlot({load_graph()})
    output$nem <- renderPlot({nem_graph()})
    output$day <- renderPlot({day_graph()})

    ### Create app messages
    
    annual_export_share <- reactive({GetAnnualExportShare(city_month_frame())})
    net_grid_use <- reactive({GetNetGridUse(city_month_frame())})
    solar_share <- reactive({GetSolarShare(city_month_frame())})
    utility_rate_summary <- reactive({GetUtilityandRate(state(), input$city)})

    ### Connect messages to output
    
    output$annual_export_share_text <- renderText({annual_export_share()})
    output$net_grid_use_text <- renderText({net_grid_use()})
    output$solar_share_text <- renderText({solar_share()})
    output$utility_rate_summary_text <- renderText({utility_rate_summary()})
    output$total_electric_text <- renderText({GetTotalElectricUse(city_month_frame())})
    output$total_solar_text <- renderText({GetTotalSolarGen(city_month_frame())})

    ### CSV download

    output$download_data <- downloadHandler(
      filename = 'hourly_solar_and_load.csv',
      content = function(file) {
        write.csv(city_frame(), file, row.names=F)
      })
    
    ### Animation 

    onclick("toggle_advanced", toggle(id = "advanced", anim = TRUE))
    onclick("toggle_sources", toggle(id = "sources", anim = TRUE))
    onclick("toggle_hints", toggle(id = "hints", anim = TRUE))
  }
)


