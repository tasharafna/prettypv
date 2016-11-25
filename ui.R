library(shiny)
library(httr)
library(RMySQL)
library(shinyjs)

source("creds.R")

GetStates <- function() {
  con <- dbConnect(RMySQL::MySQL(),
                   dbname='solarexport',
                   username=username,
                   password=password,
                   host='stolemarch.cskdhnmb36bo.us-east-1.rds.amazonaws.com',
                   port=3306)
  query <- sprintf("Select state from demand_base group by state")
  selected_frame <- dbGetQuery(con, query)
  dbDisconnect(con)
  return (selected_frame$state)
}


################# Shiny ########### 

shinyUI(
  
  fluidPage(

    ### Header

    tags$head(includeScript("google-analytics.js")),
    
    useShinyjs(),
    
    headerPanel(h1("Home Solar Export Estimator: twist the dials on a typical year", style = "font-family: 'Garamond'; color: #4d3a7d;"), windowTitle = "Pretty PV"),

    helpText("Many US states & power utilities are ", a("proposing changes to long-standing net metering programs.", href="http://climatenexus.org/net-metering-fight-understanding-latest-issue-nation%E2%80%99s-rapidly-changing-electricity-market"), "The timing of onsite solar generation and onsite power use is becoming much more important. Home Solar Export Estimator sums up hour-by-hour data from ", a("NREL",href="http://www.nrel.gov/"), " to illuminate the tradeoff between *minimizing* solar exports to the grid & *maximizing* solar coverage of household electricity use."),
   
   ### Sidebar

    sidebarLayout(
      
      sidebarPanel(
        helpText("***Twist dials here***"),
                
        tags$head(tags$style("#total_solar_text{color:#EB8A0C;font-size: 12px;font-style: bold;}")),
        tags$head(tags$style("#total_electric_text{color:#2A2936;font-size: 12px;font-style: bold;}")),
        textOutput("total_solar_text"),
        textOutput("total_electric_text"),

        helpText("     "),
        
        selectizeInput("state", "Enter State: ", state.name, multiple=F),
        
        uiOutput("city_ui"),

        helpText("     "),

        numericInput("system_capacity", "Solar array size (kW): ", min=1, max=25, value=6),
        radioButtons("load_profile", "Type of electricity user: ", choices=c("Light","Typical","Heavy"), selected="Typical"),

        helpText("     "),
        
        a(id = "toggle_advanced", "Advanced Options:"),
        hidden(
          div(id = "advanced",
              sliderInput("azimuth", "Azimuth (degrees): ", min=0, max=360, value=180),
              sliderInput("tilt", "Tilt (degrees): ", min=0, max=45, value=20),
              sliderInput("losses", "PV System Losses (%): ", min=0, max=25, value=14),
              sliderInput("storage_limit", "Battery Storage (kWh): ", min=0, max=30, value=0)
          )
        ),
        
        helpText("*****"),


        a(id = "toggle_hints", "Hints:"),
        hidden(
          div(id = "hints",
            helpText("Try reducing solar array size and pointing the panels west (azimuth = 270 degrees) or south-west (azimuth = 225 degrees). Use the 'Single Day' tab to confirm how closely solar generation matches with household demand. Or for a more dramatic impact, add battery storage.")
            )
          ),

        helpText("*****"),
        
        a(id = "toggle_sources", "Sources:"),
        hidden(
          div(id = "sources",
              helpText("Data Sources: "),
              helpText(a("Load profiles (NREL OpenEI).", href="http://en.openei.org/doe-opendata/dataset/commercial-and-residential-hourly-load-profiles-for-all-tmy3-locations-in-the-united-states")),
              helpText(a("Solar generation (NREL PVWatts TMY3).", href="https://developer.nrel.gov/docs/solar/pvwatts-v5/")),
              helpText("Home Solar Export Estimator is a simple wrapper that combines two data sources: 1) Estimated hourly solar generation from NREL PVWatts at ~1000 U.S. locations based on PV system specs and irradidance from a 'typical meterological year', 2) Simulated hourly electricity use profiles (low, base, high) for each location based on generic household consumption patterns adjusted for local climate, also developed by NREL. Solar generation assumes 'standard' module type and 'fixed (roof-mount)' array type. Battery storage is assumed to have a 100% round-trip efficiency."),
              helpText("Want more detail on your specific home? Try ",a("Google Project Sunroof", href="https://www.google.com/get/sunroof#p=0")),
              helpText("- Built by ", a("Nick Culver", href="https://twitter.com/solarrec")," (2016)")
          )
        ),

      helpText("*****"),

      downloadButton("download_data","CSV"),
        
        width = 3
      ),

      ### Main

      mainPanel(

        helpText("***View results here***"),
  
        tabsetPanel(
          tabPanel("Solar export", 
                   tags$head(tags$style("#annual_export_share_text{color: #0C6199;font-size: 18px;font-style: bold}")),
                   plotOutput("solar_gen"), 
                   textOutput("annual_export_share_text")),
          tabPanel("Solar usage", 
                   tags$head(tags$style("#solar_share_text{color: #04B562;font-size: 18px;font-style: bold}")),
                   plotOutput("load"),
                   textOutput("solar_share_text")),
          tabPanel("Net Metering", 
                  tags$head(tags$style("#net_grid_use_text{font-size: 18px;font-style: bold}")),
                  plotOutput("nem"),
                  textOutput("net_grid_use_text")
                  # textOutput("utility_rate_summary_text")
                  ),
          tabPanel("Single Day",
                   plotOutput("day"),
                   sliderInput("date", "Select Date: ", min=as.Date('2016-01-01'), 
                               max=as.Date('2016-12-31'), value=as.Date('2016-01-01'),
                               timeFormat="%F"))
          
        )
      )
      
    )
    
  )
)