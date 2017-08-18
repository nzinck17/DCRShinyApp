##############################################################################################################################
#     Title: Tributary-Time.R
#     Type: Module for DCR Shiny App
#     Description: Time Series plots, tables, and summary stats for Tributaries
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:
#   1. Make the Metero/Hydro Filters work
#   2. Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)

##############################################################################################################################
# User Interface
##############################################################################################################################

Trib.time.UI <- function(id, df) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    wellPanel(       
      fluidRow(
        column(4,
               # Site Selection
               wellPanel(
                 checkboxGroupInput(ns("site"), "Site Location:", 
                                    choices= levels(factor(df$Site)),
                                    inline=TRUE),
                 br(), br(),
               leafletOutput(ns("map"), height = 350 )
               ) # end Well Panel
        ), # end Column
        column(1),
        column(3,
               # Parameter Selection
               wellPanel(
                 uiOutput(ns("param.ui")),
                 uiOutput(ns("range.ui"))
               ), # end Well Panel
               br(),
               # Date Selection
               wellPanel(
                 uiOutput(ns("date.ui"))
               ), # end Well Panel
               br(),
               # Text - Number of Samples or "Select a site"
               wellPanel(
                 h2(textOutput(ns("text.num.null")), align = "center"),
                 h4(textOutput(ns("text.num.text")), align = "center"),
                 h3(textOutput(ns("text.num")), align = "center")
               ) # end Well Panel
        ),#col
        column(1),
        column(3,
               # Meteoro/Hydro Filter 1
               wellPanel(
                 strong("Meteoro/Hydro Filter 1"), # Bold Text
                 br(), br(),
                 radioButtons(ns("met.option.1"), label = NULL, 
                              choices = c("off", "on", "group"), 
                              inline = TRUE),
                 selectInput(ns("met.param.1"), label = NULL, 
                             choices = c("Wind Speed", 
                                         "Wind Direction", 
                                         "Precipitation - 24 hrs",
                                         "Precipitation - 48 hrs",
                                         "Temperature",
                                         "Cloud Cover",
                                         "Flow - Quabbin Aquaduct",
                                         "Flow - East Branch Swift",
                                         "Flow - West Branch Swift",
                                         "Flow - Quinapoxet",
                                         "Flow - Stillwater"),
                             selected = "Wind Speed"),
                 sliderInput(ns("met.value.1"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
               ), # end Well Panel
               # Meteoro/Hydro Filter 2
               wellPanel(
                 strong("Meteoro/Hydro Filter 2"), # Bold Text
                 br(), br(),
                 radioButtons(ns("met.option.2"), label = NULL, 
                              choices = c("off", "on", "group"), 
                              inline = TRUE),
                 selectInput(ns("met.param.2"), label = NULL, 
                             choices = c("Wind Speed", 
                                         "Wind Direction", 
                                         "Precipitation - 24 hrs",
                                         "Precipitation - 48 hrs",
                                         "Temperature",
                                         "Cloud Cover",
                                         "Flow - Quabbin Aquaduct",
                                         "Flow - East Branch Swift",
                                         "Flow - West Branch Swift",
                                         "Flow - Quinapoxet",
                                         "Flow - Stillwater"),
                             selected = "Precipitation - 24 hrs"),
                 sliderInput(ns("met.value.2"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
               ) # end Well Panel
        ) # end column
      ) # end fluidrow     
    ), # end well panel
    
    # Tabset panel for plots, tables, etc. 
    tabsetPanel(
      
      # Plot Tab
      tabPanel("Plot",
               plot.time.UI(ns("Plot Time"))
      ), # end Tab Panel - Plot
      
      # Table Tabpanel
      tabPanel("Table",
               fluidRow(br(),
                        br(),
                        actionButton(ns("table.print"), "Print Table")
               ), # end Fluid Row
               fluidRow(
                 dataTableOutput(ns("table"))
               ) # end Fluid Row
      ), # end Tab Panel - Table
      
      # Summary Tabpanel
      tabPanel("Summary",
               summary.UI(ns("Summary"))
      ) # end Tab Panel - Summary
    )  # end tabsetpanel (plots, stats, etc.)
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

Trib.time <- function(input, output, session, df, df.site) {
  
# Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6
  
  parameters.non.historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  
  
  # Parameter Selection UI
  
  output$param.ui <- renderUI({
    
    req(input$site) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    param.choices.new <- df %>%
      filter(Site %in% c(input$site)) %>%
      filter(Parameter %in% parameters.non.historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    param.choices.old <- df %>%
      filter(Site %in% c(input$site)) %>%
      filter(!(Parameter %in% parameters.non.historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    param.choices <- c(param.choices.new, param.choices.old)
    
    selectInput(ns("param"), "Parameter: ",
                choices=c(param.choices))
    
  })
  
  
# Units Texts for Selected Parameter
  
  param.units <- reactive({
    
    req(input$site) # See General Note _
    
    df %>%
      filter(Parameter %in% input$param) %>%
      .$Units %>%
      factor() %>%
      levels()
    
  })
  
  
# Parameter Value Range Bar UI
  
  output$range.ui <- renderUI({
    
    req(input$site) # See General Note _
    
    ns <- session$ns # see General Note 1
    
    result <- df %>%
      filter(Site %in% c(input$site)) %>%
      filter(Parameter %in% input$param) %>%
      .$Result
    
    param.min <- result %>% min(na.rm=TRUE)
    
    param.max <- result %>% max(na.rm=TRUE)
    
    sliderInput(ns("range"), paste("Range (", param.units() , ")"),
                min = param.min, max = param.max,
                value = c(param.min, param.max))
    
  })
  
  
# Date Selection UI
  
  output$date.ui <- renderUI({
    
    req(input$site) # See General Note _
    
    ns <- session$ns # see General Note 1
    
    Dates <- df %>% 
      filter(Site %in% c(input$site)) %>%
      .$Date
    
    Date.min <- Dates %>% min(na.rm=TRUE)
    Date.max <- Dates %>% max(na.rm=TRUE)
    
    # Date Input
    dateRangeInput(ns("date"), "Date Range:", 
                   start = Date.min, 
                   end = Date.max,  
                   min = Date.min,
                   max = Date.max,
                   startview = "year")
    
  })
  
  
# Reactive Dataframe - filter for selected site, param, value range, date, and remove rows with NA for Result
  
  df.react <- reactive({
    
    req(input$site) # See General Note _
    
    df %>% 
      filter(Site %in% c(input$site), 
             Parameter %in% input$param, 
             Result > input$range[1], Result < input$range[2],
             Date > input$date[1], Date < input$date[2],
             !is.na(Result))
  })
  

  
# Text - Select Site - Red
  
  output$text.num.null <- renderText({
    req(is.null(input$site)) # See General Note 1
    "Select a Site"
  })
  
  # Text - Number of Samples
  
  output$text.num.text <- renderText({
    req(input$site) # See General Note 1
    "Number of Samples in Selected Data"
  })
  
  # Text - Number of Samples
  
  output$text.num <- renderText({
    req(input$site) # See General Note 1
    df.react() %>% summarise(n()) %>% paste()
  })
  
# Plot
  
  callModule(plot.time, "Plot Time", df = df.react)
  
# Tables
  
  output$table <- renderDataTable(df.react())
  
# Summary Statistics

  callModule(summary, "Summary", df = df.react)
  
# Reactive Site Dataframe for Map Coloring, Creating a Selected Column with "Yes" or "No" values
  
  df.site.react <- reactive({
    df.temp <- df.site %>% filter(!is.na(LocationLat), !is.na(LocationLong))
    df.temp$Selected <- ifelse(df.temp$Site %in% input$site, "yes", "no")
    df.temp
  })
  
  
# Map Color Scheme - Coloring the Selected Site Markers a different color than the unselected 
  
  colorpal <- reactive({
    colorFactor(c("navy", "red"), domain = c("yes", "no"))
  })
  
  
# Base Leaflet Map - See General Note 3
  
  output$map <- renderLeaflet({
    
    leaflet(data = df.site %>% filter(!is.na(LocationLat), !is.na(LocationLong))) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(lng = ~LocationLong, lat = ~LocationLat,
                       label=~LocationLabel,
                       popup = ~paste("ID =", Site, "<br/>", 
                                      "Description =", LocationDescription, "<br/>",
                                      "Lat = ", LocationLat, "<br/>", 
                                      "Long = ", LocationLong, "<br/>",
                                      "Elev = ", LocationElevFt, "ft"),
                       radius = 5,
                       weight = 3,
                       opacity = 1,
                       fillOpacity = 0,
                       color = "navy")
    
  })
  
  
# Map Proxy - UPdate Color of Circle Markers as Site selection changes
  
  observe({
    
    pal <- colorpal()
    
    leafletProxy("map", data = df.site.react()) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = ~LocationLong, lat = ~LocationLat,
                       label=~LocationLabel,
                       popup = ~paste("ID =", Site, "<br/>", 
                                      "Description =", LocationDescription, "<br/>",
                                      "Lat = ", LocationLat, "<br/>", 
                                      "Long = ", LocationLong, "<br/>",
                                      "Elev = ", LocationElevFt, "ft"),
                       radius = 5,
                       weight = 3,
                       opacity = 1,
                       fillOpacity = 0,
                       color = ~pal(Selected))
    
  })
  
  
} # end Server Function

