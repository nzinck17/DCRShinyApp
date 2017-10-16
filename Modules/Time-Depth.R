##############################################################################################################################
#     Title: Res-Nutrient-Time.R
#     Type: Module for DCR Shiny App
#     Description: Time Series plots, tables, and summary stats for Reservoir Data
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. 
#
# To-Do List:
#   1. Add Reactive Parameter Units to the Value Bars. This includes removing the units from the dataframe and
#      making a seperate column for the parameter and units
#   2. Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)
#   3. Fix Map to show reservoir locations
#   4. Get data from Database
#   5. Get the location/site/station terminology straight
#   6. Split into transect and nutrient

##############################################################################################################################
# User Interface
##############################################################################################################################

time.depth.UI <- function(id) {
  
ns <- NS(id)

tagList(
  wellPanel(
    fluidRow(
      column(3,
             # Site Selection
             wellPanel(
               uiOutput(ns("station.ui"))
             ),
             # Sampling Level
             wellPanel(
               uiOutput(ns("level.ui"))
             ) # end Well Panel
      ), # end Column
      column(3,
             # Text - Number of Samples or "Select a site"
             wellPanel(
               h3(textOutput(ns("text.station.null")), align = "center"),
               h3(textOutput(ns("text.level.null")), align = "center"),
               h3(textOutput(ns("text.param.null")), align = "center"),
               h4(textOutput(ns("text.num.text")), align = "center"),
               h3(textOutput(ns("text.num")), align = "center")
             ), # end Well Panel
             wellPanel(
               sitemap.UI(ns("site.map"))
             ) # end Well Panel
      ), # end Column
      column(3,
             uiOutput(ns("param.ui")),
             br(),
             # Date Selection
             uiOutput(ns("date.ui"))
      ), # end column
      column(3,
             # Meteoro/Hydro Filter 1
             wellPanel(
               strong("Meteoro/Hydro Filter 1"),
               br(), br(),
               radioButtons(ns("met.option.1"), label = NULL, 
                            choices = c("off", "on"), 
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
             ),
             # Meteoro/Hydro Filter 2
             wellPanel(
               strong("Meteoro/Hydro Filter 2"),
               br(), br(),
               radioButtons(ns("met.option.2"), label = NULL, 
                            choices = c("off", "on"), 
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
      ) # end Column
    ) # end Fluid Row
  ), # Well Panel
  
  # Tabset panel for plots, tables, Summary Stats
  tabsetPanel(
    
    # Plot tab
    tabPanel("Plot", 
             plot.time.depth.UI(ns("plot"))
    ),
    
    # Table Tab
    tabPanel("Table",
             # first row - print button, etc
             fluidRow(
               br(),
               actionButton(ns("table.print"), "Print Table")
             ),
             # next row
             fluidRow(
               br(), br(),
               dataTableOutput(ns("table"))
             ) # end fluid row
    ), # end tabpanel
    
    tabPanel("Summary",
             summary.depth.UI(ns("Summary"))
    ) # end Tab Panel - Summary
  ) # end tabset panel
) # end taglist
} # end UI


##############################################################################################################################
# Server Function
##############################################################################################################################

time.depth <- function(input, output, session, df, df.site) {
  
  
  
  ### Station
  
  # Choices
  station.choices <- df$Station %>% factor() %>% levels()
  
  # UI
  output$station.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("station"), "Station:", choices = station.choices) #, inline = TRUE
  })
  
  # Server
  station <- callModule(checkboxSelectAll, "station", choices = station.choices)
  
  
  
  ### Sampling Level
  
  # Choices
  level.choices <- df$Sampling_Level %>% factor() %>% levels()
  
  # UI
  output$level.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("level"), "Sampling Level:", choices = level.choices) #, inline = TRUE
  })
  
  # Server
  level <- callModule(checkboxSelectAll, "level", choices = level.choices)

  
  
  ### Site List 
  site <- reactive({
    #req(input$station, input$level)
    req(station(), level())
    
    df %>% 
      #filter(Station %in% input$station,
      #       Sampling_Level %in% input$level) %>%
      filter(Station %in% station(),
             Sampling_Level %in% level()) %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
  })
  
  
  
  
  ### Parameter Selection using ParameterSelect Module
  
  # Ui
  output$param.ui <- renderUI({
    ns <- session$ns # see General Note 1
    param.select.UI(ns("param"))
  })
  
  # Server
  param <- callModule(param.select, "param", df = df, site = site)
  
  
  
  
  ### Date Range Selection Using DateSelect Module
  
  # Ui
  output$date.ui <- renderUI({
    ns <- session$ns # see General Note 1
    date.select.UI(ns("date"))
  })
  
  # Server
  date <- callModule(date.select, "date", df = df, site = site)
  
  
  
  
  ### Reactive Dataframe - filter for selected site, param, value range, date, and remove rows with NA for Result
  
  df.react <- reactive({
    
    req(site(), param$type(), param$range.min(), param$range.min(), date$lower(), date$upper()) # See General Note _
    
    df %>% 
      filter(LocationLabel %in% site(), 
             Parameter %in% param$type(), 
             Result > param$range.min(), Result < param$range.max(),
             Date > date$lower(), Date < date$upper(),
             !is.na(Result))
  })
  
  
  
  
  ### Texts 
  
  # Text - Select Station
  
  output$text.station.null <- renderText({
    req(is.null(station())) # See General Note 1
    "Select Station"
  })
  
  # Text - Select Sampling Level
  
  output$text.level.null <- renderText({
    req(is.null(level())) # See General Note 1
    "Select Sampling Level"
  })
  
  # Text - Select Param
  
  output$text.param.null <- renderText({
    req(is.null(param$type())) # See General Note 1
    "Select Parameter"
  })
  
  # Text - Number of Samples
  
  output$text.num.text <- renderText({
    req(site(), param$type()) # See General Note 1
    "Number of Samples in Selected Data:"
  })
  
  # Text - Number of Samples
  
  output$text.num <- renderText({
    req(df.react()) # See General Note 1
    df.react() %>% summarise(n()) %>% paste()
  })
  
  
  # Plot

  callModule(plot.time.depth, "plot", df = df.react)
  

# Table
  
  output$table <- renderDataTable(df.react())
  
  
# Summary Statistics
  
  callModule(summary.depth, "summary", df = df.react)
  
  
# Site Map
  
  callModule(sitemap, "site.map", df.site = df.site, site.list = site)
  
  
} # end server
