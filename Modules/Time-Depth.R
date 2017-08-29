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

time.depth.UI <- function(id, df) {
  
ns <- NS(id)

tagList(
  wellPanel(
    fluidRow(
      column(3,
             # Location and Depth Selection with Map
             wellPanel(
               checkboxGroupInput(ns("station"), "Station:", 
                                  choices=levels(factor(df$Station)),
                                  inline = TRUE),
               checkboxGroupInput(ns("level"), "Sampling Level:", 
                                  choices=levels(factor(df$Sampling_Level)),
                                  inline = TRUE),
               br(),
               sitemap.UI(ns("Site Map"))
             ) # end Well Panel
      ),
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
             br(), br(), br(),
             # Number of Samples
             wellPanel(
               h3(textOutput(ns("text.num.null1")), align = "center"),
               h3(textOutput(ns("text.num.null2")), align = "center"),
               h4(textOutput(ns("text.num.text")), align = "center"),
               h3(textOutput(ns("text.num")), align = "center")
             ) # end well Panel
      ), # end Column
      column(1),
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
             plot.time.depth.UI(ns("Plot"))
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
  
  
  # Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6
  
  parameters.non.historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  
  # Parameter Selection UI
  
  output$param.ui <- renderUI({
    
    req(input$station, input$level) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    param.choices.new <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Parameter %in% parameters.non.historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    param.choices.old <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             !(Parameter %in% parameters.non.historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    param.choices <- c(param.choices.new, param.choices.old)
    
    selectInput(ns("param"), "Parameter: ",
                choices=c(param.choices))
    
  })
  
 # Reactive Text - Units of Parameter Selected (for Parameter Range Text)
  
  param.units <- reactive({ 
    df %>%
      filter(Parameter %in% input$param) %>%
      .$Units %>%
      factor() %>%
      levels()
    
  })
  
#Parameter Value Range UI
  
  output$range.ui <- renderUI({
    
    req(input$station, input$level) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    result <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Parameter %in% input$param) %>%
      .$Result
    
    param.min <- result %>% min(na.rm=TRUE)
    
    param.max <- result %>% max(na.rm=TRUE)
    
    sliderInput(ns("range"), label = paste("Range (", param.units(), ")"), 
                min = param.min, max = param.max,
                value = c(param.min, param.max))
    
  })
  
  
# Date Selection UI
  
  output$date.ui <- renderUI({
    
    req(input$station, input$level) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    Dates <- df %>% 
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level)) %>%
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
  
# Reactive Dataframe
  
  df.react <- reactive({
    
    req(input$station, input$level, input$param, input$range, input$date) # See General Note 5
    
    df %>% 
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Parameter %in% c(input$param),
             Result > input$range[1], Result < input$range[2],
             Date > input$date[1], Date < input$date[2])

  })
  
  
  # Text - Select Station
  
  output$text.num.null1 <- renderText({
    req(is.null(input$station)) # See General Note 1
    "Select a Station"
  })
  
  # Text - Select Depth
  
  output$text.num.null2 <- renderText({
    req(is.null(input$level)) # See General Note 1
    "Select a Depth"
  })
  
  # Text - Number of Samples
  
  output$text.num.text <- renderText({
    req(input$station, input$level) # See General Note 1
    "Number of Samples in Selected Data"
  })
  
  # Text - Number of Samples
  
  output$text.num <- renderText({
    req(input$station, input$level) # See General Note 1
    df.react() %>% summarise(n()) %>% paste()
  })
  
# Plot

callModule(plot.time.depth, "Plot", df = df.react)
  
# Create Table
  
  output$table <- renderDataTable(df.react())
  
# Create Summary
  
  callModule(summary.depth, "Summary", df = df.react)
  
  
  # Site Map
  # Combine Site Input
  
  site.list <- reactive({
    input$site
  })
  
  callModule(sitemap, "Site Map", df.site = df.site, site.list = site.list)
  
} # end server
