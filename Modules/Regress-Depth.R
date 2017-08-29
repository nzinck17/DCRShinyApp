##############################################################################################################################
#     Title: Res-Nutrient-Regress.R
#     Type: Module for DCR Shiny App
#     Description: Regression plots, tables, and summary stats for Tributaries
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. 
#
# To-Do List:
#   1. Add Reactive Parameter Units to the Value Bars. This includes removing the units from the dataframe and
#      making a seperate column for the parameter and units
#   2. Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)

##############################################################################################################################
# User Interface
##############################################################################################################################

regress.depth.UI <- function(id, df) {
  
ns <- NS(id) # see General Note 1

tagList(
  wellPanel(
    fluidRow(
      column(3,
             # SITE
             wellPanel(
               checkboxGroupInput(ns("station"), "Station:", 
                                  choices=levels(factor(df$Station)),
                                  inline = TRUE),
               checkboxGroupInput(ns("level"), "Sampling Level:", 
                                  choices=levels(factor(df$Sampling_Level)),
                                  inline = TRUE)
             )
      ), # end column
      column(3,
             # TEXT
             wellPanel(
               h3(textOutput(ns("text.num.null1")), align = "center"),
               h3(textOutput(ns("text.num.null2")), align = "center"),
               h4(textOutput(ns("text.num.text")), align = "center"),
               h3(textOutput(ns("text.num")), align = "center")
             ), # end Well Panel
             # DATE SELECTION
             wellPanel(
               uiOutput(ns("date.ui")) 
             ), # end Well Panel
             # MAP
             wellPanel(
               sitemap.UI(ns("Site Map"))
             ) # end Well Panel
      ),# end Column
      column(6,
             fluidRow(
               column(6,
                      # Y Parameter Selection
                      wellPanel(
                        uiOutput(ns("y.param.ui")),
                        uiOutput(ns("y.range.ui"))
                      ) # well
               ), # end Column
               column(6,
                      wellPanel(
                        # X Parameter Selection
                        strong("X axis Parameter:"),
                        radioButtons(ns("x.option"), label = NULL, choices = c("Water Quality", "Meteorology or Hydrology"), inline = TRUE),
                        #See General Note 2
                        conditionalPanel(condition = paste0("input['", ns("x.option"), "'] == 'Water Quality' "),
                                         uiOutput(ns("x.param.ui")),
                                         uiOutput(ns("x.range.ui"))
                        ),# end Conditional Panel
                        conditionalPanel(condition = paste0("input['", ns("x.option"), "'] == 'Meteorology or Hydrology' "),
                                         selectInput(ns("x.met.param"), label = NULL, choices = c("Wind Speed", 
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
                                         sliderInput(ns("x.met.range"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                        )# end Conditional Panel
                      )# end Well Panel
               )
             ),# end fluid row
             hr(),
             br(),
             fluidRow(
               column(4,
                      wellPanel(
                        # MET/HYDRO FILTER 1
                        strong("Meteoro/Hydro Filter 1"),
                        br(), br(),
                        radioButtons(ns("met.option.1"), label = NULL, choices = c("off", "on"), inline = TRUE),
                        selectInput(ns("met.param.1"), label = NULL, choices = c("Wind Speed", 
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
                      )# end Well Panel
               ),# end Column
               column(4,
                      # MET/HYDRO FILTER 2
                      wellPanel(
                        strong("Meteoro/Hydro Filter 2"),
                        br(), br(),
                        radioButtons(ns("met.option.2"), label = NULL, 
                                     choices = c("off", "on"), inline = TRUE),
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
                      )# end Well Panel
               ),# end Column
               column(4,
                      # MET/HYDRO FILTER 3
                      wellPanel(
                        strong("Meteoro/Hydro Filter 3"),
                        br(), br(),
                        radioButtons(ns("met.option.3"), label = NULL, 
                                     choices = c("off", "on"), 
                                     inline = TRUE),
                        selectInput(ns("met.param.3"), label = NULL, 
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
                        sliderInput(ns("met.value.3"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                      )# end Well Panel
               )# end Column
             )# end Fluid Row
      )# end Column
    )# end Fluid Row
  ), # end Well panel
  # Tabset Panel for plots and tables 
  tabsetPanel(
    
    # the "Plot" tab panel
    tabPanel("Plot", 
             plot.regress.UI(ns("Plot"))
    ), # end "plot" tabpanel
    
    # Table tabpanel
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
    ) # end Tab panel - Table
  )  # end tabsetpanel - Plot and Table
) # end Taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

regress.depth <- function(input, output, session, df, df.site) {
  
  # Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6
  
  parameters.non.historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
# Y axis Parameter
  
  #Parameter Selection UI
  
  output$y.param.ui <- renderUI({
    
    req(input$station, input$level) # See General Note _
    
    ns <- session$ns
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    y.param.choices.new <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Parameter %in% parameters.non.historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    y.param.choices.old <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             !(Parameter %in% parameters.non.historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    y.param.choices <- c(y.param.choices.new, y.param.choices.old)
    
    selectInput(ns("y.param"), "Y-axis Parameter:",        
                choices=c(y.param.choices))
    
  })
  
 # Y Parameter
  
 #  Reactive Text - Units of Parameter Selected (for Parameter Range Text)
  
  y.param.units <- reactive({ 
    df %>%
      filter(Parameter %in% input$param) %>%
      .$Units %>%
      factor() %>%
      levels()
  })
  
  #Parameter Value Range UI
  
  output$y.range.ui <- renderUI({
    
    req(input$station, input$level) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    y.result <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Parameter %in% input$y.param) %>%
      .$Result
    
    y.param.min <- y.result %>% min(na.rm=TRUE)
    
    y.param.max <- y.result %>% max(na.rm=TRUE)
    
    sliderInput(ns("y.range"), paste("Range (", y.param.units() ,")"),
                min = y.param.min, max = y.param.max,
                value = c(y.param.min, y.param.max))
    
  })
  
  
# X Parameter
  
  # Parameter Selection UI
  
  output$x.param.ui <- renderUI({
    
    req(input$station, input$level) # See General Note _
    
    ns <- session$ns
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    x.param.choices.new <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Parameter %in% parameters.non.historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    x.param.choices.old <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             !(Parameter %in% parameters.non.historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    x.param.choices <- c(x.param.choices.new, x.param.choices.old)
    
    selectInput(ns("x.param"), "X-axis Parameter:",        
                choices=c(x.param.choices))
    
  })
  
  # Reactive Text - Units of Parameter Selected (for Parameter Range Text)
  
  x.param.units <- reactive({ 
    df %>%
      filter(Parameter %in% input$param) %>%
      .$Units %>%
      factor() %>%
      levels() 
  })
  
# X Parameter Value Range UI
  
  output$x.range.ui <- renderUI({
    
    req(input$station, input$level) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    x.result <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Parameter %in% input$x.param) %>%
      .$Result
    
    x.param.min <- x.result %>% min(na.rm=TRUE)
    
    x.param.max <- x.result %>% max(na.rm=TRUE)
    
    sliderInput(ns("x.range"), paste("Range (", x.param.units() , ")"),
                min = x.param.min, max = x.param.max,
                value = c(x.param.min, x.param.max))
    
  })
  
# Date Selection UI
  
  output$date.ui <- renderUI({
    
    req(input$station, input$level) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    Dates <- df %>% 
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level)) %>%
      .$Date
    
    Date.min <- Dates %>% min(na.rm = TRUE)
    Date.max <- Dates %>% max(na.rm = TRUE)
    
    dateRangeInput(ns("date"), "Date Range:", 
                   start = Date.min, 
                   end = Date.max,
                   min = Date.min,
                   max = Date.max,
                   startview = "year")
    
  })
  
  
# Reactive Dataframe
  
  df.react <- reactive({
    
    req(input$station, input$level, input$y.param, input$y.range, input$x.param, input$x.range, input$date) # See General Note 5
    
    # filter by location, depth, and Date adn save
    df.temp <- df %>% 
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Date > input$date[1], Date < input$date[2])
    
    # X Parameter filter and make modifications
    df.temp.x <-  df.temp %>% 
      filter(Parameter %in% c(input$x.param),
             Result > input$x.range[1], Result < input$x.range[2]) %>%
      rename(x.Parameter = Parameter, x.Result = Result) %>%
      select(Site, Station, Sampling_Level, Date, x.Parameter, x.Result)
    
    # Y Parameter filter and make modifications
    df.temp.y <-  df.temp %>% 
      filter(Parameter %in% c(input$y.param),
             Result > input$y.range[1], Result < input$y.range[2]) %>%
      rename(y.Parameter = Parameter, y.Result = Result) %>%
      select(Site, Station, Sampling_Level, Date, y.Parameter, y.Result)
    
    # Join the two X and Y parameters dataframes (Is Site redundant?)
    inner_join(df.temp.x, df.temp.y, by = c("Site", "Station", "Sampling_Level", "Date"))
    
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
  
  callModule(plot.regress.depth, "Plot", df = df.react)
  
  
  # Tables
  
  output$table <- renderDataTable(df.react())
  
  
  # Site Map
  # Combine Site Input
  
  site.list <- reactive({
    input$site
  })
  
  callModule(sitemap, "Site Map", df.site = df.site, site.list = site.list)

  
} # end Server Function
