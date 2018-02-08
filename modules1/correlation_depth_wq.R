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

CORRELATION_DEPTH_WQ_UI <- function(id) {
  
ns <- NS(id) # see General Note 1

tagList(
  wellPanel(
    fluidRow(
      column(3,
             # SITE
             wellPanel(
               checkboxGroupInput(ns("station"), "Station:", 
                                  choices=c("update me"),
                                  inline = TRUE),
               checkboxGroupInput(ns("level"), "Sampling Level:", 
                                  choices=c("update me"),
                                  inline = TRUE)
             )
      ), # end column
      column(3,
             # TEXT
             wellPanel(
               h3(textOutput(ns("text_num_null1")), align = "center"),
               h3(textOutput(ns("text_num_null2")), align = "center"),
               h4(textOutput(ns("text_num_text")), align = "center"),
               h3(textOutput(ns("text_num")), align = "center")
             ), # end Well Panel
             # DATE SELECTION
             wellPanel(
               uiOutput(ns("date_ui")) 
             ), # end Well Panel
             # MAP
             wellPanel(
               SITE_MAP_UI(ns("Site Map"))
             ) # end Well Panel
      ),# end Column
      column(6,
             fluidRow(
               column(6,
                      # Y Parameter Selection
                      wellPanel(
                        uiOutput(ns("y_param_ui")),
                        uiOutput(ns("y_range_ui"))
                      ) # well
               ), # end Column
               column(6,
                      wellPanel(
                        # X Parameter Selection
                        strong("X axis Parameter:"),
                        radioButtons(ns("x_option"), label = NULL, choices = c("Water Quality", "Meteorology or Hydrology"), inline = TRUE),
                        #See General Note 2
                        conditionalPanel(condition = paste0("input['", ns("x_option"), "'] == 'Water Quality' "),
                                         uiOutput(ns("x_param_ui")),
                                         uiOutput(ns("x_range_ui"))
                        ),# end Conditional Panel
                        conditionalPanel(condition = paste0("input['", ns("x_option"), "'] == 'Meteorology or Hydrology' "),
                                         selectInput(ns("x_met_param"), label = NULL, choices = c("Wind Speed", 
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
                                         sliderInput(ns("x_met_range"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
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
                        radioButtons(ns("met_option_1"), label = NULL, choices = c("off", "on"), inline = TRUE),
                        selectInput(ns("met_param_1"), label = NULL, choices = c("Wind Speed", 
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
                        sliderInput(ns("met_value_1"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                      )# end Well Panel
               ),# end Column
               column(4,
                      # MET/HYDRO FILTER 2
                      wellPanel(
                        strong("Meteoro/Hydro Filter 2"),
                        br(), br(),
                        radioButtons(ns("met_option_2"), label = NULL, 
                                     choices = c("off", "on"), inline = TRUE),
                        selectInput(ns("met_param_2"), label = NULL, 
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
                        sliderInput(ns("met_value_2"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                      )# end Well Panel
               ),# end Column
               column(4,
                      # MET/HYDRO FILTER 3
                      wellPanel(
                        strong("Meteoro/Hydro Filter 3"),
                        br(), br(),
                        radioButtons(ns("met_option_3"), label = NULL, 
                                     choices = c("off", "on"), 
                                     inline = TRUE),
                        selectInput(ns("met_param_3"), label = NULL, 
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
                        sliderInput(ns("met_value_3"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
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
             PLOT_CORR_DEPTH_WQ_UI(ns("Plot"))
    ), # end "plot" tabpanel
    
    # Table tabpanel
    tabPanel("Table",
             # first row - print button, etc
             fluidRow(
               br(),
               actionButton(ns("table_print"), "Print Table")
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

CORRELATION_DEPTH_WQ <- function(input, output, session, df, df_site) {
  
  # Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6
  
  parameters_non_historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
# Y axis Parameter
  
  #Parameter Selection UI
  
  output$y_param_ui <- renderUI({
    
    req(input$station, input$level) # See General Note _
    
    ns <- session$ns
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    y_param_choices_new <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Parameter %in% parameters_non_historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    y_param_choices_old <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             !(Parameter %in% parameters_non_historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    y_param_choices <- c(y_param_choices_new, y_param_choices_old)
    
    selectInput(ns("y_param"), "Y-axis Parameter:",        
                choices=c(y_param_choices))
    
  })
  
 # Y Parameter
  
 #  Reactive Text - Units of Parameter Selected (for Parameter Range Text)
  
  y_param_units <- reactive({ 
    df %>%
      filter(Parameter %in% input$param) %>%
      .$Units %>%
      factor() %>%
      levels()
  })
  
  #Parameter Value Range UI
  
  output$y_range_ui <- renderUI({
    
    req(input$station, input$level) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    y_result <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Parameter %in% input$y_param) %>%
      .$Result
    
    y_param_min <- y_result %>% min(na.rm=TRUE)
    
    y_param_max <- y_result %>% max(na.rm=TRUE)
    
    sliderInput(ns("y_range"), paste("Range (", y_param_units() ,")"),
                min = y_param_min, max = y_param_max,
                value = c(y_param_min, y_param_max))
    
  })
  
  
# X Parameter
  
  # Parameter Selection UI
  
  output$x_param_ui <- renderUI({
    
    req(input$station, input$level) # See General Note _
    
    ns <- session$ns
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    x_param_choices_new <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Parameter %in% parameters_non_historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    x_param_choices_old <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             !(Parameter %in% parameters_non_historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    x_param_choices <- c(x_param_choices_new, x_param_choices_old)
    
    selectInput(ns("x_param"), "X-axis Parameter:",        
                choices=c(x_param_choices))
    
  })
  
  # Reactive Text - Units of Parameter Selected (for Parameter Range Text)
  
  x_param_units <- reactive({ 
    df %>%
      filter(Parameter %in% input$param) %>%
      .$Units %>%
      factor() %>%
      levels() 
  })
  
# X Parameter Value Range UI
  
  output$x_range_ui <- renderUI({
    
    req(input$station, input$level) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    x_result <- df %>%
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Parameter %in% input$x_param) %>%
      .$Result
    
    x_param_min <- x_result %>% min(na.rm=TRUE)
    
    x_param_max <- x_result %>% max(na.rm=TRUE)
    
    sliderInput(ns("x_range"), paste("Range (", x_param_units() , ")"),
                min = x_param_min, max = x_param_max,
                value = c(x_param_min, x_param_max))
    
  })
  
# Date Selection UI
  
  output$date_ui <- renderUI({
    
    req(input$station, input$level) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    Dates <- df %>% 
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level)) %>%
      .$Date
    
    Date_min <- Dates %>% min(na.rm = TRUE)
    Date_max <- Dates %>% max(na.rm = TRUE)
    
    dateRangeInput(ns("date"), "Date Range:", 
                   start = Date_min, 
                   end = Date_max,
                   min = Date_min,
                   max = Date_max,
                   startview = "year")
    
  })
  
  
# Reactive Dataframe
  
  Df2 <- reactive({
    
    req(input$station, input$level, input$y_param, input$y_range, input$x_param, input$x_range, input$date) # See General Note 5
    
    # filter by location, depth, and Date adn save
    df_temp <- df %>% 
      filter(Station %in% c(input$station),
             Sampling_Level %in% c(input$level),
             Date > input$date[1], Date < input$date[2])
    
    # X Parameter filter and make modifications
    df_temp_x <-  df_temp %>% 
      filter(Parameter %in% c(input$x_param),
             Result > input$x_range[1], Result < input$x_range[2]) %>%
      rename(x_Parameter = Parameter, x_Result = Result) %>%
      select(Site, Station, Sampling_Level, Date, x_Parameter, x_Result)
    
    # Y Parameter filter and make modifications
    df_temp_y <-  df_temp %>% 
      filter(Parameter %in% c(input$y_param),
             Result > input$y_range[1], Result < input$y_range[2]) %>%
      rename(y_Parameter = Parameter, y_Result = Result) %>%
      select(Site, Station, Sampling_Level, Date, y_Parameter, y_Result)
    
    # Join the two X and Y parameters dataframes (Is Site redundant?)
    inner_join(df_temp_x, df_temp_y, by = c("Site", "Station", "Sampling_Level", "Date"))
    
  })
  
  
  # Text - Select Station
  
  output$text_num_null1 <- renderText({
    req(is.null(input$station)) # See General Note 1
    "Select a Station"
  })
  
  # Text - Select Depth
  
  output$text_num_null2 <- renderText({
    req(is.null(input$level)) # See General Note 1
    "Select a Depth"
  })
  
  # Text - Number of Samples
  
  output$text_num_text <- renderText({
    req(input$station, input$level) # See General Note 1
    "Number of Samples in Selected Data"
  })
  
  # Text - Number of Samples
  
  output$text_num <- renderText({
    req(input$station, input$level) # See General Note 1
    Df2() %>% summarise(n()) %>% paste()
  })

  # Plot
  
  callModule(PLOT_CORR_DEPTH_WQ, "Plot", Df = Df2)
  
  
  # Tables
  
  output$table <- renderDataTable(Df2())
  
  
  # Site Map
  # Combine Site Input
  
  site_list <- reactive({
    input$site
  })
  
  callModule(SITE_MAP, "Site Map", df_site = df_site, Site_List = site_list)

  
} # end Server Function
