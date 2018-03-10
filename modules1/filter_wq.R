##############################################################################################################################
#     Title: Export-WQ.R
#     Type: Module for DCR Shiny App
#     Description: Filter and Export Water Quality Data
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

FILTER_WQ_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    wellPanel(
      fluidRow(
        column(4,
               # Date Selection
               wellPanel(
                 # Date Range
                 dateRangeInput(ns("date"), "Date Range:", 
                                start = as.Date("2010-1-1"), 
                                end = Sys.Date(),  
                                min = as.Date("1990-1-1"),
                                max = Sys.Date(),
                                startview = "year")
               ), # end Well Panel
               wellPanel(
                 # Month
                 CHECKBOX_SELECT_ALL_UI(ns("month"))
               ), # end Well Panel
               wellPanel(
                 # Year
                 SELECT_SELECT_ALL_UI(ns("year"))
               ) # end Well Panel
        ), # end Column
        column(4,
               # Flag Selection
               wellPanel(
                 SELECT_SELECT_ALL_UI(ns("flag"))
               ), # end Well Panel
               # storm Sample Selection
               wellPanel(
                 CHECKBOX_SELECT_ALL_UI(ns("storm"))
               ), # end Well Panel
               # Text - Number of Samples or "Select a site"
               wellPanel(
                 h5(textOutput(ns("text_no_month"))),
                 h5(textOutput(ns("text_no_year"))),
                 h5(textOutput(ns("text_no_flag"))),
                 h5(textOutput(ns("text_no_storm")))
               ) # end Well Panel
        ), # end column
        column(4,
               # Meteoro/Hydro Filter 1
               wellPanel(
                 strong("Meteoro/Hydro Filter 1"), # Bold Text
                 br(), br(),
                 radioButtons(ns("met_option_1"), label = NULL, 
                              choices = c("off", "on", "group"), 
                              inline = TRUE),
                 selectInput(ns("met_param_1"), label = NULL, 
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
                 sliderInput(ns("met_value_1"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
               ), # end Well Panel
               # Meteoro/Hydro Filter 2
               wellPanel(
                 strong("Meteoro/Hydro Filter 2"), # Bold Text
                 br(), br(),
                 radioButtons(ns("met_option_2"), label = NULL, 
                              choices = c("off", "on", "group"), 
                              inline = TRUE),
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
               ) # end Well Panel
        ) # end column
      ) # end fluidrow     
    ) # end well panel
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# This module does not take any reactive expressions. Changes will have to be made to accmodate reactive expressions
# dfs is a list of dataframes

FILTER_WQ <- function(input, output, session, dfs, col) { 

  ### Month Selection
  
  # server
  Month <- callModule(CHECKBOX_SELECT_ALL, "month",
                            label = "Months:",
                            choices = reactive({month.name}), 
                            selected = reactive({month.name}), 
                            colwidth = 3,
                            hidden = TRUE,
                            inline = TRUE)
  
  
  ### Year Selection
  
  # Choices
  year_choices <- c(rev(year(seq(as.Date("1980-01-01"), Sys.Date(), "years")))) # Change to first year of data
  
  # Server
  Year <- callModule(SELECT_SELECT_ALL, "year", 
                           label = "Years:", 
                           choices = reactive({year_choices}), 
                           selected = reactive({year_choices}), 
                           colwidth = 3,
                           hidden = TRUE)
  
  
  ### Flag Selection

  # Choices
  flag_choices <- dfs[[1]]$FlagCode %>% factor() %>% levels()

  # Server
  Flag <- callModule(SELECT_SELECT_ALL, "flag",
                     label = "Flags:",
                     choices = reactive({flag_choices}),
                     selected = reactive({flag_choices}),
                     colwidth = 3,
                     hidden = TRUE)


  ### Storm Selection

  # Choices
  storm_choices <- dfs[[1]]$StormSample %>% factor() %>% levels()

  # Server
  Storm <- callModule(CHECKBOX_SELECT_ALL, "storm",
                      label = "Storm Sample:",
                      choices = reactive({storm_choices}),
                      selected = reactive({storm_choices}),
                      hidden = TRUE)
  
  
  ### Reactive List of (non-reactive) Dataframes - filter for selected site, param, value range, date, and remove rows with NA for Result
  
  Df_List <- reactive({
    
    req(input$date, Month(), Year()) #, Flag(), Storm() # See General Note _
    
    dfs %>% lapply(. %>% filter(Date > input$date[1], Date < input$date[2],
                                as.character(month(Date, label = TRUE, abbr = FALSE)) %in% Month(),
                                year(Date) %in% Year(),
                                #FlagCode %in% Flag(),
                                #StormSample %in% Storm(),
                                !is.na(Result)))
  })
  
   
  ### Texts
  
  output$text_no_month <- renderText({
    req(is.null(Month()))
    "- Please Select Months"
  })
  
  output$text_no_year <- renderText({
    req(is.null(Year()))
    "- Please Select Years"
  })
  
  output$text_no_flag <- renderText({
    req(is.null(Flag()))
    "- Please Select Flag Types"
  })

  output$text_no_storm <- renderText({
    req(is.null(Storm()))
    "- Please Select Storm Sample Types"
  })


  ### Return List of Reactive Dataframes
  # a Reactive List Expression is converted to a (Non-reactive) List of Reactive Expressions
  
  return(list(reactive({Df_List()[[1]]}),
              reactive({Df_List()[[2]]}),
              reactive({Df_List()[[3]]})))
  
  
} # end Server Function

