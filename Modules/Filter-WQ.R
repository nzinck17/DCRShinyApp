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

filter.wq.UI <- function(id) {
  
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
                 checkboxSelectAll.UI(ns("month"))
               ), # end Well Panel
               wellPanel(
                 # Year
                 selectInputSelectAll.UI(ns("year"))
               ) # end Well Panel
        ), # end Column
        column(4,
               # Flag Selection
               wellPanel(
                 selectInputSelectAll.UI(ns("flag"))
               ), # end Well Panel
               # storm Sample Selection
               wellPanel(
                 checkboxSelectAll.UI(ns("storm"))
               ), # end Well Panel
               # Text - Number of Samples or "Select a site"
               wellPanel(
                 h5(textOutput(ns("text.no.month"))),
                 h5(textOutput(ns("text.no.year"))),
                 h5(textOutput(ns("text.no.flag"))),
                 h5(textOutput(ns("text.no.storm")))
               ) # end Well Panel
        ), # end column
        column(4,
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
    ) # end well panel
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# This module does not take any reactive expressions. Changes will have to be made to accmodate reactive expressions
# dfs is a list of dataframes

filter.wq <- function(input, output, session, dfs, col) { 

  
  
  ### Month Selection
  
  # server
  month.input <- callModule(checkboxSelectAll, "month",
                            label = "Months:",
                            choices = reactive({month.name}), 
                            selected = reactive({month.name}), 
                            colwidth = 3,
                            hidden = TRUE,
                            inline = TRUE)
  
  
  
  ### Year Selection
  
  # Choices
  year.choices <- c(rev(year(seq(as.Date("1990-1-1"), Sys.Date(), "years"))))
  
  # Server
  year.input <- callModule(selectInputSelectAll, "year", 
                           label = "Years:", 
                           choices = reactive({year.choices}), 
                           selected = reactive({year.choices}), 
                           colwidth = 3,
                           hidden = TRUE)
  
  
  

  
  
  
  ### Flag Selection

  # Choices
  flag.choices <- dfs[[1]]$FlagCode %>% factor() %>% levels()

  # Server
  flag <- callModule(selectInputSelectAll, "flag",
                     label = "Flags:",
                     choices = reactive({flag.choices}),
                     selected = reactive({flag.choices}),
                     colwidth = 3,
                     hidden = TRUE)




  ### Storm Selection

  # Choices
  storm.choices <- dfs[[1]]$StormSample %>% factor() %>% levels()

  # Server
  storm <- callModule(checkboxSelectAll, "storm",
                      label = "Storm Sample:",
                      choices = reactive({storm.choices}),
                      selected = reactive({storm.choices}),
                      hidden = TRUE)
  
  
  

  
  
  
  ### Reactive List of (non-reactive) Dataframes - filter for selected site, param, value range, date, and remove rows with NA for Result
  
  df.react.list <- reactive({
    
    req(input$date, month.input(), year.input()) #, flag(), storm() # See General Note _
    
    dfs %>% lapply(. %>% filter(Date > input$date[1], Date < input$date[2],
                                as.character(month(Date, label = TRUE, abbr = FALSE)) %in% month.input(),
                                year(Date) %in% year.input(),
                                #FlagCode %in% flag(),
                                #StormSample %in% storm(),
                                !is.na(Result)))
    
  })
  
   
  
  ### Texts
  
  output$text.no.month <- renderText({
    req(is.null(month.input()))
    "- Please Select Months"
  })
  
  output$text.no.year <- renderText({
    req(is.null(year.input()))
    "- Please Select Years"
  })
  
  output$text.no.flag <- renderText({
    req(is.null(flag()))
    "- Please Select Flag Types"
  })

  output$text.no.storm <- renderText({
    req(is.null(storm()))
    "- Please Select Storm Sample Types"
  })


  ### Return List of Reactive Dataframes
  # a Reactive List Expression is converted to a (Non-reactive) List of Reactive Expressions
  
  return(list(reactive({df.react.list()[[1]]}),
              reactive({df.react.list()[[2]]}),
              reactive({df.react.list()[[3]]})))
  
  

  
  
} # end Server Function

