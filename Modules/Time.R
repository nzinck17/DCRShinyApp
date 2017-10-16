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

time.UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    wellPanel(       
      fluidRow(
        column(3,
               uiOutput(ns("site.ui"))
        ), # end Column
        column(3,
               # Text - Number of Samples or "Select a site"
               wellPanel(
                 h3(textOutput(ns("text.site.null")), align = "center"),
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
               plot.time.UI(ns("plot"))
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
               summary.UI(ns("summary"))
      ) # end Tab Panel - Summary
    )  # end tabsetpanel (plots, stats, etc.)
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

time <- function(input, output, session, df, df.site) {
  
  
  # Site Selection using Site Select Module
  
  # Ui
  output$site.ui <- renderUI({
    ns <- session$ns # see General Note 1
    site.checkbox.UI(ns("site"), df = df)
  })
  
  # Server
  site <- callModule(site.checkbox, "site", df = df)
  
  
  # Parameter Selection using ParameterSelect Module
  
  # Ui
  output$param.ui <- renderUI({
    ns <- session$ns # see General Note 1
    param.select.UI(ns("param"))
  })
  
  # Server
  param <- callModule(param.select, "param", df = df, site = site)
  
  
  
  # Date Range Selection Using DateSelect Module
  
  # Ui
  output$date.ui <- renderUI({
    ns <- session$ns # see General Note 1
    date.select.UI(ns("date"))
  })
  
  # Server
  date <- callModule(date.select, "date", df = df, site = site)
  
  
  
  # Reactive Dataframe - filter for selected site, param, value range, date, and remove rows with NA for Result
  
  df.react <- reactive({
    
    req(site(), param$type(), param$range.min(), param$range.min(), date$lower(), date$upper()) # See General Note _
    
    df %>% 
      filter(LocationLabel %in% site(), 
             Parameter %in% param$type(), 
             Result > param$range.min(), Result < param$range.max(),
             Date > date$lower(), Date < date$upper(),
             !is.na(Result))
  })
  

  
  # Text - Select Site
  
  output$text.site.null <- renderText({
    req(is.null(site())) # See General Note 1
    "Select Site(s)"
  })
  
  # Text - Select Param
  
  output$text.param.null <- renderText({
    req(param$type() == "") # See General Note 1
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
  
  callModule(plot.time, "plot", df = df.react)
  
  
# Table
  
  output$table <- renderDataTable(df.react())
  
  
# Summary Statistics

  callModule(summary, "summary", df = df.react)
  
  
# Site Map
  
  callModule(sitemap, "site.map", df.site = df.site, site.list = site)
  
} # end Server Function

