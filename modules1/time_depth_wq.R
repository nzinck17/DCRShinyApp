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
      column(4,
             radioButtons(ns("dfchoice"), "Full or Filtered Data:", 
                          choices = c("full", "filtered"),
                          inline = TRUE),
             conditionalPanel(
               condition = paste0("input['", ns("dfchoice"), "'] == 'filtered'"), 
               p('This Dataset has been filtered and therefore some observations (data points) may be excluded.\n See "Filtered tab"')
             ),
             uiOutput(ns("text.select")),
             wellPanel(
               sitemap.UI(ns("site.map"))
             ) # end Well Panel
      ), # end Column
      column(4,
             # Station and Level Selection
             uiOutput(ns("site.ui"))
      ), # end Column
      column(4,
             uiOutput(ns("param.ui")),
             br(),
             # Date Selection
             uiOutput(ns("date.ui"))
      ) # end column
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
             fluidRow(br(),
                      downloadButton(ns("downloadData"), "Download table as csv"),
                      br()
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

# Note that Argument "df.filtered"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

time.depth <- function(input, output, session, df.full, df.filtered, df.site) {
  
  ns <- session$ns # see General Note 1
  
  ### Dataframe filtered or full based on Selection
  
  df <- reactive({
    if(input$dfchoice == "filtered"){
      df.filtered()
    }else{
      df.full
    }
  })
  
  
  
  ### Site Selection using Site Select Module
  
  # Ui
  output$site.ui <- renderUI({
    station.level.checkbox.UI(ns("site"))
  })
  
  # Server
  site <- callModule(station.level.checkbox, "site", df = df)
  
  
  
  ### Parameter Selection using ParameterSelect Module
  
  # Ui
  output$param.ui <- renderUI({
    param.select.UI(ns("param"))
  })
  
  # Server
  param <- callModule(param.select, "param", df = df, site = site)
  
  
  
  ### Date Range Selection Using DateSelect Module
  
  # Ui
  output$date.ui <- renderUI({
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
  
  # Text Output
  output$text.select <- renderUI({
    # Text - Number of Samples or "Select a site"
    wellPanel(
      h5(textOutput(ns("text.site.null")), align = "center"),
      h5(textOutput(ns("text.param.null")), align = "center"),
      h5(textOutput(ns("text.date.null")), align = "center"),
      h5(textOutput(ns("text.num.text")), align = "center"),
      strong(textOutput(ns("text.num")), align = "center")
    ) # end Well Panel
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
  
  # Text - Select Param
  output$text.date.null <- renderText({
    req(any(is.null(date$lower()), is.null(date$upper()))) # See General Note 1
    "Select Lower Date Range"
  })
  
  # Text - Number of Samples - Words
  output$text.num.text <- renderText({
    req(site(), param$type()) # See General Note 1
    "Number of Samples in Selected Data:"
  })
  
  # Text - Number of Samples - Number
  output$text.num <- renderText({
    req(df.react()) # See General Note 1
    df.react() %>% summarise(n()) %>% paste()
  })
  
  
  
  ### Plot

  callModule(plot.time.depth, "plot", df = df.react)
  

  ### Table
  
  output$table <- renderDataTable(df.react())
  
  
  ### Summary Statistics
  
  callModule(summary.depth, "summary", df = df.react)
  
  
  ### Site Map
  
  callModule(sitemap, "site.map", df.site = df.site, site.list = site)
  
  
  ### Downloadable csv of selected dataset
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("DCRExportedWQData", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(df.react(), file)
    }
  )
  
} # end server

