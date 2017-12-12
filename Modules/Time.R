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
        column(6,
               uiOutput(ns("site.ui"))
        ), # end Column
        column(3,
               uiOutput(ns("param.ui")),
               br(),
               # Date Selection
               uiOutput(ns("date.ui"))
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
                        column(3,
                               downloadButton(ns("downloadData"), "Download table as csv")
                        ),
                        column(9,
                               uiOutput(ns("column_ui"))
                        ),
                        br()
               ), # end Fluid Row
               fluidRow(
                 dataTableOutput(ns("table"))
               ) # end Fluid Row
      ), # end Tab Panel - Table
      
      # Summary Tabpanel
      tabPanel("Summary",
               summary.UI(ns("summary"))
      ) # end Tab Panel - Summary
    ) # end tabsetpanel (plots, stats, etc.)
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "df.filtered"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

time <- function(input, output, session, df.full, df.filtered, df.site) {
  
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
    site.checkbox.UI(ns("site"))
  })
  
  # Server
  site <- callModule(site.checkbox, "site", df = df)
  
  
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
    
    df() %>% 
      filter(LocationLabel %in% site(), 
             Parameter %in% param$type(), 
             Result > param$range.min(), Result < param$range.max(),
             Date > date$lower(), Date < date$upper(),
             !is.na(Result))
  })
  
  
  ### Column Selection for table output
  
  output$column_ui <- renderUI({
    checkboxGroupInput(ns("column"), "Table Columns:", 
                       choices = names(df.react()), 
                       selected = names(df.react()),
                       inline = TRUE)
  })
  
  # Column Selection
  df.react.table <- reactive({
    df.react() %>% select(c(input$column))
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
  
  callModule(plot.time, "plot", df = df.react)
  
  
  ### Table
  
  output$table <- renderDataTable(df.react.table())
  
  
  ### Summary Statistics

  callModule(summary, "summary", df = df.react)
  
  
  ### Site Map
  
  callModule(sitemap, "site.map", df.site = df.site, site.list = site)
  
  
  ### Downloadable csv of selected dataset
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("DCRExportedWQData", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(df.react.table(), file)
    }
  )
  
} # end Server Function

