##############################################################################################################################
#     Title: Tributary-Time.R
#     Type: Module for DCR Shiny App
#     Description: Time Series plots, tables, and summary stats for Tributaries
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   
#
# To-Do List:
#   2. Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)

##############################################################################################################################
# User Interface
##############################################################################################################################

TIME_WQ_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    wellPanel(       
      fluidRow(
        column(3,
               radioButtons(ns("df_choice"), "Full or Filtered Data:", 
                            choices = c("full", "filtered"),
                            inline = TRUE),
               p(textOutput(ns("text_filtered"))),
               wellPanel(
                 h4(textOutput(ns("text_site_null")), align = "center"),
                 h4(textOutput(ns("text_param_null")), align = "center"),
                 h4(textOutput(ns("text_date_null")), align = "center"),
                 h5(textOutput(ns("text_num_text")), align = "center"),
                 strong(textOutput(ns("text_num")), align = "center")
               ), # end Well Panel
               wellPanel(
                 SITE_MAP_UI(ns("site_map"))
               ) # end Well Panel
        ), # end Column
        column(6,
               SITE_CHECKBOX_UI(ns("site"))
        ), # end Column
        column(3,
               PARAM_SELECT_UI(ns("param")),
               br(),
               DATE_SELECT_UI(ns("date"))
        ) # end column
      ) # end fluidrow     
    ), # end well panel
    
    # Tabset panel for plots, tables, etc. 
    tabsetPanel(
      
      # Plot Tab
      tabPanel("Plot",
               h4(textOutput(ns("text_plot_no_data"))),
               h4(textOutput(ns("text_plot_zero_data"))),
               PLOT_TIME_WQ_UI(ns("plot"))
      ), # end Tab Panel - Plot
      
      # Table Tabpanel
      tabPanel("Table",
               h4(textOutput(ns("text_table_no_data"))),
               h4(textOutput(ns("text_table_zero_data"))),
               fluidRow(br(),
                        column(3,
                               downloadButton(ns("download_data"), "Download table as csv")
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
      tabPanel("Stats",
               h4(textOutput(ns("text_stats_no_data"))),
               h4(textOutput(ns("text_stats_zero_data"))),
               STAT_TIME_WQ_UI(ns("stats"))
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

TIME_WQ <- function(input, output, session, df_full, Df_Filtered, df_site) {
  
  ns <- session$ns # see General Note 1
  
  # Dataframe filtered or full based on Selection
  Df1 <- reactive({
    if(input$df_choice == "filtered"){
      Df_Filtered()
    }else{
      df_full
    }
  })
  
  
  # Site Selection using Site Select Module
  Site <- callModule(SITE_CHECKBOX, "site", Df = Df1)
  
  
  # Reactive Dataframe - first filter of the dataframe for Site
  Df2 <- reactive({
    req(Site())
    
    Df1() %>% 
      filter(LocationLabel %in% Site(),
             !is.na(Result)) # can remove this once certain no NAs. Maybe remove for Rdata files
  })
  
  
  # Parameter Selection using ParameterSelect Module
  Param <- callModule(PARAM_SELECT, "param", Df = Df2, Site = Site)
  

  #Date Range Selection Using DateSelect Module
  Date_Range <- callModule(DATE_SELECT, "date", Df = Df2, Site = Site)
  
  
  
  # Reactive Dataframe - filter for param, value range, date, and remove rows with NA for Result
  Df3 <- reactive({
    req(Param$Type(), Param$Range_Min(), Param$Range_Min(), Date_Range$Lower(), Date_Range$Upper()) # See General Note _
    
    Df2() %>% 
      filter(Parameter %in% Param$Type(), 
             Result > Param$Range_Min(), Result < Param$Range_Max(),
             Date > Date_Range$Lower(), Date < Date_Range$Upper())
  })
  
  
  # Plot
  callModule(PLOT_TIME_WQ, "plot", Df = Df3)
  
  
  # Table
  output$table <- renderDataTable(Df_Table())
  
  
  # Stats
  callModule(STAT_TIME_WQ, "stats", Df = Df3)
  
  
  # Site Map
  callModule(SITE_MAP, "site_map", df_site = df_site, Site_List = Site)
  
  
  ### Downloadable csv of selected dataset
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("DCRExportedWQData", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(Df_Table(), file)
    }
  )
  
  
  # Column Selection for table output
  output$column_ui <- renderUI({
    checkboxGroupInput(ns("column"), "Table Columns:", 
                       choices = names(Df3()), 
                       selected = names(Df3()),
                       inline = TRUE)
  })
  
  # Df for Table
  Df_Table <- reactive({
    req(input$column)
    Df3() %>% select(c(input$column))
  })

  
  ### Texts
  
  # Text - Filtered Data
  output$text_filtered <- renderText({
    req(input$df_choice == "filtered") # See General Note 1
    'This Dataset has been filtered and therefore some observations (data points) may be excluded.\n See "Filtered tab"'
  })
  
  # Text - Select Site
  output$text_site_null <- renderText({
    req(!isTruthy(Site())) # See General Note 1
    "Select Site(s)"
  })
  
  # Text - Select Param
  output$text_param_null <- renderText({
    req(!isTruthy(Param$Type())) # See General Note 1
    "Select Parameter"
  })
  
  # Text - Select Param
  output$text_date_null <- renderText({
    req(!isTruthy(Date_Range$Lower()) | !isTruthy(Date_Range$Upper())) # See General Note 1
    "Select Lower Date Range"
  })
  
  # Text - Number of Samples - Words
  output$text_num_text <- renderText({
    req(Df3()) # See General Note 1
    "Number of Samples in Selected Data:"
  })
  
  # Text - Number of Samples - Number
  output$text_num <- renderText({
    req(Df3()) # See General Note 1
    Df2() %>% summarise(n()) %>% paste()
  })
  
  # Text - Plot- No data Selected
  output$text_plot_no_data <- renderText({
    req(!isTruthy(Site()) | !isTruthy(Param$Type()) | !isTruthy(Date_Range$Lower()) | !isTruthy(Date_Range$Upper()))
    "Select Site, Param, and Date"
  })
  
  # Text - Plot - Zero data Selected for Plot
  output$text_plot_zero_data <- renderText({
    req(Df3()) # See General Note 1
    if(Df3() %>% summarise(n()) %>% unlist() == 0){
      "Selection Contains Zero Observations"
    }
  })
  
  # Text - Table - No data Selected
  output$text_table_no_data <- renderText({
    req(!isTruthy(Site()) | !isTruthy(Param$Type()) | !isTruthy(Date_Range$Lower()) | !isTruthy(Date_Range$Upper()))
    "Select Site, Param, and Date"
  })
  
  # Text - Table - Zero data Selected
  output$text_table_zero_data <- renderText({
    req(Df3()) # See General Note 1
    if(Df3() %>% summarise(n()) %>% unlist() == 0){
      "Selection Contains Zero Observations"
    }
  })
  
  # Text - Stats - No data Selected
  output$text_stats_no_data <- renderText({
    req(!isTruthy(Site()) | !isTruthy(Param$Type()) | !isTruthy(Date_Range$Lower()) | !isTruthy(Date_Range$Upper()))
    "Select Site, Param, and Date"
  })
  
  # Text - Stats - Zero Data Selected
  output$text_stats_zero_data <- renderText({
    req(Df3()) # See General Note 1
    if(Df3() %>% summarise(n()) %>% unlist() == 0){
      "Selection Contains Zero Observations"
    }
  })
  
  

  
} # end Server Function

