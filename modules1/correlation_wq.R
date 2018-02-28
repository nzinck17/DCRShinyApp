##############################################################################################################################
#     Title: Tributary-Regression.R
#     Type: Module for DCR Shiny App
#     Description: Regression plots and tables
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   

##############################################################################################################################
# User Interface
##############################################################################################################################

CORRELATION_WQ_UI <- function(id) {

ns <- NS(id)

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
    tabPanel("Scatter Plot",
             h4(textOutput(ns("text_plot1_no_data"))),
             h4(textOutput(ns("text_plot1_zero_data"))),
             PLOT_CORR_WQ_UI(ns("plot_scatter"))
    ), # end Tab Panel - Plot
    
    # Summary Tabpanel
    tabPanel("Correlation Matrix",
             h4(textOutput(ns("text_plot2_no_data"))),
             h4(textOutput(ns("text_plot2_zero_data"))),
             PLOT_CORR_MATRIX_WQ_UI(ns("plot_matrix"))
    ), # end Tab Panel - Summary
    
    # Table Tabpanel
    tabPanel("Table",
             h4(textOutput(ns("text_table_no_data"))),
             h4(textOutput(ns("text_table_zero_data"))),
             fluidRow(br(),
                      column(3,
                             radioButtons(ns("table_format"), "Table Format",
                                          choices = c("long", "wide")),
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
    ) # end Tab Panel - Table
    
  ) # end tabsetpanel (plots, stats, etc.)
) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

CORRELATION_WQ <- function(input, output, session, df_full, Df_Filtered, df_site) {

  
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
  
  
  # Date Range Selection Using DateSelect Module
  Date_Range <- callModule(DATE_SELECT, "date", Df = Df2, Site = Site)
  
  
  
  # Reactive Dataframe - filter for param, value range, date, and remove rows with NA for Result
  Df3 <- reactive({
    req(Param$Type(), Param$Range_Min(), Param$Range_Min(), Date_Range$Lower(), Date_Range$Upper()) # See General Note _
    
    Df2() %>% 
      filter(Parameter %in% Param$Type(), 
             Result > Param$Range_Min(), Result < Param$Range_Max(),
             Date > Date_Range$Lower(), Date < Date_Range$Upper())
  })
  
  # Reactive Dataframe - Wide Format (for Correlation Matrix and for option in table )
  Df4 <- reactive({
    Df3() %>%
      select(-Units) %>%
      distinct(LocationLabel, Date, Parameter, .keep_all = TRUE) %>%
      spread("Parameter", "Result")
  })
  
  
  # Plot - Scatter
  callModule(PLOT_CORR_WQ, "plot_scatter", Df = Df3)
  
  # Plot - Scatter
  callModule(PLOT_CORR_MATRIX_WQ, "plot_matrix", Df = Df4)
  
  # Table
  output$table <- renderDataTable(Df_Table())
  
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
    if(input$table_format == "long"){
      df <- Df3()
    } else{
      df <- Df4()
    }
    checkboxGroupInput(ns("column"), "Table Columns:", 
                       choices = names(df), 
                       selected = names(df),
                       inline = TRUE)
  })
  
  # Df for Table
  Df_Table <- reactive({
    req(input$column)
    if(input$table_format == "long"){
      Df3() %>% select(c(input$column))
    } else{
      Df4() %>% select(c(input$column))
    }
  })
  
  
################ Texts ###################################
  
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
    Df3() %>% summarise(n()) %>% paste()
  })
  
  # Text - Plot1- No data Selected
  output$text_plot1_no_data <- renderText({
    req(!isTruthy(Site()) | !isTruthy(Param$Type()) | !isTruthy(Date_Range$Lower()) | !isTruthy(Date_Range$Upper()))
    "Select Site, Param, and Date"
  })
  
  # Text - Plot1 - Zero data Selected for Plot
  output$text_plot1_zero_data <- renderText({
    req(Df3()) # See General Note 1
    if(Df3() %>% summarise(n()) %>% unlist() == 0){
      "Selection Contains Zero Observations"
    }
  })
  
  # Text - Plot 2 - No data Selected
  output$text_plot2_no_data <- renderText({
    req(!isTruthy(Site()) | !isTruthy(Param$Type()) | !isTruthy(Date_Range$Lower()) | !isTruthy(Date_Range$Upper()))
    "Select Site, Param, and Date"
  })
  
  # Text - Plot 2 - Zero Data Selected
  output$text_plot2_zero_data <- renderText({
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
  


  
} # end Server Function

