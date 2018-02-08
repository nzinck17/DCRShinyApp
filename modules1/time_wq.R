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

TIME_WQ_UI <- function(id) {
  
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
               uiOutput(ns("text_select")),
               wellPanel(
                 SITE_MAP_UI(ns("site_map"))
               ) # end Well Panel
        ), # end Column
        column(6,
               uiOutput(ns("site_ui"))
        ), # end Column
        column(3,
               uiOutput(ns("param_ui")),
               br(),
               # Date Selection
               uiOutput(ns("date_ui"))
        ) # end column
      ) # end fluidrow     
    ), # end well panel
    
    # Tabset panel for plots, tables, etc. 
    tabsetPanel(
      
      # Plot Tab
      tabPanel("Plot",
               PLOT_TIME_WQ_UI(ns("plot"))
      ), # end Tab Panel - Plot
      
      # Table Tabpanel
      tabPanel("Table",
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
      tabPanel("Summary",
               STAT_TIME_WQ_UI(ns("summary"))
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
  
  ### Dataframe filtered or full based on Selection
  Df1 <- reactive({
    if(input$dfchoice == "filtered"){
      Df_Filtered()
    }else{
      df_full
    }
  })
  
  ### Site Selection using Site Select Module
  
  # Ui
  output$site_ui <- renderUI({
    SITE_CHECKBOX_UI(ns("site"))
  })
  
  # Server
  Site <- callModule(SITE_CHECKBOX, "site", Df = Df1)
  
  
  ### Parameter Selection using ParameterSelect Module
  
  # Ui
  output$param_ui <- renderUI({
    PARAM_SELECT_UI(ns("param"))
  })
  
  # Server
  Param <- callModule(PARAM_SELECT, "param", Df = Df1, Site = Site)
  
  
  
  ### Date Range Selection Using DateSelect Module
  
  # Ui
  output$date_ui <- renderUI({
    DATE_SELECT_UI(ns("date"))
  })
  
  # Server
  Date_Range <- callModule(DATE_SELECT, "date", Df = Df1, Site = Site)
  
  
  
  ### Reactive Dataframe - filter for selected site, param, value range, date, and remove rows with NA for Result
  
  Df2 <- reactive({
    
    req(Site(), Param$Type(), Param$Range_Min(), Param$Range_Min(), Date_Range$Lower(), Date_Range$Upper()) # See General Note _
    
    Df1() %>% 
      filter(LocationLabel %in% Site(), 
             Parameter %in% Param$Type(), 
             Result > Param$Range_Min(), Result < Param$Range_Max(),
             Date > Date_Range$Lower(), Date < Date_Range$Upper(),
             !is.na(Result))
  })
  
  
  ### Column Selection for table output
  
  output$column_ui <- renderUI({
    checkboxGroupInput(ns("column"), "Table Columns:", 
                       choices = names(Df2()), 
                       selected = names(Df2()),
                       inline = TRUE)
  })
  
  # Column Selection
  Df_Table <- reactive({
    Df2() %>% select(c(input$column))
  })

  
  ### Texts
  
  # Text Output
  output$text_select <- renderUI({
    # Text - Number of Samples or "Select a site"
    wellPanel(
      h5(textOutput(ns("text_site_null")), align = "center"),
      h5(textOutput(ns("text_param_null")), align = "center"),
      h5(textOutput(ns("text_date_null")), align = "center"),
      h5(textOutput(ns("text_num_text")), align = "center"),
      strong(textOutput(ns("text_num")), align = "center")
    ) # end Well Panel
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
    req(Df2()) # See General Note 1
    "Number of Samples in Selected Data:"
  })
  
  # Text - Number of Samples - Number
  output$text_num <- renderText({
    req(Df2()) # See General Note 1
    Df2() %>% summarise(n()) %>% paste()
  })
  
  
  
  ### Plot
  
  callModule(PLOT_TIME_WQ, "plot", Df = Df2)
  
  
  ### Table
  
  output$table <- renderDataTable(Df_Table())
  
  
  ### Summary Statistics

  callModule(STAT_TIME_WQ, "summary", Df = Df2)
  
  
  ### Site Map
  
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
  
} # end Server Function

