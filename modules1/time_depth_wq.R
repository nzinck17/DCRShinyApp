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

TIME_DEPTH_WQ_UI <- function(id) {
  
ns <- NS(id)

tagList(
  wellPanel(
    fluidRow(
      column(4,
             radioButtons(ns("df_choice"), "Full or Filtered Data:", 
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
      column(4,
             # Station and Level Selection
             uiOutput(ns("site_ui"))
      ), # end Column
      column(4,
             uiOutput(ns("param_ui")),
             br(),
             # Date Selection
             uiOutput(ns("date_ui"))
      ) # end column
    ) # end Fluid Row
  ), # Well Panel
  
  # Tabset panel for plots, tables, Summary Stats
  tabsetPanel(
    
    # Plot tab
    tabPanel("Plot", 
             PLOT_TIME_DEPTH_WQ_UI(ns("plot"))
    ),
    
    # Table Tab
    tabPanel("Table",
             # first row - print button, etc
             fluidRow(br(),
                      downloadButton(ns("download_data"), "Download table as csv"),
                      br()
             ),
             # next row
             fluidRow(
               br(), br(),
               dataTableOutput(ns("table"))
             ) # end fluid row
    ), # end tabpanel
    
    tabPanel("Summary",
             STAT_TIME_DEPTH_WQ_UI(ns("stat"))
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

TIME_DEPTH_WQ <- function(input, output, session, df_full, Df_Filtered, df_site) {
  
  ns <- session$ns # see General Note 1
  
  ### Dataframe filtered or full based on Selection
  
  Df1 <- reactive({
    if(input$df_choice == "filtered"){
      Df_Filtered()
    }else{
      df_full
    }
  })
  
  
  
  ### Site Selection using Site Select Module
  
  # Ui
  output$site_ui <- renderUI({
    STATION_LEVEL_CHECKBOX_UI(ns("site"))
  })
  
  # Server
  Site <- callModule(STATION_LEVEL_CHECKBOX, "site", Df = Df1)
  
  
  
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
  Date <- callModule(DATE_SELECT, "date", Df = Df1, Site = Site)
  
  
  
  
  ### Reactive Dataframe - filter for selected site, param, value range, date, and remove rows with NA for Result
  
  Df2 <- reactive({
    
    req(Site(), Param$Type(), Param$Range_Min(), Param$Range_Min(), Date$Lower(), Date$Upper()) # See General Note _
    
    Df1() %>% 
      filter(LocationLabel %in% Site(), 
             Parameter %in% Param$Type(), 
             Result > Param$Range_Min(), Result < Param$Range_Max(),
             Date > Date$Lower(), Date < Date$Upper(),
             !is.na(Result))
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
    req(is.null(Site())) # See General Note 1
    "Select Site(s)"
  })
  
  # Text - Select Param
  output$text_param_null <- renderText({
    req(Param$Type() == "") # See General Note 1
    "Select Parameter"
  })
  
  # Text - Select Param
  output$text_date_null <- renderText({
    req(any(is.null(Date$Lower()), is.null(Date$Upper()))) # See General Note 1
    "Select Lower Date Range"
  })
  
  # Text - Number of Samples - Words
  output$text_num_text <- renderText({
    req(Site(), Param$Type()) # See General Note 1
    "Number of Samples in Selected Data:"
  })
  
  # Text - Number of Samples - Number
  output$text_num <- renderText({
    req(Df2()) # See General Note 1
    Df2() %>% summarise(n()) %>% paste()
  })
  
  
  
  ### Plot

  callModule(PLOT_TIME_DEPTH_WQ, "plot", Df = Df2)
  

  ### Table
  
  output$table <- renderDataTable(Df2())
  
  
  ### Summary Statistics
  
  callModule(STAT_TIME_DEPTH_WQ, "stat", Df = Df2)
  
  
  ### Site Map
  
  callModule(SITE_MAP, "site_map", df_site = df_site, Site_List = Site)
  
  
  ### Downloadable csv of selected dataset
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("DCRExportedWQData", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(Df2(), file)
    }
  )
  
} # end server

