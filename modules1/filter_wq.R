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
    tabsetPanel(
      tabPanel("Select / Filter Data",
               wellPanel(
                 fluidRow(h3("Main Selections", align = "center")),
                 fluidRow(
                   column(3,
                          wellPanel(
                            h4(textOutput(ns("text_site_null")), align = "center"),
                            h4(textOutput(ns("text_param_null")), align = "center"),
                            h4(textOutput(ns("text_date_null")), align = "center"),
                            h4(textOutput(ns("text_no_month"))),
                            h5(textOutput(ns("text_num_text")), align = "center"),
                            strong(textOutput(ns("text_num")), align = "center")
                          ), # end Well Panel
                          wellPanel(
                            SITE_MAP_UI(ns("site_map"))
                          ) # end Well Panel
                   ), # end Column
                   column(5,
                          uiOutput(ns("site_ui"))
                   ), # end Column
                   column(4,
                          # Parameter INput - Using Module Parameter Select
                          PARAM_SELECT_UI(ns("param")),
                          br(),
                          wellPanel(
                            # Date Input - Using Module Date Select
                            DATE_SELECT_UI(ns("date")),
                            # Year Input - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
                            SELECT_SELECT_ALL_UI(ns("year"))
                          ),
                          br(),
                          wellPanel(
                            # Month Input - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
                            CHECKBOX_SELECT_ALL_UI(ns("month"))
                          )
                   ) # end column
                 ) # end fluidrow
               ), # end well panel
               # Advanced Filters
               wellPanel(
                 fluidRow(h3("Advanced Filters", align = "center")),
                 fluidRow(
                   column(4,
                          # Flag Selection
                          wellPanel(
                            # Flag Input - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
                            SELECT_SELECT_ALL_UI(ns("flag"))
                          ),
                          # storm Sample Selection
                          wellPanel(
                            strong("Storm Samples:"), # Bold Text
                            checkboxInput(ns("storm"),
                                          label =  "Include Storm Samples",
                                          value = TRUE),
                            checkboxInput(ns("nonstorm"),
                                          label =  "Include Non-Storm Samples",
                                          value = TRUE)

                          ), # end Well Panel
                          uiOutput(ns("depth_ui")),
                          # Text - Number of Samples or "Select a site"
                          wellPanel(
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
                          ) # end Well Panel
                   ),
                   column(4,
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
      ),
      tabPanel("View Data in Table",
               fluidRow(br(), downloadButton(ns("download_data"), "Download table as csv"), align = "center"),
               dataTableOutput(ns("table"))
      )
    )
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# This module does not take any reactive expressions. Changes will have to be made to accmodate reactive expressions
# dfs is a list of dataframes

FILTER_WQ <- function(input, output, session, df, df_site, df_flags = NULL, df_flag_sample_index = NULL, type = "wq"){

# Types include: "wq", "wq_depth", and "profile". More can be added
########################################################
# Main Selection

  ns <- session$ns # see General Note 1

  # Site Map
  callModule(SITE_MAP, "site_map", df_site = df_site, Site_List = Site)


  ### Site Selection

  # Display sites w/o depths OR sites w/ Depths
  output$site_ui <- renderUI({
    if(type == "wq" | type == "profile"){
      SITE_CHECKBOX_UI(ns("site"))
    } else if(type == "wq_depth"){
      STATION_LEVEL_CHECKBOX_UI(ns("site"))
    }
  })

  # Site Selection using Site Select Module
  Site <- if(type == "wq" | type == "profile"){
    callModule(SITE_CHECKBOX, "site", df = df)
  } else if(type == "wq_depth"){
    callModule(STATION_LEVEL_CHECKBOX, "site", df = df)
  }


  # Reactive Dataframe - first filter of the dataframe for Site
  Df1 <- reactive({
    # A Site must be selected in order for Df1 (or anything that uses Df1()) to be executed
    req(Site())
    
    df %>% filter(LocationLabel %in% Site())

  })


  ### Parameter and Date Range

  # Parameter Selection using Param_Select Module
  Param <- callModule(PARAM_SELECT, "param", Df = Df1)

  # Date Range and Year Using Date_Select Module
  Date_Year <- callModule(DATE_SELECT, "date", Df = Df1)
  
  # Month Selection
  Month <- callModule(CHECKBOX_SELECT_ALL, "month",
                      label = "Months:",
                      Choices = reactive({month.name}),
                      Selected = reactive({month.name}),
                      colwidth = 3,
                      inline = TRUE)
  

  # Reactive Dataframe - filter for param, value range, date, and remove rows with NA for Result
  Df2 <- reactive({
    # Wait for all neccesary Inputs to Proceed
    req(Param$Type(), Param$Range_Min(), Param$Range_Min(), Month(),
        (isTruthy(Date_Year$Lower()) & isTruthy(Date_Year$Upper())) | isTruthy(Date_Year$Years())) # See General Note _

    Df1() %>%
      # filter by parameter, parameter value range, and by date range
      filter(Parameter %in% Param$Type(),
             # Filter by Result Range
             Result > Param$Range_Min(), Result < Param$Range_Max(),
             # Filter by either Date Range or By Years (Include both)
             (Date > Date_Year$Lower() & Date < Date_Year$Upper()) | year(Date) %in% Date_Year$Years(),
             # Filter by Month
             as.character(month(Date, label = TRUE, abbr = FALSE)) %in% Month())
             
  })


  ### Texts

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
    req((!isTruthy(Date_Year$Lower()) | !isTruthy(Date_Year$Upper())) & !isTruthy(Date_Year$Years()) ) # See General Note 1
    "Select Date Range or Years"
  })
  
  # Text - Select Month
  output$text_no_month <- renderText({
    req(is.null(Month()))
    "- Select Months"
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

##################################################
# Advanced Filter


  ### Flag Selection

  # Choices
  flag_choices <- df_flags$label[df_flags$Flag_ID != 114]

  # server - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
  Flag <- callModule(SELECT_SELECT_ALL, "flag",
                     label = "Select flag(s) to EXCLUDE from the data:",
                     Choices = reactive({df_flags$label}),
                     colwidth = 3)

  # Subset the Sample Flag Index by the flags selected to exclude - this results in a vector of IDs to filter out
  flagged_ids <- reactive({
    df_flag_sample_index %>%
      filter(FlagCode %in% as.numeric(substr(Flag(),1, 3))) %>%
      .$SampleID
  })

  
  ### Storm Sample Selection

  # Filter df_flag_sample_index so that only flag 114 (Storm Sample Flag) are included
  storm_ids <- reactive({
    df_flag_sample_index %>%
      filter(FlagCode == 114) %>%
      .$SampleID
  })

  
  # ### Depth Filter (Profile)
  
  # UI
  output$depth_ui <- renderUI({
    if(type == "profile"){
      max_depth <- max(df$Depthm)
      tagList(
        wellPanel(
          sliderInput(ns("depth"),"Depth Range", min = 0, max = max_depth, value = c(0, max_depth))
        )
      )
    }
  })
  

  ### Reactive List of (non-reactive) Dataframes - filter for selected site, param, value range, date, and remove rows with NA for Result

  Df3 <- reactive({

    # Assign a temporary dataframe and filter NAs
    df_temp <- Df2() %>% filter(!is.na(Result))

    # filter out Selected Flags
    if(isTruthy(Flag()) & isTruthy(df_flag_sample_index)){
      df_temp <- df_temp %>% filter(!(ID %in% flagged_ids()))
    }

    # filter out Storm Samples if unchecked
    if(input$storm != TRUE & isTruthy(df_flag_sample_index)){
      df_temp <- df_temp %>% filter(!(ID %in% storm_ids()))
    }

    # filter out Non Storm Samples if unchecked
    if(input$nonstorm != TRUE & isTruthy(df_flag_sample_index)){
      df_temp <- df_temp %>% filter(ID %in% storm_ids())
    }
    
    # filter out Depth for Profile Data
    if(isTruthy(input$depth)){
      df_temp <- df_temp %>% 
        filter(Depthm >= input$depth[1],
               Depthm <= input$depth[2])
    }

    df_temp

  })


  ### Texts - Tell the user to Select a Input type when none is chosen but neccessary
  

  # Text - Select Storm Sample Types when none are selected
  output$text_no_storm <- renderText({
    req(input$storm == FALSE & input$nonstorm == FALSE)
    "- Please Select Storm Sample Types"
  })


  ### Return List of Reactive Dataframes
  # a Reactive List Expression is converted to a (Non-reactive) List of Reactive Expressions


  # Reactive Dataframe - Wide Format (for Correlation ScatterPlot and Correlation Matrix)
  Df3_Wide <- reactive({
    # require Dataframe to be more than zero observations - prevent from crashing
    req(Df3() %>% summarise(n()) %>% unlist() != 0)
    Df3() %>%
      # Need to get rid of Units column to properly Spread the data due to discrepencies in Units
      select(-Units) %>%
      # Should verify no duplicate records and then remove this dinstinct code line
      distinct(LocationLabel, Date, Parameter, .keep_all = TRUE) %>%
      # Spread parameters to each have their own row (wide format)
      spread("Parameter", "Result")
  })

  # Reactive Dataframe - Adding Columns for Year, Season, and Month for grouping purposesin some modules
  Df3_Stat <- reactive({
    Df3() %>%
      mutate(Year = factor(lubridate::year(Date)),
             Season = getSeason(Date),
             Month = month.abb[lubridate::month(Date)])
  })


#####################################################
# CSV output and Table

  # render Datatable
  output$table <- renderDataTable(Df3(), selection = 'none')

  # Downloadable csv of selected dataset
  output$download_data <- downloadHandler(
    filename = function() {
      paste("DCRExportedWQData", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(Df3(), file)
    }
  )

#####################################################
# Return from Module a list of reactive dataframes.

  return(list(Long = Df3,
              Wide = Df3_Wide,
              Stat = Df3_Stat))

} # end Server Function

