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
        column(2,
               downloadButton(ns("download_data"), "Download table as csv")
        ),
        column(10,
               checkboxInput(ns("wide"), 'transform to "wide" data format (Caution should be taken)')
        )
      )
    ),
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
                            h5(textOutput(ns("text_num_text")), align = "center"),
                            strong(textOutput(ns("text_num")), align = "center")
                          ), # end Well Panel
                          wellPanel(
                            SITE_MAP_UI(ns("site_map"))
                          ) # end Well Panel
                   ), # end Column
                   column(6,
                          uiOutput(ns("site_ui"))
                   ), # end Column
                   column(3,
                          PARAM_SELECT_UI(ns("param")),
                          br(),
                          DATE_SELECT_UI(ns("date"))
                   ) # end column
                 ) # end fluidrow
               ), # end well panel
               # Advanced Filters
               wellPanel(
                 fluidRow(h3("Advanced Filters", align = "center")),
                 fluidRow(
                   column(4,
                          # Date Selection
                          wellPanel(
                            # Month Input - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
                            CHECKBOX_SELECT_ALL_UI(ns("month"))
                          ),
                          wellPanel(
                            # Year Input - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
                            SELECT_SELECT_ALL_UI(ns("year"))
                          )
                   ), # end Column
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
                          # Text - Number of Samples or "Select a site"
                          wellPanel(
                            h5(textOutput(ns("text_no_month"))),
                            h5(textOutput(ns("text_no_year"))),
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
      ),
      tabPanel("View Data in Table",
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
    if(type == "wq"){
      SITE_CHECKBOX_UI(ns("site"))
    } else if(type == "wq_depth"){
      STATION_LEVEL_CHECKBOX_UI(ns("site"))
    } else if(type == "profile"){
      SITE_PROFILE_UI(ns("site"))
    }
  })

  # Site Selection using Site Select Module
  Site <- reactive({
    if(type == "wq"){
      Sites <- callModule(SITE_CHECKBOX, "site", Df = reactive({df}))
      Sites()
    } else if(type == "wq_depth"){
      Sites <- callModule(STATION_LEVEL_CHECKBOX, "site", Df = reactive({df}))()
      Sites()
    } else if(type == "profile"){
      # It is useful to have Site choices all named Site, even for profile, due to "req(Site())" in Df1
      Site_Depth()$Sites
    }
  })

  # Profile Only - Site and Depth selection
  Site_Depth <- if(type == "profile"){
    # Returns a named list with 2 elements: vector of Locations and vector of lower and upper depth
    callModule(SITE_PROFILE, "site", Df = reactive({df}))
  }


  # Reactive Dataframe - first filter of the dataframe for Site
  Df1 <- reactive({
    # A Site must be selected in order for Df1 (or anything that uses Df1()) to be executed
    # Note how this does not require depth for the Profile Data.
    # It is hard to include this becuase there is no depth for the other types of data
    req(Site())

    if(type == "wq" | type == "wq_depth"){
      # Filter WQ and WQ_depth data for Site
      df %>%
        filter(LocationLabel %in% Site())

    } else if(type == "profile"){
      # FIlter Profile Data for Site and Depths
      df %>%
        filter(LocationLabel %in% Site(),
               Depthm >= Site_Depth()$Depths[1],
               Depthm <= Site_Depth()$Depths[2])
    }
  })


  ### Parameter and Date Range

  # Parameter Selection using Param_Select Module
  Param <- callModule(PARAM_SELECT, "param", Df = Df1)

  # Date Range Selection Using Date_Select Module
  Date_Range <- callModule(DATE_SELECT, "date", Df = Df1)

  # Reactive Dataframe - filter for param, value range, date, and remove rows with NA for Result
  Df2 <- reactive({
    req(Param$Type(), Param$Range_Min(), Param$Range_Min(), Date_Range$Lower(), Date_Range$Upper()) # See General Note _

    Df1() %>%
      # filter by parameter, parameter value range, and by date range
      filter(Parameter %in% Param$Type(),
             Result > Param$Range_Min(), Result < Param$Range_Max(),
             Date > Date_Range$Lower(), Date < Date_Range$Upper())
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





##################################################
# Advanced Filter

  ### Month Selection

  # server - Using the custom Module CHECKBOX_SELECT_ALL, see script of dev manual
  Month <- callModule(CHECKBOX_SELECT_ALL, "month",
                            label = "Months:",
                            choices = reactive({month.name}),
                            selected = reactive({month.name}),
                            colwidth = 3,
                            inline = TRUE)


  ### Year Selection

  # Choices
  year_choices <- c(rev(year(seq(as.Date("1980-01-01"), Sys.Date(), "years")))) # Change to first year of data

  # server - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
  Year <- callModule(SELECT_SELECT_ALL, "year",
                           label = "Years:",
                           choices = reactive({year_choices}),
                           selected = reactive({year_choices}),
                           colwidth = 3)


  ### Flag Selection

  # Choices
  flag_choices <- df_flags$label[df_flags$Flag_ID != 114]

  # server - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
  Flag <- callModule(SELECT_SELECT_ALL, "flag",
                     label = "Select flag(s) to EXCLUDE from the data:",
                     choices = reactive({df_flags$label}),
                     colwidth = 3)



  # Subset the Sample Flag Index by the flags selected to exclude - this results in a vector of IDs to filter out
  flagged_ids <- reactive({
    df_flag_sample_index %>%
      filter(FlagCode %in% as.numeric(substr(Flag(),1, 3))) %>%
      .$SampleID
  })

  # # Convert the selected flag items to a list of numbers that match the flag code
  # flagsSelected <- reactive({
  #   req(input$flag())
  #   as.numeric(substr(input$flag,1, 3))
  # })
  # # Subset the Sample Flag Index by the dfs in the active filter and by the flags selected to exclude
  # # This results in a list of IDs to filter out
  # flagged_ids <- reactive({
  #   # req(input$flag)
  #   if(length(flagsSelected()) > 0){
  #     filter(df_flag_sample_index$SampleID,
  #            df_flag_sample_index$Dataset %in% dfs & df_flag_sample_index$FlagCode %in% flagsSelected())
  #   } else {
  #     NA
  #   }
  # })


  ### Storm Sample Selection

  # Filter df_flag_sample_index so that only flag 114 (Storm Sample Flag) are included
  storm_ids <- reactive({
    df_flag_sample_index %>%
      filter(FlagCode == 114) %>%
      .$SampleID
  })


  # Storm <- reactive({input$storm})
  # nonStorm <- reactive({input$nonstorm})
  # # Filter df_flag_sample_index so that only flag 114 for dfs are included
  # storm_ids <- reactive({
  #   filter(df_flag_sample_index$SampleID,
  #          df_flag_sample_index$Dataset %in% dfs & df_flag_sample_index$FlagCode == 114)
  # })



  ### Reactive List of (non-reactive) Dataframes - filter for selected site, param, value range, date, and remove rows with NA for Result

  Df3 <- reactive({

    # if either month or year is empty (non selected), then do not run. This will result in no data, so will draw errors
    req(Month(), Year()) # See General Note _

    # filter by month and year
    df_temp <- Df2() %>% filter(as.character(month(Date, label = TRUE, abbr = FALSE)) %in% Month(),
                                year(Date) %in% Year(),
                                !is.na(Result))

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
      df_temp <- df_temp %>% filter((ID %in% storm_ids()))
    }

    df_temp

  })


  ### Texts - Tell the user to Select a Input type when none is chosen but neccessary

  # Text - Select Month
  output$text_no_month <- renderText({
    req(is.null(Month()))
    "- Select Months"
  })

  # Text - Select Month
  output$text_no_year <- renderText({
    req(is.null(Year()))
    "- Select Years"
  })

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

  # Dataframe for output and Table
  Df_Table <- reactive({
    if(input$wide){
      Df3_Wide()
    } else{
      Df3()
    }
  })

  # render Datatable
  output$table <- renderDataTable(Df_Table(), selection = 'none')

  # Downloadable csv of selected dataset
  output$download_data <- downloadHandler(
    filename = function() {
      paste("DCRExportedWQData", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(Df_Table(), file)
    }
  )

#####################################################
# Return from Module a list of reactive dataframes.

  return(list(Long = Df3,
              Wide = Df3_Wide,
              Stat = Df3_Stat))

} # end Server Function

