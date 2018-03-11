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
                 fluidRow(h3("Advanced Filters")),
                 fluidRow(
                   column(4,
                          # Date Selection
                          wellPanel(
                            # Month
                            CHECKBOX_SELECT_ALL_UI(ns("month"))
                          ), # end Well Panel
                          wellPanel(
                            # Year
                            SELECT_SELECT_ALL_UI(ns("year"))
                          ) # end Well Panel
                   ), # end Column
                   column(4,
                          # # Flag Selection
                          # wellPanel(
                          #   SELECT_SELECT_ALL_UI(ns("flag"))
                          # ), # end Well Panel
                          # storm Sample Selection
                          # wellPanel(
                          #   CHECKBOX_SELECT_ALL_UI(ns("storm"))
                          # ), # end Well Panel
                          # Text - Number of Samples or "Select a site"
                          wellPanel(
                            h5(textOutput(ns("text_no_month"))),
                            h5(textOutput(ns("text_no_year"))),
                            h5(textOutput(ns("text_no_flag"))),
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

FILTER_WQ <- function(input, output, session, df, df_site, type = "wq") {
# Types include: "wq", "wq_depth", and "profile". More can be added
########################################################
# Main Selection

  ns <- session$ns # see General Note 1

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
  Site <- if(type == "wq"){
    callModule(SITE_CHECKBOX, "site", Df = reactive({df}))
  } else if(type == "wq_depth"){
    callModule(STATION_LEVEL_CHECKBOX, "site", Df = reactive({df}))
  } else if(type == "profile"){
    callModule(SITE_PROFILE, "site", Df = reactive({df}))
  }

  # Site Map
  callModule(SITE_MAP, "site_map", df_site = df_site, Site_List = Site)


  # Reactive Dataframe - first filter of the dataframe for Site
  Df1 <- reactive({
    req(Site())

    if(type == "wq" | type == "wq_depth"){
    df %>%
      filter(LocationLabel %in% Site(),
             !is.na(Result)) # Is this needed? can remove this once certain no NAs.
    } else if(type == "profile"){
      df %>%
        filter(LocationLabel %in% Site()$Site,
               # Depthm >= Site()$Depth_Lower,
               # Depthm >= Site()$Depth_Upper,
               !is.na(Result)) # Is this needed? can remove this once certain no NAs.
    }
  })


  ### Parameter and Date Range

  # Parameter Selection using ParameterSelect Module
  Param <- callModule(PARAM_SELECT, "param", Df = Df1)

  # Date Range Selection Using DateSelect Module
  Date_Range <- callModule(DATE_SELECT, "date", Df = Df1)

  # Reactive Dataframe - filter for param, value range, date, and remove rows with NA for Result
  Df2 <- reactive({
    req(Param$Type(), Param$Range_Min(), Param$Range_Min(), Date_Range$Lower(), Date_Range$Upper()) # See General Note _

    Df1() %>%
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

  # server
  Month <- callModule(CHECKBOX_SELECT_ALL, "month",
                            label = "Months:",
                            choices = reactive({month.name}),
                            selected = reactive({month.name}),
                            colwidth = 3,
                            inline = TRUE)


  ### Year Selection

  # Choices
  year_choices <- c(rev(year(seq(as.Date("1980-01-01"), Sys.Date(), "years")))) # Change to first year of data

  # Server
  Year <- callModule(SELECT_SELECT_ALL, "year",
                           label = "Years:",
                           choices = reactive({year_choices}),
                           selected = reactive({year_choices}),
                           colwidth = 3)


  # ### Flag Selection
  #
  # # Choices
  # flag_choices <- Df2$FlagCode %>% factor() %>% levels()
  #
  # # Server
  # Flag <- callModule(SELECT_SELECT_ALL, "flag",
  #                    label = "Flags:",
  #                    choices = reactive({flag_choices}),
  #                    selected = reactive({flag_choices}),
  #                    colwidth = 3,
  #                    hidden = TRUE)


  ### Storm Selection

  # # Choices
  # storm_choices <- dfs[[1]]$StormSample %>% factor() %>% levels()
  #
  # # Server
  # Storm <- callModule(CHECKBOX_SELECT_ALL, "storm",
  #                     label = "Storm Sample:",
  #                     choices = reactive({storm_choices}),
  #                     selected = reactive({storm_choices}),
  #                     hidden = TRUE)


  ### Reactive List of (non-reactive) Dataframes - filter for selected site, param, value range, date, and remove rows with NA for Result

  Df3 <- reactive({

    req(Month(), Year()) #, Flag(), Storm() # See General Note _

    Df2() %>% filter(as.character(month(Date, label = TRUE, abbr = FALSE)) %in% Month(),
                   year(Date) %in% Year(),
                   #FlagCode %in% Flag(),
                   #StormSample %in% Storm(),
                   !is.na(Result))
  })


  ### Texts

  output$text_no_month <- renderText({
    req(is.null(Month()))
    "- Select Months"
  })

  output$text_no_year <- renderText({
    req(is.null(Year()))
    "- Select Years"
  })

  # output$text_no_flag <- renderText({
  #   req(is.null(Flag()))
  #   "- Please Select Flag Types"
  # })
  #
  # output$text_no_storm <- renderText({
  #   req(is.null(Storm()))
  #   "- Please Select Storm Sample Types"
  # })


  ### Return List of Reactive Dataframes
  # a Reactive List Expression is converted to a (Non-reactive) List of Reactive Expressions


  # Reactive Dataframe - Wide Format (for Correlation ScatterPlot and Correlation Matrix)
  Df3_Wide <- reactive({
    # require Dataframe to be more than zero observations - prvent from crashing
    req(Df3() %>% summarise(n()) %>% unlist() != 0)
    Df3() %>%
      select(-Units) %>%
      distinct(LocationLabel, Date, Parameter, .keep_all = TRUE) %>%
      spread("Parameter", "Result")
  })

  # Reactive Dataframe - Wide Format (for Correlation ScatterPlot and Correlation Matrix)
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

