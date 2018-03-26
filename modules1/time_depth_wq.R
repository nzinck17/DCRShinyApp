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

TIME_DEPTH_WQ <- function(input, output, session, Df) {


  ### Plot

  callModule(PLOT_TIME_DEPTH_WQ, "plot", Df = Df)


  ### Table

  output$table <- renderDataTable(Df())


  ### Summary Statistics

  callModule(STAT_TIME_DEPTH_WQ, "stat", Df = Df)


  ### Downloadable csv of selected dataset

  output$download_data <- downloadHandler(
    filename = function() {
      paste("DCRExportedWQData", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(Df(), file)
    }
  )

} # end server

