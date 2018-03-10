##############################################################################################################################
#     Title: Tributary-Time.R
#     Type: Module for DCR Shiny App
#     Description: Time Series plots, tables, and summary stats for Tributaries
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#

##############################################################################################################################
# User Interface
##############################################################################################################################

TIME_WQ_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(

    # Tabset panel for plots, tables, etc.
    tabsetPanel(

      # Plot Tab
      tabPanel("Plot",
               # h4(textOutput(ns("text_plot_zero_data"))),
               PLOT_TIME_WQ_UI(ns("plot"))
      ), # end Tab Panel - Plot

      # Table Tabpanel
      tabPanel("Table",
               # h4(textOutput(ns("text_table_zero_data"))),
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
               # h4(textOutput(ns("text_zero_data"))),
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

TIME_WQ <- function(input, output, session, Df) {

  ns <- session$ns # see General Note 1

  # Plot
  callModule(PLOT_TIME_WQ, "plot", Df = Df)


  # Table
  output$table <- renderDataTable(Df_Table())


  # Stats
  callModule(STAT_TIME_WQ, "stats", Df = Df)


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
                       choices = names(Df()),
                       selected = names(Df()),
                       inline = TRUE)
  })

  # Df for Table
  Df_Table <- reactive({
    req(input$column)
    Df() %>% select(c(input$column))
  })



  # # Text - Zero data Selected for Plot
  # output$text_zero_data <- renderText({
  #   req(Df()) # See General Note 1
  #   if(Df() %>% summarise(n()) %>% unlist() == 0){
  #     "Selection Contains Zero Observations"
  #   }
  # })




} # end Server Function

