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

  # Tabset panel for plots, tables, etc.
  tabsetPanel(

    # Plot Tab
    tabPanel("Scatter Plot",
             PLOT_CORR_WQ_UI(ns("plot_scatter"))
    ), # end Tab Panel - Plot

    # Summary Tabpanel
    tabPanel("Correlation Matrix",
             PLOT_CORR_MATRIX_WQ_UI(ns("plot_matrix"))
    )#, # end Tab Panel - Summary

    # Table Tabpanel
    # tabPanel("Table",
    #          fluidRow(br(),
    #                   column(3,
    #                          radioButtons(ns("table_format"), "Table Format",
    #                                       choices = c("long", "wide")),
    #                          downloadButton(ns("download_data"), "Download table as csv")
    #                   ),
    #                   column(9,
    #                          uiOutput(ns("column_ui"))
    #                   ),
    #                   br()
    #          ), # end Fluid Row
    #          fluidRow(
    #            dataTableOutput(ns("table"))
    #          ) # end Fluid Row
    # ) # end Tab Panel - Table

  ) # end tabsetpanel (plots, stats, etc.)
) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

CORRELATION_WQ <- function(input, output, session, Dfa, Dfb) {


  ns <- session$ns # see General Note 1

  # Plot - Scatter
  callModule(PLOT_CORR_WQ, "plot_scatter", Df = Dfa)

  # Plot - Scatter
  callModule(PLOT_CORR_MATRIX_WQ, "plot_matrix", Df = Dfb)


  ### Downloadable csv of selected dataset
#
#   output$download_data <- downloadHandler(
#     filename = function() {
#       paste("DCRExportedWQData", ".csv", sep = "")
#     },
#     content = function(file) {
#       write_csv(Df_Table(), file)
#     }
#   )
#
#
#   # Column Selection for table output
#   output$column_ui <- renderUI({
#     if(input$table_format == "long"){
#       df <- Dfa()
#     } else{
#       df <- Dfb()
#     }
#     checkboxGroupInput(ns("column"), "Table Columns:",
#                        choices = names(df),
#                        selected = names(df),
#                        inline = TRUE)
#   })
#
#   # Df for Table
#   Df_Table <- reactive({
#     req(input$column)
#     if(input$table_format == "long"){
#       Df3() %>% select(c(input$column))
#     } else{
#       Df4() %>% select(c(input$column))
#     }
#   })







} # end Server Function

