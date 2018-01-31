##############################################################################################################################
#     Title: Metadata.R
#     Type: Module for DCR Shiny App
#     Description: Shows Tables of Metadata (Location, Parameter, Flags)
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

metadata.UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    tabsetPanel(
      tabPanel("Location",
               fluidRow(
                 dataTableOutput(ns("table.site"))
               ) # end Fluid Row
      ),
      tabPanel("Parameter",
               fluidRow(
                 dataTableOutput(ns("table.param"))
               ) # end Fluid Row
      ),
      tabPanel("Flags",
               fluidRow(
                 dataTableOutput(ns("table.flag"))
               ) # end Fluid Row
      )
    )
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# This module does not take any reactive expressions. Changes will have to be made to accmodate reactive expressions
# dfs is a list of dataframes

metadata <- function(input, output, session, df.site, df.param, df.flag) {
  
  ns <- session$ns # see General Note 1
  
  ### Tables
  
  output$table.site <- renderDataTable(df.site)
  output$table.param <- renderDataTable(df.param)
  output$table.flag <- renderDataTable(df.flag)
  

} # end Server Function

