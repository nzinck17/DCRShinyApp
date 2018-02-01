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
               ), # end Fluid Row
               fluidRow(
                 column(2,
                   radioButtons(ns("df_choice_site"), "Full or Filtered Data:", 
                                choices = c("full", "filtered"))
                 ),
                 # wellPanel(
                 #   sitemap.UI(ns("site.map"))
                 # ), # end Well Panel
                 column(6,
                   tableOutput(ns("table.site.select"))
                 ),
                 column(4,
                   imageOutput(ns("river_image"))
                 )
               ) # end Fluid Row
      ),
      tabPanel("Parameter",
               fluidRow(
                 dataTableOutput(ns("table.param"))
               ), # end Fluid Row
               radioButtons(ns("df_choice_param"), "Full or Filtered Data:", 
                            choices = c("full", "filtered"),
                            inline = TRUE),
               fluidRow(
                 tableOutput(ns("table.param.select"))
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

# This module takes df as a reactive expressions. Changes will have to be made to accmodate reactive expressions
# 

metadata <- function(input, output, session, df.full, df.filtered, df.site, df.param, df.flag) {
  
  ns <- session$ns # see General Note 1
  
  
  ### Primary Tables
  
  output$table.site <- renderDataTable(df.site, selection = 'single', 
                                       options = list(lengthMenu = c(5, 10, 50), pageLength = 5))
  output$table.param <- renderDataTable(df.param, selection = 'single')
  output$table.flag <- renderDataTable(df.flag, selection = 'single')
  
  
  #DT::datatable(df.react.corr(), options = list(lengthMenu = c(15, 25, 50, 100), pageLength = 15))
  
  ### Additonal Site Info Table
  
  ### Dataframe filtered or full based on Selection
  
  DF_Site_Raw <- reactive({
    if(input$df_choice_site == "filtered"){
      df.filtered()
    }else{
      df.full
    }
  })
  
  Site_Selected <- reactive({
    df.site$LocationLabel[input$table.site_rows_selected]
  })
  
  
  DF_Site_Sum_1 <- reactive({
    
    DF_Site_Raw() %>%
      filter(LocationLabel %in% Site_Selected()) %>%
      summarise(`Parameter` = "ALL",
                `Number of Samples` = n(),
                `Start Date` = min(Date),
                `End Date` = max(Date),
                `Latest Result` = NA,
                `Units` = NA)
  })
  
  DF_Site_Sum_2 <- reactive({
    
    DF_Site_Raw() %>%
      filter(LocationLabel %in% Site_Selected()) %>%
      group_by(Parameter) %>%
      summarise(`Number of Samples` = n(),
                `Start Date` = min(Date),
                `End Date` = max(Date),
                `Latest Result` = Result[Date == max(Date)],
                `Units` = Units[Date == max(Date)])
  })
  
  
  DF_Site_Sum <- reactive({rbind(DF_Site_Sum_1(), DF_Site_Sum_2())})

  output$table.site.select <- renderTable({DF_Site_Sum()})
  
  output$river_image <- renderImage({
    list(src = "river.jpg")
  }, deleteFile = TRUE)  
  
  
  
  ### Additional Parameter Info Table
  
  DF_Param_Raw <- reactive({
    if(input$df_choice_param == "filtered"){
      df.filtered()
    }else{
      df.full
    }
  })
  
  
  Param_Selected <- reactive({
    df.site$Parameter[input$table.param_rows_selected]
  })
  
  
  DF_Param_Sum_1 <- reactive({
    
    DF_Param_Raw() %>%
      filter(Parameter %in%  Param_Selected()) %>%
      group_by(LocationLabel) %>%
      summarise(`Number of Samples` = n(),
                `Start Date` = min(Date),
                `End Date` = max(Date),
                `Latest Result` = Result[Date == max(Date)],
                `Units` = Units[Date == max(Date)])
  })
  
  
  DF_Param_Sum_2 <- reactive({
    
    DF_Param_Raw() %>%
      filter(Parameter %in%  Param_Selected()) %>%
      summarise(`LocationLabel` = "ALL",
                `Number of Samples` = n(),
                `Start Date` = min(Date),
                `End Date` = max(Date),
                `Latest Result` = NA,
                `Units` = NA)
  })
  
  
  DF_Param_Sum <- reactive({rbind(DF_Param_Sum_1(), DF_Param_Sum_2())})

  output$table.param.select <- renderTable(DF_Param_Sum())
  
  
  
  
} # end Server Function

