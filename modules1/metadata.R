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
               br(), 
               br(),
               wellPanel(
                 fluidRow(
                   column(3,
                          sitemap.UI(ns("site_map"))
                   ),
                   column(6,
                          uiOutput(ns("df_choice_site_ui")),
                          tableOutput(ns("table.site.select")),
                          h3(textOutput(ns("site_text")))
                   ),
                   column(3,
                          imageOutput(ns("river_image"))
                   )
                 )
               ) # end Fluid Row
      ),
      tabPanel("Parameter",
               fluidRow(
                 dataTableOutput(ns("table.param"))
               ), # end Fluid Row
               br(), 
               br(),
               wellPanel(
                 fluidRow(
                   column(1),
                   column(10,
                          uiOutput(ns("df_choice_param_ui")),
                          tableOutput(ns("table.param.select")),
                          h3(textOutput(ns("param_text")))
                   ),
                   column(1)
                 )
               ) # end Fluid Row
      ),
      tabPanel("Flags Codes",
               fluidRow(
                 dataTableOutput(ns("table.flag"))
               ) # end Fluid Row
      ),
      tabPanel("Flag Samples",
               fluidRow(
                 dataTableOutput(ns("table.flag.sample"))
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

metadata <- function(input, output, session, df.full, df.filtered, df.site = NULL, df.param = NULL, df.flag = NULL, df.flag.sample = NULL) {
  
  ns <- session$ns # see General Note 1
  
  
  ### Primary Tables
  
  output$table.site <- renderDataTable(df.site, selection = 'single', 
                                       options = list(lengthMenu = c(5, 10, 50), pageLength = 5))
  
  output$table.param <- renderDataTable(df.param, selection = 'single',
                                        options = list(lengthMenu = c(5, 10, 50), pageLength = 5))
  
  output$table.flag <- renderDataTable(df.flag, selection = 'single')
  
  output$table.flag.sample <- renderDataTable(df.flag.sample, selection = 'single')
  
  
  
  
  ### Additonal Site Info
  
  # Selected row from DT
  Site_Selected <- reactive({
    req(input$table.site_rows_selected)
    # if(!is.null(input$table.site_rows_selected)){
      df.site[input$table.site_rows_selected, ]
    # }
    # else{
    #   NULL
    # }
  })
  
  # UI for Full or filtered choice
  output$df_choice_site_ui <- renderUI({
    req(Site_Selected())
    radioButtons(ns("df_choice_site"), "Full or Filtered Data:", 
                 choices = c("full", "filtered"),
                 inline = TRUE)
  })
  
  # Dataframe filtered or full based on Selection
  DF_Site_Raw <- reactive({
    req(input$df_choice_site)
    if(input$df_choice_site == "filtered"){
      df.filtered()
    }else{
      df.full
    }
  })
  
  # Site Map
  callModule(Site_Map_Single, "site_map", site = Site_Selected)
  
  # Site Overview Table
  DF_Site_Sum_1 <- reactive({
    DF_Site_Raw() %>%
      filter(LocationLabel %in% Site_Selected()$LocationLabel) %>%
      summarise(`Parameter` = "ALL",
                `Number of Samples` = n(),
                `Start Date` = as.character(min(Date)),
                `End Date` = as.character(max(Date)),
                `Latest Result` = NA,
                `Units` = NA)
  })
  
  DF_Site_Sum_2 <- reactive({
    DF_Site_Raw() %>%
      filter(LocationLabel %in% Site_Selected()$LocationLabel) %>%
      group_by(Parameter) %>%
      summarise(`Number of Samples` = n(),
                `Start Date` = as.character(min(Date)),
                `End Date` = as.character(max(Date)),
                `Latest Result` = Result[Date == max(Date)],
                `Units` = Units[Date == max(Date)])
  })
  
  DF_Site_Sum <- reactive({rbind(DF_Site_Sum_1(), DF_Site_Sum_2())})

  output$table.site.select <- renderTable({
    req(Site_Selected())
    DF_Site_Sum()
    })
  
  # Site Image
  output$river_image <- renderImage({
    req(Site_Selected()) # can delete once Site_Selected is used in renderImage due to earlier req()
    
    list(src = "images/river.jpg",
         width="100%",
         height= "350")
  }, deleteFile = FALSE)
  
  # Site Text - when no site is selected
  output$site_text <- renderText({
    req(is.null(input$table.site_rows_selected))
    "SELECT a ROW above to see MORE INFO on a LOCATION"
  })
  
  
  
  
  
  ### Additional Parameter Info Table
  
  # Selected row from DT
  Param_Selected <- reactive({
    req(input$table.param_rows_selected)
    df.param[input$table.param_rows_selected, ]
  })
  
  # UI for Full or filtered choice
  output$df_choice_param_ui <- renderUI({
    req(Param_Selected())
    radioButtons(ns("df_choice_param"), "Full or Filtered Data:", 
                 choices = c("full", "filtered"),
                 inline = TRUE)
  })
  
  # Dataframe filtered or full based on Selection
  DF_Param_Raw <- reactive({
    req(input$df_choice_param)
    if(input$df_choice_param == "filtered"){
      df.filtered()
    }else{
      df.full
    }
  })
  
  # param Overview Table
  DF_Param_Sum_1 <- reactive({
    DF_Param_Raw() %>%
      filter(Parameter %in%  Param_Selected()$ParameterName) %>%
      summarise(`LocationLabel` = "ALL",
                `Number of Samples` = n(),
                `Start Date` = as.character(min(Date)),
                `End Date` = as.character(max(Date)),
                `Latest Result` = NA,
                `Units` = NA)
  })
    
  DF_Param_Sum_2 <- reactive({
    DF_Param_Raw() %>%
      filter(Parameter %in%  Param_Selected()$ParameterName) %>%
      group_by(LocationLabel) %>%
      summarise(`Number of Samples` = n(),
                `Start Date` = as.character(min(Date)), # need as.character becuase of renderTable has a bug for Date Class (xtable bug)
                `End Date` = as.character(max(Date)),
                `Latest Result` = Result[Date == max(Date)],
                `Units` = Units[Date == max(Date)])
  })
  
  DF_Param_Sum <- reactive({rbind(DF_Param_Sum_1(), DF_Param_Sum_2())})

  output$table.param.select <- renderTable(DF_Param_Sum())

  # Param Text - when no site is selected
  output$param_text <- renderText({
    req(is.null(input$table.param_rows_selected))
    "SELECT a ROW above to see MORE INFO on a PARAMETER"
  })
  
  
  
} # end Server Function

