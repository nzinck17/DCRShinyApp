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

export.wq.UI <- function(id, df, col) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    wellPanel(       
      fluidRow(
        column(3,
               # Site Selection
               uiOutput(ns("site.ui"))
        ), # end Column
        column(2,
               # Text - Number of Samples or "Select a site"
               wellPanel(
                 h5(textOutput(ns("text.no.site"))),
                 h5(textOutput(ns("text.no.param"))),
                 h5(textOutput(ns("text.no.month"))),
                 h5(textOutput(ns("text.no.year"))),
                 h5(textOutput(ns("text.no.flag"))),
                 h5(textOutput(ns("text.no.storm"))),
                 h5(textOutput(ns("text.num.text")), align = "center"),
                 h4(textOutput(ns("text.num")), align = "center")
               ), # end Well Panel
               # Date Selection
               wellPanel(
                 # Date Range
                 dateRangeInput(ns("date"), "Date Range:", 
                                start = df %>% .$Date %>% min(na.rm=TRUE), 
                                end = df %>% .$Date %>% max(na.rm=TRUE),  
                                min = df %>% .$Date %>% min(na.rm=TRUE),
                                max = df %>% .$Date %>% max(na.rm=TRUE),
                                startview = "year")
               ), # end Well Panel
               wellPanel(
                 # Month
                 checkboxSelectAll.UI(ns("month"), label = "Month:", choices = c(month.name))
               ) # end Well Panel
        ), # end Column
        column(1,
               wellPanel(
                 # Year
                 uiOutput(ns("year.ui"))
               ) # end Well Panel
        ), # end Column
        column(2,
               # Parameter Selection
               uiOutput(ns("param.ui"))
        ), # end column
        column(2,
               # Flag Selection
               wellPanel(
                 checkboxSelectAll.UI(ns("flag"), "Flags:", choices = df$FlagCode %>% factor() %>% levels())
               ), # end Well Panel
               # storm Sample Selection
               wellPanel(
                 checkboxSelectAll.UI(ns("storm"), "Storm Sample:", choices = df$StormSample %>% factor() %>% levels())
               ) # end Well Panel
        ), # end column
        column(2,
               # Column Selection
               wellPanel(
                 checkboxSelectAll.UI(ns("column"), "Table Columns:", choices = names(df), selected = col)
               )
        ) # end column
      ) # end fluidrow     
    ), # end well panel
    
    fluidRow(br(),
             br(),
             actionButton(ns("table.print"), "Print Table")
    ), # end Fluid Row
    fluidRow(
      dataTableOutput(ns("table"))
    ) # end Fluid Row

  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

export.wq <- function(input, output, session, df, df.site, col) { 
  

  
  ### Site Selection using Site Select Module
  
  # Ui
  output$site.ui <- renderUI({
    ns <- session$ns # see General Note 1
    site.checkbox.UI(ns("site"), df = df)
  })
  
  # Server
  site <- callModule(site.checkbox, "site", df = df)
  
  
  
  ### param Selection using Site Select Module
  
  # Ui
  output$param.ui <- renderUI({
    ns <- session$ns # see General Note 1
    param.checkbox.UI(ns("param"))
  })
  
  # Server
  param <- callModule(param.checkbox, "param", df = df)


  
  ### Year Selection
  
  # Choices
  year.choices <- c(rev(year(seq(as.Date("1990-1-1"), Sys.Date(), "years"))))
  
  # UI
  output$year.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("year"), label = "Year:", choices = year.choices)
  })
  
  # Server
  year.input <- callModule(checkboxSelectAll, "year", choices = year.choices, colwidth = 1)
  
  
  
  ### Server for other Inputs using tht checkboxSelectAll Module
  month.input <- callModule(checkboxSelectAll, "month", choices = c(month.name), colwidth = 2)
  flag <- callModule(checkboxSelectAll, "flag", choices = df$FlagCode %>% factor() %>% levels(), colwidth = 1)
  storm <- callModule(checkboxSelectAll, "storm", choices = df$StormSample %>% factor() %>% levels(), colwidth = 1)
  column <- callModule(checkboxSelectAll, "column", choices = names(df), selected = col, colwidth = 2)
  
  
  
  ### Reactive Dataframe - filter for selected site, param, value range, date, and remove rows with NA for Result
  
  df.react <- reactive({
    
    req(site(), param$type(), month.input(), year.input(), flag(), storm(), column()) # See General Note _
    
    # Filters
    df.temp <- df %>% 
      filter(LocationLabel %in% site(), 
             Parameter %in% param$type(), 
             Result > param$range.min(), Result < param$range.max(),
             Date > input$date[1], Date < input$date[2],
             as.character(month(Date, label = TRUE, abbr = FALSE)) %in% month.input(),
             year(Date) %in% year.input(),
             FlagCode %in% flag(),
             StormSample %in% storm(),
             !is.na(Result))
    
    # Column Selection
    df.temp %>% select(column())
  })
  
    
  
  ### Texts
  
  output$text.no.site <- renderText({
    req(is.null(site()))
    "- Please Select Sites"
  })
  
  output$text.no.param <- renderText({
    req(is.null(param$type()))
    "- Please Select Parameters"
  })
  
  output$text.no.month <- renderText({
    req(is.null(month.input()))
    "- Please Select Months"
  })
  
  output$text.no.year <- renderText({
    req(is.null(year.input()))
    "- Please Select Years"
  })
  
  output$text.no.flag <- renderText({
    req(is.null(flag()))
    "- Please Select Flag Types"
  })
  
  output$text.no.storm <- renderText({
    req(is.null(storm()))
    "- Please Select Storm Sample Types"
  })
  
  
  # Text - Number of Samples
  
  output$text.num.text <- renderText({
    req(df.react())
    "Number of Samples in Selected Data"
  })
  
  output$text.num <- renderText({
    req(df.react())
    df.react() %>% summarise(n()) %>% paste()
  })
  
  
# Tables
  
  output$table <- renderDataTable(df.react())
  
  
} # end Server Function

