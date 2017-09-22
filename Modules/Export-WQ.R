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
               wellPanel(
                 uiOutput(ns("site.primary.ui"))
               ),
               wellPanel(
                 uiOutput(ns("site.nonprim.cat.ui")),
                 uiOutput(ns("site.nonprim.ui"))
               ) # end Well Panel
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
                 checkboxSelectAll.UI(ns("year"), label = "Year:", 
                                   choices = c(rev(year(seq(as.Date("1990-1-1"), Sys.Date(), "years")))))
               ) # end Well Panel
        ), # end Column
        column(2,
               # Parameter Selection
               wellPanel(
                 uiOutput(ns("param.ui")),
                 br(),
                 uiOutput(ns("range.ui"))
               ) # end Well Panel
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
  


  # Site - Primary
  
  site.primary <- df %>% filter(LocationCategory == "Primary Active") %>%
    .$LocationLabel %>% factor() %>% levels()
  
  output$site.primary.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("site.primary"), "Primary Active Sites:", choices = site.primary)
  })
  
  callModule(checkboxSelectAll, "site.primary", choices = site.primary)

  
  # Site - Non Primary Categories
  
  # Change LocationCateogory NA to "NA" to show up in App
  df$LocationCategory <- as.character(df$LocationCategory)
  df$LocationCategory[is.na(df$LocationCategory)] <- "NA"
  
  site.categories <- df %>% filter(LocationCategory != "Primary Active") %>%
    .$LocationCategory %>% factor() %>% levels()
  
  output$site.nonprim.cat.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("site.nonprim.cat"), "Show Other Categories:", choices = site.categories)
  })
  
  callModule(checkboxSelectAll, "site.nonprim.cat", choices = site.categories)
  
  
  # Site - NonPrimary Sites
  
  site.select <- reactive({
    req(input$'site.nonprim.cat-checkbox') # See General Note 5
    
    df %>%
      filter(LocationCategory %in% input$'site.nonprim.cat-checkbox') %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
  })
  
  output$site.nonprim.ui <- renderUI({
    #req(input$site.nonprim.cat) # See General Note 5
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("site.nonprim"), "Sites:", choices = site.select())
  })
  
  callModule(checkboxSelectAll, "site.nonprim", choices = site.select())
  
  
  # Site - Combine to Vector
  
  site.list <- reactive({
    c(input$'site.primary-checkbox', input$'site.nonprim-checkbox')
  })
  
  
  
  # Parameter - Non Historical (when a Parameter has been used  in the last 5 years). See General Note 6
  
  parameters.non.historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  # Parameters which have data at any Site (in the mofule's df) within 5 years.
  param.choices.new <- df %>%
    filter(Parameter %in% parameters.non.historical) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
  param.choices.old <- df %>%
    filter(!(Parameter %in% parameters.non.historical)) %>%
    .$Parameter %>%
    factor() %>%
    levels()

  param.choices <- c(param.choices.new, param.choices.old)
  
  # Parameter - Selection UI
  output$param.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("param"), "Parameter: ", choices=c(param.choices))
  })
  
  callModule(checkboxSelectAll, "param", choices = param.choices, colwidth = 2)
  
  
  # Parameter Value Range Bar UI
  
  output$range.ui <- renderUI({
    
    ns <- session$ns # see General Note 1
    
    result <- df %>%
      filter(Parameter %in% input$'param-checkbox') %>%
      .$Result
    
    param.min <- result %>% min(na.rm=TRUE)
    
    param.max <- result %>% max(na.rm=TRUE)
    
    sliderInput(ns("range"), " Value Range",
                min = param.min, max = param.max,
                value = c(param.min, param.max))
    
  })
  
  callModule(checkboxSelectAll, "month", choices = c(month.name), colwidth = 2)
  callModule(checkboxSelectAll, "year", choices = c(rev(year(seq(as.Date("1990-1-1"), Sys.Date(), "years")))), colwidth = 1)
  callModule(checkboxSelectAll, "flag", choices = df$FlagCode %>% factor() %>% levels(), colwidth = 1)
  callModule(checkboxSelectAll, "storm", choices = df$StormSample %>% factor() %>% levels(), colwidth = 1)
  callModule(checkboxSelectAll, "column", choices = names(df), selected = col, colwidth = 2)
  
  
# Reactive Dataframe - filter for selected site, param, value range, date, and remove rows with NA for Result
  
  df.react <- reactive({
    
    req(site.list(), input$'param-checkbox', input$'flag-checkbox', input$'storm-checkbox',
        input$'month-checkbox', input$'year-checkbox', input$'column-checkbox') # See General Note _
    
    # Filters
    df.temp <- df %>% 
      filter(LocationLabel %in% site.list(), 
             Parameter %in% input$'param-checkbox', 
             Result > input$range[1], Result < input$range[2],
             Date > input$date[1], Date < input$date[2],
             as.character(month(Date, label = TRUE, abbr = FALSE)) %in% input$'month-checkbox',
             year(Date) %in% input$'year-checkbox',
             FlagCode %in% input$'flag-checkbox',
             StormSample %in% input$'storm-checkbox',
             !is.na(Result))
    
    # Column Selection
    df.temp %>% select(input$'column-checkbox')
  })
  
    
  # Texts - Please Select
  
  output$text.no.site <- renderText({
    req(is.null(site.list()))
    "- Please Select Sites"
  })
  
  output$text.no.param <- renderText({
    req(is.null(input$'param-checkbox'))
    "- Please Select Parameters"
  })
  
  output$text.no.month <- renderText({
    req(is.null(input$'month-checkbox'))
    "- Please Select Months"
  })
  
  output$text.no.year <- renderText({
    req(is.null(input$'year-checkbox'))
    "- Please Select Years"
  })
  
  output$text.no.flag <- renderText({
    req(is.null(input$'flag-checkbox'))
    "- Please Select Flag Types"
  })
  
  output$text.no.storm <- renderText({
    req(is.null(input$'storm-checkbox'))
    "- Please Select Storm Sample Types"
  })
  
  
  # Text - Number of Samples
  
  output$text.num.text <- renderText({
    req(df.react())
    "Number of Samples in Selected Data"
  })
  
  output$text.num <- renderText({
    req(df.react()) # See General Note 1
    df.react() %>% summarise(n()) %>% paste()
  })
  
  
# Tables
  
  output$table <- renderDataTable(df.react())
  
  
} # end Server Function

