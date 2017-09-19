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
        column(3,
               # Text - Number of Samples or "Select a site"
               wellPanel(
                 h5(textOutput(ns("text.num.text")), align = "center"),
                 h4(textOutput(ns("text.num")), align = "center")
               ), # end Well Panel
               # Site Map
               wellPanel(
                 sitemap.UI(ns("Site Map"))
               ), # end Well Panel
               # Date Selection
               wellPanel(
                 # Date Range
                 dateRangeInput(ns("date"), "Date Range:", 
                                start = df %>% .$Date %>% min(na.rm=TRUE), 
                                end = df %>% .$Date %>% max(na.rm=TRUE),  
                                min = df %>% .$Date %>% min(na.rm=TRUE),
                                max = df %>% .$Date %>% max(na.rm=TRUE),
                                startview = "year"),
                 # Month
                 checkboxGroupInput(ns("month"), "Month:", 
                                    choices = c("All Months",
                                                January = 1,
                                                February = 2,
                                                March = 3,
                                                April = 4,
                                                May = 5,
                                                June = 6,
                                                July = 7,
                                                August = 8,
                                                September = 9,
                                                October = 10,
                                                November = 11,
                                                December = 12), 
                                    selected = "All Months")
               ) # end Well Panel
        ), # end Column
        column(1,
               wellPanel(
                 # Year
                 checkboxGroupInput(ns("year"), "Year:", 
                                    choices = c("All Years", rev(year(seq(as.Date("1990-1-1"), Sys.Date(), "years")))), 
                                    selected = "All Years")
               ) # end Well Panel
        ), # end Column
        column(2,
               # Parameter Selection
               wellPanel(
                 uiOutput(ns("param.ui")),
                 uiOutput(ns("range.ui"))
               ) # end Well Panel
        ), # end column
        column(1,
               # Flag Selection
               wellPanel(
                 checkboxGroupInput(ns("flag"), "Flags:",
                                    choices = df$FlagCode %>% factor() %>% levels())
               ), # end Well Panel
               # storm Sample Selection
               wellPanel(
                 checkboxGroupInput(ns("storm"), "Storm Sample:",
                                    choices = df$StormSample %>% factor() %>% levels())
               ) # end Well Panel
        ), # end column
        column(2,
               # Column Selection
               wellPanel(
                 checkboxGroupInput(ns("column"), "Columns:",
                                    choices = names(df),
                                    selected = col)
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
  

  # Site primary
  
  output$site.primary.ui <- renderUI({
    
    ns <- session$ns # see General Note 1
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    site.primary <- df %>%
      filter(LocationCategory == "Primary Active") %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
    
    # Check box input
    checkboxGroupInput(ns("site.primary"), "Primary Active Sites:",
                       choices = site.primary)
    
    
    
  })  

# Site Categories UI (RendeUI becuase move Primary Active to front)
  
  output$site.nonprim.cat.ui <- renderUI({
    
    ns <- session$ns # see General Note 1
    
    # Change LocationCateogory NA to "NA" to show up in App
    df$LocationCategory <- as.character(df$LocationCategory)
    df$LocationCategory[is.na(df$LocationCategory)] <- "NA"
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    site.categories <- df %>%
      filter(LocationCategory != "Primary Active") %>%
      .$LocationCategory %>%
      factor() %>%
      levels()
    
    # Site Categories
    checkboxGroupInput(ns("site.nonprim.cat"), "Show Other Categories:",
                       choices = site.categories)
    
  })
  
  
  
  # Site Non Primary
  output$site.nonprim.ui <- renderUI({
    
    req(input$site.nonprim.cat) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    site.select <- df %>%
      filter(LocationCategory %in% input$site.nonprim.cat) %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
    
    # Sites
    checkboxGroupInput(ns("site.nonprim"), "Sites:",
                       choices = site.select)
    
  })
  
  
  # Combine Site Input
  
  site.list <- reactive({
    
    c(input$site.primary, input$site.nonprim)
    
  })
  
  
  # Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6
  
  parameters.non.historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  

  # Parameter Selection UI
  
  output$param.ui <- renderUI({
    
    ns <- session$ns # see General Note 1
    
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
    
    # Recent parameters first and then old parameters
    param.choices <- c(param.choices.new, param.choices.old)
    
    checkboxGroupInput(ns("param"), "Parameter: ",
                choices=c(param.choices))
    
  })
  
  
  # Parameter Value Range Bar UI
  
  output$range.ui <- renderUI({
    
    ns <- session$ns # see General Note 1
    
    result <- df %>%
      filter(Parameter %in% input$param) %>%
      .$Result
    
    param.min <- result %>% min(na.rm=TRUE)
    
    param.max <- result %>% max(na.rm=TRUE)
    
    sliderInput(ns("range"), " Value Range",
                min = param.min, max = param.max,
                value = c(param.min, param.max))
    
  })
  
  
# Reactive Dataframe - filter for selected site, param, value range, date, and remove rows with NA for Result
  
  df.react <- reactive({
    
    req(site.list(), input$param, input$range, input$date) # See General Note _
    
    # Filters
    df.temp <- df %>% 
      filter(LocationLabel %in% site.list(), 
             Parameter %in% input$param, 
             Result > input$range[1], Result < input$range[2],
             Date > input$date[1], Date < input$date[2],
             FlagCode %in% input$flag,
             StormSample %in% input$storm, 
             !is.na(Result))
      
    # Filter by Year and Month
    if(input$year != "All Years"){
      df.temp <- df.temp %>% filter(year(Date) == input$year)
    }
    if (input$month != "All Months"){
      df.temp <- df.temp %>% filter(month(Date) == input$month)
    }
    
    # Column Selection
    df.temp %>% select(input$column)
  })
  
  
  # Text - Number of Samples
  
  output$text.num.text <- renderText({
    req(site.list()) # See General Note 1
    "Number of Samples in Selected Data"
  })
  
  # Text - Number of Samples
  
  output$text.num <- renderText({
    req(df.react()) # See General Note 1
    df.react() %>% summarise(n()) %>% paste()
  })
  
  
# Tables
  
  output$table <- renderDataTable(df.react())
  
  
# Site Map
  
  callModule(sitemap, "Site Map", df.site = df.site, site.list = site.list)
  
} # end Server Function

