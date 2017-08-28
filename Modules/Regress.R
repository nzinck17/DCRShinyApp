##############################################################################################################################
#     Title: Tributary-Regression.R
#     Type: Module for DCR Shiny App
#     Description: Regression plots and tables
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

regress.UI <- function(id, df) {

ns <- NS(id)

tagList(
         wellPanel(
           fluidRow(
             column(3,
                    # SITE
                    wellPanel(
                      uiOutput(ns("site.primary.ui"))
                    ),
                    wellPanel(
                      uiOutput(ns("site.nonprim.cat.ui")),
                      uiOutput(ns("site.nonprim.ui"))
                    ) # end Well Panel
             ), # end column
             column(3,
                    # TEXT
                    wellPanel(
                      h2(textOutput(ns("text.num.null")), align = "center"),
                      h4(textOutput(ns("text.num.text")), align = "center"),
                      h3(textOutput(ns("text.num")), align = "center")
                    ), # end Well Panel
                    # DATE SELECTION
                    wellPanel(
                      uiOutput(ns("date.ui")) 
                    ), # end Well Panel
                    # MAP
                    wellPanel(
                      sitemap.UI(ns("Site Map"))
                    ) # end Well Panel
             ),# end Column
             column(6,
                    fluidRow(
                      column(6,
                             # Y Parameter Selection
                             wellPanel(
                               uiOutput(ns("y.param.ui")),
                               uiOutput(ns("y.range.ui"))
                             ) # well
                      ), # end Column
                      column(6,
                             wellPanel(
                               # X Parameter Selection
                               strong("X axis Parameter:"),
                               radioButtons(ns("x.option"), label = NULL, choices = c("Water Quality", "Meteorology or Hydrology"), inline = TRUE),
                               #See General Note 2
                               conditionalPanel(condition = paste0("input['", ns("x.option"), "'] == 'Water Quality' "),
                                                uiOutput(ns("x.param.ui")),
                                                uiOutput(ns("x.range.ui"))
                               ),# end Conditional Panel
                               conditionalPanel(condition = paste0("input['", ns("x.option"), "'] == 'Meteorology or Hydrology' "),
                                                selectInput(ns("x.met.param"), label = NULL, choices = c("Wind Speed", 
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
                                                sliderInput(ns("x.met.range"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                               )# end Conditional Panel
                             )# end Well Panel
                      )
                    ),# end fluid row
                    hr(),
                    br(),
                    fluidRow(
                      column(4,
                             wellPanel(
                               # MET/HYDRO FILTER 1
                               strong("Meteoro/Hydro Filter 1"),
                               br(), br(),
                               radioButtons(ns("met.option.1"), label = NULL, choices = c("off", "on"), inline = TRUE),
                               selectInput(ns("met.param.1"), label = NULL, choices = c("Wind Speed", 
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
                               sliderInput(ns("met.value.1"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                             )# end Well Panel
                      ),# end Column
                      column(4,
                             # MET/HYDRO FILTER 2
                             wellPanel(
                               strong("Meteoro/Hydro Filter 2"),
                               br(), br(),
                               radioButtons(ns("met.option.2"), label = NULL, 
                                            choices = c("off", "on"), inline = TRUE),
                               selectInput(ns("met.param.2"), label = NULL, 
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
                               sliderInput(ns("met.value.2"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                             )# end Well Panel
                      ),# end Column
                      column(4,
                             # MET/HYDRO FILTER 3
                             wellPanel(
                               strong("Meteoro/Hydro Filter 3"),
                               br(), br(),
                               radioButtons(ns("met.option.3"), label = NULL, 
                                            choices = c("off", "on"), 
                                            inline = TRUE),
                               selectInput(ns("met.param.3"), label = NULL, 
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
                               sliderInput(ns("met.value.3"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                             )# end Well Panel
                      )# end Column
                    )# end Fluid Row
             )# end Column
           )# end Fluid Row
         ), # end Well panel
         # Tabset Panel for plots and tables 
         tabsetPanel(
           
           # the "Plot" tab panel
           tabPanel("Plot", 
                    plot.regress.UI(ns("Plot"))
           ), # end "plot" tabpanel
           
           # Table tabpanel
           tabPanel("Table",
                    # first row - print button, etc
                    fluidRow(
                      br(),
                      actionButton(ns("table.print"), "Print Table")
                    ),
                    # next row
                    fluidRow(
                      br(), br(),
                      dataTableOutput(ns("table"))
                    ) # end fluid row
           ) # end Tab panel - Table
         )  # end tabsetpanel - Plot and Table
) # end Taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

regress <- function(input, output, session, df, df.site) {

# Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6
  
  parameters.non.historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  
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
  
  
# Y axis Parameter
  
  #Parameter Selection UI
  
  output$y.param.ui <- renderUI({
    
    req(site.list()) # See General Note _
    
    ns <- session$ns
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    y.param.choices.new <- df %>%
      filter(LocationLabel %in% c(site.list())) %>%
      filter(Parameter %in% parameters.non.historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    y.param.choices.old <- df %>%
      filter(LocationLabel %in% c(site.list())) %>%
      filter(!(Parameter %in% parameters.non.historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    y.param.choices <- c(y.param.choices.new, y.param.choices.old)
    
    selectInput(ns("y.param"), "Y-axis Parameter:",        
                choices=c(y.param.choices))

  })
  
  
  # Reactive Texts
  
  y.param.units <- reactive({ 
    
    req(site.list()) # See General Note _
    
    df %>%
      filter(Parameter %in% input$y.param) %>%
      .$Units %>%
      factor() %>%
      levels()
    
  })
  
  
  #Parameter Value Range UI
  
  output$y.range.ui <- renderUI({
    
    req(site.list()) # See General Note _
    
    ns <- session$ns
    
    y.result <- df %>%
      filter(LocationLabel %in% c(site.list())) %>%
      filter(Parameter %in% input$y.param) %>%
      .$Result
    
    y.param.min <- y.result %>% min(na.rm = TRUE)
    
    y.param.max <- y.result %>% max(na.rm = TRUE)
    
    sliderInput(ns("y.range"), paste("Range (", y.param.units() ,")"), 
                min = y.param.min, max = y.param.max,
                value = c(y.param.min, y.param.max))
    
  })
  

# X axis parameter 
  
  #Parameter Selection UI
  
  output$x.param.ui <- renderUI({
    
    req(site.list()) # See General Note _
    
    ns <- session$ns
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    x.param.choices.new <- df %>%
      filter(LocationLabel %in% c(site.list())) %>%
      filter(Parameter %in% parameters.non.historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    x.param.choices.old <- df %>%
      filter(LocationLabel %in% c(site.list())) %>%
      filter(!(Parameter %in% parameters.non.historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    x.param.choices <- c(x.param.choices.new, x.param.choices.old)
    
    selectInput(ns("x.param"), "X-axis Parameter:",        
                choices=c(x.param.choices))
    
  })
  
  
  # Reactive Texts
  
  x.param.units <- reactive({ 
    
    req(site.list()) # See General Note _
    
    df %>%
      filter(Parameter %in% input$x.param) %>%
      .$Units %>%
      factor() %>%
      levels()
    
  })
  
  
  #Parameter Value Range UI
  output$x.range.ui <- renderUI({
    
    req(site.list()) # See General Note _
    
    ns <- session$ns
    
    x.result <- df %>%
      filter(LocationLabel %in% c(site.list())) %>%
      filter(Parameter %in% input$x.param) %>%
      .$Result
    
    x.param.min <- x.result %>% min(na.rm = TRUE)
    
    x.param.max <- x.result %>% max(na.rm = TRUE)
    
    sliderInput(ns("x.range"), paste("Range (", x.param.units() ,")"), 
                min = x.param.min, max = x.param.max,
                value = c(x.param.min, x.param.max))
    
  })

  
# Date Selection UI
  
  output$date.ui <- renderUI({
    
    req(site.list()) # See General Note _
    
    ns <- session$ns
    
    Dates <- df %>% 
      filter(LocationLabel %in% c(site.list())) %>%
      .$Date
    
    Date.min <- Dates %>% min(na.rm = TRUE)
    Date.max <- Dates %>% max(na.rm = TRUE)
    
    dateRangeInput(ns("date"), "Date Range:", 
                   start = Date.min, 
                   end = Date.max,
                   min = Date.min,
                   max = Date.max,
                   startview = "year")
    
  })
  
  
# Reactive Dataframe
  
  df.react <- reactive({
    
    req(site.list(), input$y.param, input$y.range, input$x.param, input$x.range, input$date) # See General Note _
    
    # filter by Site and Date adn save
    df.temp <- df %>% 
      filter(LocationLabel %in% c(site.list()),
             Date > input$date[1], Date < input$date[2])
    
    # X Parameter filter and make modifications
    df.temp.x <-  df.temp %>% 
      filter(Parameter %in% c(input$x.param),
             Result > input$x.range[1], Result < input$x.range[2]) %>%
      rename(x.Parameter = Parameter, x.Result = Result) %>%
      select(Site, Date, x.Parameter, x.Result)
    
    # Y Parameter filter and make modifications
    df.temp.y <-  df.temp %>% 
      filter(Parameter %in% c(input$y.param),
             Result > input$y.range[1], Result < input$y.range[2]) %>%
      rename(y.Parameter = Parameter, y.Result = Result) %>%
      select(Site, Date, y.Parameter, y.Result)
    
    # Join the two X and Y parameters dataframes
    inner_join(df.temp.x, df.temp.y, by = c("Site", "Date"))
    
  })
  
  
  # Text - Select Site - Red
  
  output$text.num.null <- renderText({
    req(is.null(site.list())) # See General Note 1
    "Select a Site"
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
  
  # Plot
  
  callModule(plot.regress, "Plot", df = df.react)
  
  # Tables
  
  output$table <- renderDataTable(df.react())
  
  # Site Map
  
  callModule(sitemap, "Site Map", df.site = df.site, site.list = site.list)
  
} # end Server Function