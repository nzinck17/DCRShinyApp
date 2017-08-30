##############################################################################################################################
#     Title: Profile-Line.R
#     Type: Module for DCR Shiny App
#     Description: Line Plot for 
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#   2. Tried Progress Bar for Plot and did not work well. Used Custom Message instead

# To-Do List:
#   1. Make Loading Bar for Plot
#   2. Make option for COloring Scale (whether based on Site, Year; Site; or None)
#   3. Change Decimal Date to DOY

##############################################################################################################################
# User Interface
##

prof.line.UI <- function(id, df) {
  
ns <- NS(id)

tagList(
  wellPanel(
    fluidRow(
      column(2,
             # SITE
             wellPanel(
               checkboxGroupInput(ns("site"), "Site: (Select First)", 
                                  choices = levels(factor(df$Site)))
             )
      ),
      column(2,
             # SITE
             wellPanel(
               h3(textOutput(ns("text.site.null")), align = "center"),
               h3(textOutput(ns("text.param.null")), align = "center"),
               h4(textOutput(ns("text.num.text")), align = "center"),
               h3(textOutput(ns("text.num")), align = "center")
             )
      ),
      column(3,
             # PARAMETER
             wellPanel(
               uiOutput(ns("param.ui"))
             )
      ),
      column(5,
             # DATE
             wellPanel(
               fluidRow(
                 column(6,
                        radioButtons(ns("date.option"), "Choose Date Method:",        
                                     choices=c("Calendar Range",
                                               "Select Year",
                                               "Select Month",
                                               "Select Day"),
                                     selected = "Calendar Range")
                 ),
                 column(6,
                        uiOutput(ns("date.ui"))
                 )
               ) # end Fluid Row
             ) # end Well Panel
      ) # end Column
    ) # end Fluid Row
  ), # well panel
  
  tabsetPanel(
    # the "Plot" tab panel where everything realted to the plot goes
    tabPanel("Custom Plot",
             plot.profline.custom.UI(ns("Plot Profile Line Custom"))
    ),
    tabPanel("Standard Template Line Plot",
             fluidRow(
               h2("Soon to Come", align = "center")
             )
    ),
    tabPanel("Table",
             fluidRow(
               dataTableOutput(ns("table.dynamic"))
             )
    )
  ) # end tabsetpanel
) # end taglist 
}

##############################################################################################################################
# Server Function
##############################################################################################################################


prof.line <- function(input, output, session, df) {
  
  # filter DF for blank data
  
  df <- df %>% filter(!is.na(Date), 
                      !is.na(Depthm),
                      !is.na(Result))
  
  # Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6
  
  parameters.non.historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  
  # Parameter Selection UI
  
  output$param.ui <- renderUI({
    
    req(input$site) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    param.choices.new <- df %>%
      filter(Site %in% input$site,
             Parameter %in% parameters.non.historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    param.choices.old <- df %>%
      filter(Site %in% input$site,
             !(Parameter %in% parameters.non.historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    param.choices <- c(param.choices.new, param.choices.old)
    
    # Parameter Input
    checkboxGroupInput(ns("param"), "Parameter:",        
                       choices=levels(factor(df$Parameter)))
    
  })
  
  
# Depending on input$date.option, we'll generate a different UI date component 
  
  output$date.ui <- renderUI({
    
    req(input$site) # See General Note 5
    
    ns <- session$ns
    
    Dates <- df %>% 
      filter(Site %in% input$site) %>%
      .$Date
    
    Date.min <- Dates %>% min(na.rm=TRUE)
    Date.max <- Dates %>% max(na.rm=TRUE)
    
    # Date Input
    
    Months.unique <- levels(factor(month(Dates)))
    Days.unique <- levels(factor(Dates))
    
    switch(input$date.option,
           
           "Calendar Range" = dateRangeInput(ns("date"), "Date Range:", 
                                             start = Date.max - years(1), 
                                             end = Date.max,  
                                             min = Date.min,
                                             max = Date.max,
                                             startview = "year"),
           
           "Select Year" = selectInput(ns("date"), "Year:", 
                                       choices = year(seq(Date.min, Date.max, "years")), 
                                       selected = year(Date.max)),
           
           "Select Month" = selectInput(ns("date"), "Month:", 
                       choices = c(Months.unique), 
                       selected = month(Sys.Date())),
           
           "Select Day" = selectInput(ns("date"), "Year:", 
                                        choices = Days.unique)
           
    )
  })
  
  
# Reactive Data Frames for different date selection methods:
  
  df.react <- reactive({
    
    req(input$site, input$param, input$date) # See General Note 5
    
    if(input$date.option == "Select Year"){
      
        df %>% 
          filter(Parameter %in% input$param,
                 Site %in% c(input$site),
                 year(Date) == input$date)
      
    } else if (input$date.option == "Select Month"){
      df %>% 
        filter(Parameter %in% input$param,
               Site %in% c(input$site),
               month(Date) == input$date)
      
    } else if (input$date.option == "Calendar Range"){
      df %>% 
        filter(Parameter %in% input$param,
               Site %in% c(input$site),
               Date > input$date[1], Date < input$date[2])
      
    } else if (input$date.option == "Select Day"){
  
        df %>% 
          filter(Parameter %in% input$param,
                 Site %in% c(input$site),
                 Date == input$date)
    }
    
  })
  
  
  # Text - Select Site
  
  output$text.site.null <- renderText({
    req(is.null(input$site)) # See General Note 1
    "Select Site(s)"
  })
  
  # Text - Select Parameter
  
  output$text.param.null <- renderText({
    req(!is.null(input$site), is.null(input$param)) # See General Note 1
    "Select Parameter(s)"
  })
  
  # Text - Number of Samples
  
  output$text.num.text <- renderText({
    req(input$site, input$param) # See General Note 1
    "Number of Samples in Selected Data"
  })
  
  # Text - Number of Samples
  
  output$text.num <- renderText({
    req(df.react()) # See General Note 1
    df.react() %>% summarise(n()) %>% paste()
  })
  
# Plot

  callModule(plot.profline.custom, "Plot Profile Line Custom", df = df.react)
  
# Table
  
  output$table.dynamic <- renderDataTable(df.react())
  
  
}