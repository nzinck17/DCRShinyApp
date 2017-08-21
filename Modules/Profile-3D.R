##############################################################################################################################
#     Title: Profile-3D.R
#     Type: Module for DCR Shiny App
#     Description: 3D Plot for Profile Data
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

prof.3D.UI <- function(id, df) {
  
ns <- NS(id)
  
tagList(
  wellPanel(
    fluidRow(
      # Site Input
      column(2,
             selectInput(ns("site"), "Site:", 
                         choices = levels(factor(df$Site)), 
                         selected = factor(df$Site)[1])
      ),
      # Parameter Input
      column(2,
             uiOutput(ns("param.ui"))
      ),
      # Date Input
      column(2,
             uiOutput(ns("date.ui"))
      ),
      # Date Style Input
      column(2,
             radioButtons(ns("date.style"), "Date Method:",
                          choices = c("Month", "Day of Year" = "DOY"),
                          selected = "Month")
      )          
    ) # end fluid row
  ), # end well panel

  plotlyOutput(ns("plot"), height = 500)

) # end taglist
} # end UI

##############################################################################################################################
# Server Function
##############################################################################################################################

prof.3D <- function(input, output, session, df) {
  
  
  
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
      filter(Site %in% c(input$site)) %>%
      filter(Parameter %in% parameters.non.historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    param.choices.old <- df %>%
      filter(Site %in% c(input$site)) %>%
      filter(!(Parameter %in% parameters.non.historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    param.choices <- c(param.choices.new, param.choices.old)
    
    selectInput(ns("param"), "Parameter: ",
                choices=c(param.choices))
    
  })
  
  
  # Date Selection UI
  
  output$date.ui <- renderUI({
    
    req(input$site) # See General Note _
    
    ns <- session$ns # see General Note 1
    
    Dates <- df %>% 
      filter(Site %in% c(input$site)) %>%
      .$Date
    
    Date.min <- Dates %>% min(na.rm=TRUE)
    Date.max <- Dates %>% max(na.rm=TRUE)
    
    # Date Input
    dateRangeInput(ns("date"), "Date Range:", 
                   start = Date.min, 
                   end = Date.max,  
                   min = Date.min,
                   max = Date.max,
                   startview = "year")
    
  })
  
# Reactive Data Frames:
  
  df.active <- reactive({
    
    df.temp <- df %>% 
      filter(Site %in% c(input$site),
             Parameter %in% input$param,
             Date > input$date[1], Date < input$date[2])
    
    df.temp %>%
      mutate(Year = lubridate::year(Date),
             Month = lubridate::month(Date),
             DOY = as.numeric(strftime(Date, format = "%j")))
  })
  

# Plot - Plotly Visualization
  
  output$plot <- renderPlotly({
    
    plot_ly(df.active(), x = ~Year, y = ~get(input$date.style), z = ~Depthm, color = ~Result,
            marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(15, 15))
    
  })
  
# Plot - print
  
  output$save.plot <- downloadHandler(
    filename = function (){paste(input$date,' ', input$param,' Site ',input$site,'.png', sep='')},
    content = function(file) {ggsave(file, plot = p(), device = "png")},   #function(file) {ggsave(file, plot = p(), device = "png")}
    contentType = 'image/png'
  )
  
} # end server
