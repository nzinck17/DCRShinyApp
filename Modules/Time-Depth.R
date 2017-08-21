##############################################################################################################################
#     Title: Res-Nutrient-Time.R
#     Type: Module for DCR Shiny App
#     Description: Time Series plots, tables, and summary stats for Reservoir Data
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. 
#
# To-Do List:
#   1. Add Reactive Parameter Units to the Value Bars. This includes removing the units from the dataframe and
#      making a seperate column for the parameter and units
#   2. Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)
#   3. Fix Map to show reservoir locations
#   4. Get data from Database
#   5. Get the location/site/station terminology straight
#   6. Split into transect and nutrient

##############################################################################################################################
# User Interface
##############################################################################################################################

time.depth.UI <- function(id, df) {
  
ns <- NS(id)

tagList(
  wellPanel(
    fluidRow(
      column(3,
             # Location and Depth Selection with Map
             wellPanel(
               checkboxGroupInput(ns("loc"), "Station:", 
                                  choices=levels(factor(df$Loc)),
                                  inline = TRUE),
               checkboxGroupInput(ns("depth"), "Depth:", 
                                  choices=levels(factor(df$Depth)),
                                  inline = TRUE),
               br(),
               leafletOutput(ns("map"), height = 350 )
             ) # end Well Panel
      ),
      column(1),
      column(3,
             # Parameter Selection
             wellPanel(
               uiOutput(ns("param.ui")),
               uiOutput(ns("range.ui"))
             ), # end Well Panel
             br(),
             # Date Selection
             wellPanel(
               uiOutput(ns("date.ui"))
             ), # end Well Panel
             br(), br(), br(),
             # Number of Samples
             wellPanel(
               h2(textOutput(ns("text.num.null")), align = "center"),
               h4(textOutput(ns("text.num.text")), align = "center"),
               h3(textOutput(ns("text.num")), align = "center")
             ) # end well Panel
      ), # end Column
      column(1),
      column(3,
             # Meteoro/Hydro Filter 1
             wellPanel(
               strong("Meteoro/Hydro Filter 1"),
               br(), br(),
               radioButtons(ns("met.option.1"), label = NULL, 
                            choices = c("off", "on"), 
                            inline = TRUE),
               selectInput(ns("met.param.1"), label = NULL, 
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
                           selected = "Wind Speed"),
               sliderInput(ns("met.value.1"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
             ),
             # Meteoro/Hydro Filter 2
             wellPanel(
               strong("Meteoro/Hydro Filter 2"),
               br(), br(),
               radioButtons(ns("met.option.2"), label = NULL, 
                            choices = c("off", "on"), 
                            inline = TRUE),
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
             ) # end Well Panel
      ) # end Column
    ) # end Fluid Row
  ), # Well Panel
  
  # Tabset panel for plots, tables, Summary Stats
  tabsetPanel(
    
    # Plot tab
    tabPanel("Plot", 
             plot.time.depth.UI(ns("Plot"))
    ),
    
    # Table Tab
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
    ), # end tabpanel
    
    # Summary Tab
    tabPanel("Summary",
             column(3,
                    checkboxInput(ns("summary.group.loc"), label = "Group by Location", value = TRUE),
                    checkboxInput(ns("summary.group.depth"), label = "Group by Depth", value = TRUE),
                    radioButtons(ns("summary.group.time"), "Group by:",
                                 choices = c("None" = 1, 
                                             "Year" = 2, 
                                             "Season (all years)" = 3, 
                                             "Month (all years)" = 4, 
                                             "Season (each year)" = 5, 
                                             "month (each year)" = 6),
                                 selected = 1)
             ),
             column(9,
                    tableOutput(ns("summary"))
             ) # end column
    ) # end Tab Panel - "Summary"
  ) # end tabset panel
) # end taglist
} # end UI


##############################################################################################################################
# Server Function
##############################################################################################################################

time.depth <- function(input, output, session, df, df.site) {
  
  
  # Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6
  
  parameters.non.historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  
  # Parameter Selection UI
  
  output$param.ui <- renderUI({
    
    req(input$loc) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    param.choices.new <- df %>%
      filter(Loc %in% c(input$loc)) %>%
      filter(Parameter %in% parameters.non.historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    param.choices.old <- df %>%
      filter(Loc %in% c(input$loc)) %>%
      filter(!(Parameter %in% parameters.non.historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    param.choices <- c(param.choices.new, param.choices.old)
    
    selectInput(ns("param"), "Parameter: ",
                choices=c(param.choices))
    
  })
  
 # Reactive Text - Units of Parameter Selected (for Parameter Range Text)
  
  param.units <- reactive({ 
    df %>%
      filter(Parameter %in% input$param) %>%
      .$Units %>%
      factor() %>%
      levels()
    
  })
  
#Parameter Value Range UI
  
  output$range.ui <- renderUI({
    
    req(input$loc) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    result <- df %>%
      filter(Loc %in% c(input$loc)) %>%
      filter(Parameter %in% input$param) %>%
      .$Result
    
    param.min <- result %>% min(na.rm=TRUE)
    
    param.max <- result %>% max(na.rm=TRUE)
    
    sliderInput(ns("range"), label = paste("Range (", param.units(), ")"), 
                min = param.min, max = param.max,
                value = c(param.min, param.max))
    
  })
  
  
# Date Selection UI
  
  output$date.ui <- renderUI({
    
    req(input$loc) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    Dates <- df %>% 
      filter(Loc %in% c(input$loc)) %>%
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
  
# Reactive Dataframe
  
  df.react <- reactive({
    
    req(input$loc) # See General Note 5
    
    df %>% 
      filter(Loc %in% c(input$loc)) %>%
      filter(Depth %in% c(input$depth)) %>%
      filter(Parameter %in% c(input$param)) %>% 
      filter(Result > input$range[1], Result < input$range[2]) %>%
      filter(Date > input$date[1], Date < input$date[2])

  })
  
  
  # Text - Select Site - Red
  
  output$text.num.null <- renderText({
    req(is.null(input$loc)) # See General Note 1
    "Select a Site"
  })
  
  # Text - Number of Samples
  
  output$text.num.text <- renderText({
    req(input$loc) # See General Note 1
    "Number of Samples in Selected Data"
  })
  
  # Text - Number of Samples
  
  output$text.num <- renderText({
    req(input$loc) # See General Note 1
    df.react() %>% summarise(n()) %>% paste()
  })
  
# Plot

callModule(plot.time.depth, "Plot", df = df.react)
  
# Create Table
  
  output$table <- renderDataTable(df.react())
  
# Create Summary
  
  output$summary <- renderTable({
    
    sum.1 <- df.react() %>%
      mutate(Year = lubridate::year(Date), 
             Season = getSeason(Date),
             Month = lubridate::month(Date)
      )
    
    # group by time
    if (input$summary.group.time == 1){
      sum.dots = c()
    } else if (input$summary.group.time == 2) {
      sum.dots = c("Year")
    } else if (input$summary.group.time == 3) {
      sum.dots = c("Season")
    } else if (input$summary.group.time == 4) {
      sum.dots = c("Month")
    } else if (input$summary.group.time == 5) {
      sum.dots = c("Year", "Season")
    } else if (input$summary.group.time == 6) {
      sum.dots = c("Year", "Month")
    }
    
    # group by Location
    if(input$summary.group.loc == TRUE){
      sum.dots <- c(sum.dots, "Site")
    }
    
    # group by Depth
    if(input$summary.group.depth == TRUE){
      sum.dots <- c(sum.dots, "Depth")
    } 
    
    # Applying Grouping if Grouping selected
    if (input$summary.group.loc == FALSE & input$summary.group.depth == FALSE & input$summary.group.time == 1){
      sum.2 <- sum.1
    } else {
      sum.2 <- sum.1 %>%
        group_by_(.dots = sum.dots)
    }
    
    # Making the Sumamry Statistic Columns
    sum.2 %>% summarise(average = mean(Result), 
                                 min = min(Result), 
                                 max = max(Result), 
                                 median = median(Result), 
                                 variance = var(Result), 
                                 `stand.dev.` = sd(Result),
                                 `number of samples` = n())
  })
  
  
  # Map Color
  
  df.site.react <- reactive({
    df.temp <- df.site %>% filter(!is.na(LocationLat), !is.na(LocationLong))
    df.temp$Selected <- ifelse(df.temp$Loc %in% input$loc, "yes", "no")
    df.temp
  })
  
  colorpal <- reactive({
    colorFactor(c("navy", "red"), domain = c("yes", "no"))
  })
  
  
  # Base Leaflet Map - See General Note 3
  
  output$map <- renderLeaflet({
    
    leaflet(data = df.site %>% filter(!is.na(LocationLat), !is.na(LocationLong))) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(lng = ~LocationLong, lat = ~LocationLat,
                       label=~LocationLabel,
                       popup = ~paste("ID =", Site, "<br/>", 
                                      "Description =", LocationDescription, "<br/>",
                                      "Lat = ", LocationLat, "<br/>", 
                                      "Long = ", LocationLong, "<br/>",
                                      "Elev = ", LocationElevFt, "ft"),
                       radius = 5,
                       weight = 3,
                       opacity = 1,
                       fillOpacity = 0,
                       color = "navy")
  })
  
  
  # Map Proxy - UPdate Color of Circle Markers as Site selection changes
  
  observe({
    
    pal <- colorpal()
    
    leafletProxy("map", data = df.site.react()) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = ~LocationLong, lat = ~LocationLat,
                       label=~LocationLabel,
                       popup = ~paste("ID =", Site, "<br/>", 
                                      "Description =", LocationDescription, "<br/>",
                                      "Lat = ", LocationLat, "<br/>", 
                                      "Long = ", LocationLong, "<br/>",
                                      "Elev = ", LocationElevFt, "ft"),
                       radius = 5,
                       weight = 3,
                       opacity = 1,
                       fillOpacity = 0,
                       color = ~pal(Selected))
  })
  
} # end server
