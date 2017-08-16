##############################################################################################################################
#     Title: Reservoir-Time.R
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

##############################################################################################################################
# User Interface
##############################################################################################################################

Res.time.UI <- function(id, df) {
  
ns <- NS(id)

tagList(
  wellPanel(
    fluidRow(
      column(3,
             # Location and Depth Selection with Map
             wellPanel(
               checkboxGroupInput(ns("loc"), "Site Location:", 
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
             plotlyOutput(ns("plot"), width = "100%", height = 600),
             fluidRow(
               column(2,
                      downloadButton(ns('save.plot'), "Save Plot"),
                      br(), br(),
                      radioButtons(ns("plot.nplots"), "Plot Style:", choices = c("One Plot", "Multiple Plots"))
               ),
               column(3,
                      checkboxGroupInput(ns("plot.misc"), "Misc. Plot Options:", 
                                         choices=c("Non-Detection Level",
                                                   "Reporting Limit",
                                                   "Performance Standard",
                                                   "Log Scale (Y-axis)",
                                                   "Show Trendline"))
               ),
               column(3,
                      #make a condition to select multiple plots if other than none is selected
                      radioButtons(ns("plot.color"), "Group By:", 
                                   choices = c("None" = 1, 
                                               "Location" = "Loc",
                                               "Depth" = "Depth",
                                               "met/hydro filter 1 (select group)" = "met1",
                                               "met/hydro filter 2 (select group)" = "met2",
                                               "met/hydro filter 3 (select group)" = "met3",
                                               "Flagged data" = "FlagCode"),
                                   selected = "Loc")
               ),
               column(3,
                      radioButtons(ns("plot.shape"), "Flagged Data Display:", 
                                   choices = c("None" = 1, 
                                               "Location" = "Loc",
                                               "Depth" = "Depth",
                                               "met/hydro filter 1 (select group)" = "met1",
                                               "met/hydro filter 2 (select group)" = "met2",
                                               "met/hydro filter 3 (select group)" = "met3",
                                               "Flagged data" = "FlagCode"),
                                   selected = 1)
               ) # end Column
             ) # end Fluid Row
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

Res.time <- function(input, output, session, df, df.site) {
  
  
  # Parameter Selection UI
  
  output$param.ui <- renderUI({
    
    req(input$loc) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    param.choices <- df %>%
      filter(Loc %in% c(input$loc)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    selectInput(ns("param"), "Parameter: ",
                choices=c(param.choices))
    
  })
  
# Reactive Text - Units of Parameter Selected (for Parameter Range Text)
  
 # param.units <- reactive({ 
 #   df %>%
 #     filter(Parameter %in% input$param) %>%
 #     .$Units %>%
 #     factor() %>%
 #     levels()
    
 # })
  
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
    
    sliderInput(ns("range"), label = paste("Range (",  ")"), #param.units(),
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
  
  
# Plot Creation
  
  p <- reactive({
    
    # Features in which all plot options have in common
    p <- ggplot(df.react(), aes(x = Date, y = Result)) +
      labs(x = 'Date', y = paste(input$param)) +    # , " (", param.units(),")", sep= ""
      theme_bw() +
      theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))
    
    # Coloring and Shapes as well as Trendline
    
    # Group by both Color and Shape when both selected
    if(input$plot.color != 1 & input$plot.shape != 1){
      p <- p + geom_point(aes_string(color = input$plot.color, shape = input$plot.shape))
      if("Show Trendline" %in% input$plot.display){
        p <- p + geom_smooth(method = "loess", size = 1.5, aes_string(color = input$plot.color, linetype = input$plot.shape))
      }
    }
    # Group by only Color when only color grouping is selected
    else if (input$plot.color != 1){
      p <- p + geom_point(aes_string(color = input$plot.color))
      if("Show Trendline" %in% input$plot.display){
        p <- p + geom_smooth(method = "loess", size = 1.5, aes_string(color = input$plot.color))
      }
    } 
    # Group by only Shape when only shape grouping is selected 
    else if (input$plot.shape != 1){
      p <- p + geom_point(aes_string(shape = input$plot.shape))
      if("Show Trendline" %in% input$plot.display){
        p <- p + geom_smooth(method = "loess", size = 1.5, aes_string(linetype = input$plot.shape))
      }
    } 
    # No Grouping Selected
    else {
      p <- p + geom_point()
      if("Show Trendline" %in% input$plot.display){
        p <- p + geom_smooth(method = "loess", size = 1.5)
      }
    }
    
    # Facet for Sites if no grouping for site is selected and number of sites is greater than 1
    if(input$plot.color != "Loc" & input$plot.shape != "Loc" & length(c(input$loc)) > 1){
      if(input$plot.color != "Depth" & input$plot.shape != "Depth" & length(c(input$depth)) > 1){
        p <- p + facet_grid(Loc~Depth)
      } else {
        p <- p + facet_grid(Loc~.)
      }
    } else {
      if(input$plot.color != "Depth" & input$plot.shape != "Depth" & length(c(input$depth)) > 1){
        p <- p + facet_grid(.~Depth)
      }
    }
    
    # Log Scale
    if("Log Scale (Y-axis)" %in% input$plot.display){
      p <- p + scale_y_log10()
    }
    
    # Show Non-Detect Level
    if("Non-Detection Level" %in% input$plot.display){
      p <- p + geom_hline(yintercept = 2, linetype = "dashed")
    }
    
    # Show Reprting Limit
    if("Reporting Limit" %in% input$plot.display){
      p <- p + geom_hline(yintercept = 5, linetype = "dashed")
    }
    
    # Performance Standard
    if("Performance Standard" %in% input$plot.display){
      p <- p + geom_hline(yintercept = 3)
    }
    
    p
    
  })
  
# Plot Visual
  
  output$plot <- renderPlotly({

    ggplotly(p())

  })
  
# Plot Print
  
  output$save.plot <- downloadHandler(
    filename = function (){paste(input$param,' Site(s) ', paste(unique(df.react()$Site)),' from ', input$date[1],' to ', input$date[2], '.png', sep='')},
    content = function(file) {ggsave(file, plot = p(), device = "png")},   #function(file) {ggsave(file, plot = p(), device = "png")}
    contentType = 'image/png'
  )
  
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
