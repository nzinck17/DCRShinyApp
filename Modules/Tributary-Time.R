##############################################################################################################################
#     Title: Tributary-Time.R
#     Type: Module for DCR Shiny App
#     Description: Time Series plots, tables, and summary stats for Tributaries
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################
#     General Notes: 
#       1. Shiny Modules have a special required format. Everything in the UI must be surrounded by taglist. Every Input and
#          Output object must be wrapped in ns(__), this gives each object a unique name by locating that name within a
#          namespace identified by the inputted "id". This allows the use of identical locally named Input/output objects,
#          becuse globally they are different. When using RenderUI, one must also use the ns() wrap
#       2. Conditional panels and Shiny modules don't work well due to ns() is an R function
#          and conditional panel condition uses javascript
#
#     To-Do List:
#       1. 
####################################################################################################
# User Interface
#####################################################################################################

Trib.time.UI <- function(id, df) {

ns <- NS(id)

tagList(
  wellPanel(       
    fluidRow(
      column(4,
             wellPanel(
               checkboxGroupInput(ns("site"), "Sites: (Choose 1st)", 
                                  choices= levels(factor(df$Site)),  #df[df$`Core or EQA` == "Core", "Site"]
                                  selected = factor(df$Site[1]),
                                  inline=TRUE),
               br(), br()
             ), # end Well Panel
             leafletOutput(ns("map"), height = 350 )
      ), # end Column
      column(1),
      column(3,      
             wellPanel(
               uiOutput(ns("param.ui")),
               uiOutput(ns("range.ui"))
             ), # end Well Panel
             br(),
             wellPanel(
               uiOutput(ns("date.ui"))
             ), # end Well Panel
             br(),
             wellPanel(
               h4("Number of Samples in Selected Data:", align = "center"),
               h3(textOutput(ns("text.num")), align = "center")
             )#well
      ),#col
      column(1),
      column(3,
             # Meteoro/Hydro Filter 1
             wellPanel(
               strong("Meteoro/Hydro Filter 1"), # Bold Text
               br(), br(),
               radioButtons(ns("met.option.1"), label = NULL, 
                            choices = c("off", "on", "group"), 
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
             ), # end Well Panel
             # Meteoro/Hydro Filter 2
             wellPanel(
               strong("Meteoro/Hydro Filter 2"), # Bold Text
               br(), br(),
               radioButtons(ns("met.option.2"), label = NULL, 
                            choices = c("off", "on", "group"), 
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
      ) # end column
    ) # end fluidrow     
  ), # end well panel
         
  # Tabset panel for plots, tables, etc. 
  tabsetPanel(
    
    # Plot Tab
    tabPanel("Plot", 
             # Plot Output
             plotlyOutput(ns("plot"), width = "100%", height = 600),
             # Plot Options
             fluidRow(br(), br(),
                      column(1),
                      column(1,
                             downloadButton(ns('save.plot'), "Save Plot")
                      ), # end column
                      column(1),
                      column(3,
                             checkboxGroupInput(ns("plot.display"), "Display Options:", 
                                                choices= c("Non-Detection Level",
                                                           "Reporting Limit",
                                                           "Performance Standard",
                                                           "Log Scale (Y-axis)",
                                                           "Show Trendline"))
                      ), # end column
                      #new column
                      column(3,
                             radioButtons(ns("plot.color"), label = "Group with Colors:", 
                                          choices = c("None" = 1, 
                                                      "Site" = "Site",
                                                      "met/hydro filter 1 (select group)" = "met1",
                                                      "met/hydro filter 2 (select group)" = "met2",
                                                      "Flagged data" = "FlagCode"),
                                          selected = "Site")
                      ), # end column
                      # new column
                      column(3,
                             radioButtons(ns("plot.shape"), label = "Group with Shapes:", 
                                          choices = c("None" = 1, 
                                                      "Site" = "Site",
                                                      "met/hydro filter 1 (select group)" = "met1",
                                                      "met/hydro filter 2 (select group)" = "met2",
                                                      "Flagged data" = "FlagCode"),
                                          selected = 1)
                      ) # end column
             ) # end flluid row
    ), # end Tab Panel - Plot
    
    # Table Tabpanel
    tabPanel("Table",
             fluidRow(br(),
                      br(),
                      actionButton(ns("table.print"), "Print Table")
             ), # end Fluid Row
             fluidRow(
               dataTableOutput(ns("table"))
             ) # end Fluid Row
    ), # end Tab Panel - Table
    
    # Summary Tabpanel
    tabPanel("Summary",
             column(3,
                    checkboxInput(ns("summary.group.site"), label = "Group by Site", value = TRUE),
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
    ) # end Tab Panel - Summary
  )  # end tabsetpanel (plots, stats, etc.)
) # end taglist
} # end UI function

##############################################################################################################################
# UI Notes
#   1. 
##############################################################################################################################


##############################################################################################################################
# Server Function
##############################################################################################################################

Trib.time <- function(input, output, session, df, df.site) {
  
# Parameter Selection UI
  
  output$param.ui <- renderUI({
    
    ns <- session$ns # see General Note 1

    param.choices <- df %>%
      filter(Site %in% c(input$site)) %>%
      .$Parameter %>%
      factor() %>% 
      levels()

    selectInput(ns("param"), "Parameter: ",
                choices=c(param.choices))

  })
  
  
# Units Texts for Selected Parameter
  
  param.units <- reactive({ 
    df %>%
      filter(Parameter %in% input$param) %>%
      .$Units %>%
      factor() %>%
      levels()
  })
  
  
# Parameter Value Range Bar UI
  
  output$range.ui <- renderUI({
    
    ns <- session$ns # see General Note 1
    
    result <- df %>%
      filter(Site %in% c(input$site)) %>%
      filter(Parameter %in% input$param) %>%
      .$Result
    
    param.min <- result %>% min(na.rm=TRUE)
    
    param.max <- result %>% max(na.rm=TRUE)
    
    sliderInput(ns("range"), paste("Range (", param.units() , ")"),
                min = param.min, max = param.max,
                value = c(param.min, param.max))
    
  })
  

  
# Date Selection UI
  
  output$date.ui <- renderUI({
    
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
  
  
# Reactive Dataframe - filter for selected site, param, value range, date, and remove rows with NA for Result
  
  df.react <- reactive({
    df %>% 
      filter(Site %in% c(input$site), 
             Parameter %in% input$param, 
             Result > input$range[1], Result < input$range[2],
             Date > input$date[1], Date < input$date[2],
             !is.na(Result))
  })
  
# Number of Selected samples Text
  
  output$text.num <- renderText({
    df.react() %>% summarise(n()) %>% paste()
  })
  
# Plot Creation
  
  p <- reactive({
    
    # Features in which all plot options have in common
    p <- ggplot(df.react(), aes(x = Date, y = Result)) +
      labs(x = 'Date', y = paste(input$param, " (", param.units(),")", sep= "")) +
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
    if(input$plot.color != "Site" & input$plot.shape != "Site" & length(c(input$site)) > 1){
      p <- p + facet_wrap(~Site, ncol = ceiling(length(c(input$site))/4))
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
  
# Plot Visualization - convert plot to interactive plot and create an plot output object
  
  output$plot <- renderPlotly({
    ggplotly(p())
  })
  
  
# Plot Print
  
  output$save.plot <- downloadHandler(
    filename = function (){paste(input$param,' Site(s) ', input$site,' from ', input$date[1],' to ', input$date[2], '.png', sep='')},
    content = function(file) {ggsave(file, plot = p(), device = "png")},
    contentType = 'image/png'
  )
  
  
# Tables
  
  output$table <- renderDataTable(df.react())
  
  
# Summary Statistics
  
  output$summary <- renderTable({
    
    # Add Year, season, and Month Columns
    sum.1 <- df.react() %>%
      mutate(Year = lubridate::year(Date), 
             Season = getSeason(Date),
             Month = lubridate::month(Date)
      )
    
    # Group by time (year, season, month)
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

    # Group by site
    if(input$summary.group.site == TRUE){
      sum.dots <- c(sum.dots, "Site")
    }  
    
    # Applying Grouping (if Grouping selected)
    if (input$summary.group.site == FALSE & input$summary.group.time == 1){
      sum.2 <- sum.1
    } else {
      sum.2 <- sum.1 %>%
        group_by_(.dots = sum.dots)
    }
    
    # Making the Sumamry Statistic Columns
    sum.2 %>% summarise(average = mean(Result), 
                        min = min(Result, na.rm=TRUE), 
                        max = max(Result, na.rm=TRUE), 
                        median = median(Result, na.rm=TRUE), 
                        variance = var(Result, na.rm=TRUE), 
                        `stand.dev.` = sd(Result, na.rm=TRUE),
                        `number of samples` = n())
  })
  
  
# Reactive Site Dataframe for Map Coloring, Creating a Selected Column with "Yes" or "No" values
  
  df.site.react <- reactive({
    df.temp <- df.site %>% filter(!is.na(LocationLat), !is.na(LocationLong))
    df.temp$Selected <- ifelse(df.temp$Site %in% input$site, "yes", "no")
    df.temp
  })
  
  
# Map Color Scheme - Coloring the Selected Site Markers a different color than the unselected 
  
  colorpal <- reactive({
    colorFactor(c("navy", "red"), domain = c("yes", "no"))
  })
  
  
# Base Leaflet Map - See Note 1
  
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
  
  
# # Map Proxy - UPdate Color of Circle Markers as Site selection changes
  
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
  
  
} # end Server Function

##############################################################################################################################
# Server Notes
#   1. The Base Leaflet Map contains the Map Tiles and the Circle Markers for the Site Locations. It is not neccesary to 
#      to specify the starting min/max Lat/Long coordinates becuase the location of the markers tell it where to start.
##############################################################################################################################