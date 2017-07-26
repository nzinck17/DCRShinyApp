##############################################################################################################################
#     Title: Tributary.R
#     Type: Module for Tributaries
#     Description: This script is the UI and server for the Reservior page.
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

#=============================================================================================
# User Interface side

Res.time.UI <- function(id, df) {
  
  ns <- NS(id)
  
  tagList(
    
    wellPanel(
      fluidPage(
        
        # top of page Search Widgets (apply to all plot, table, and statistics)
        column(3,
               
               wellPanel(
               
               checkboxGroupInput(ns("loc"), "Site Location:", 
                                  choices=levels(factor(df$Loc)),
                                  selected = factor(df$Loc[1]),
                                  inline = TRUE),
               
               
               checkboxGroupInput(ns("depth"), "Depth:", 
                                  choices=levels(factor(df$Depth)),
                                  selected = factor(df$Depth[1]),
                                  inline = TRUE),
               br(),
               
               leafletOutput(ns("map"), height = 350 )
               
               )#well

        ),
        
        column(1),
        
        column(3,
               
               wellPanel(
               
               selectInput(ns("param"), "Water Quality Parameter:",        
                           choices=levels(factor(df$Parameter)),
                           selected = factor(df$Parameter[4])),
               
               uiOutput(ns("range.ui"))
               
               ),#well
               
               br(),
               
               wellPanel(
               
                 uiOutput(ns("date.ui")) 
               
               ),#well
               
               br(), br(), br(),
               
               wellPanel(
                 
                 h4("Number of Samples in Selected Data:", align = "center"),
                 
                 h3(textOutput(ns("text.num")), align = "center")
                 
               )#well
               
               ),
        
        column(1),
        
        column(3,
               
               wellPanel(
                 
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
               ),
               
               wellPanel(
                 
                 strong("Meteoro/Hydro Filter 2"),
                 
                 br(), br(),
                 
                 radioButtons(ns("met.option.2"), label = NULL, choices = c("off", "on"), inline = TRUE),
                 
                 selectInput(ns("met.param.2"), label = NULL, choices = c("Wind Speed", 
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
               )
        )
      )
    ),
    
    tabsetPanel(
      
      tabPanel("Plot", 
               plotlyOutput(ns("plot"), width = "100%", height = 600),
               fluidRow(
                 column(2,
                        downloadButton(ns('save.plot'), "Save Plot"),
                        br(),
                        br(),
                        radioButtons(ns("plot.nplots"), "Plot Style:", choices = c("One Plot", "Multiple Plots"))
                 ),
                 column(3,
                        checkboxGroupInput(ns("plot.misc"), "Misc. Plot Options:", 
                                           choices=c("Non-Detection Level",
                                                     "Reporting Limit",
                                                     "Performance Standard",
                                                     "Log Scale (Y-axis)",
                                                     "Show Trendline"),
                                           selected = "Interactive")
                 ),
                 
                 column(3,
                        #make a condition to select multiple plots if other than none is selected
                        radioButtons(ns("plot.color"), "Group By:", 
                                     choices=c("None" = 1, 
                                               "Site" = "Site",
                                               "met/hydro filter 1 (select group)" = "met1",
                                               "met/hydro filter 2 (select group)" = "met2",
                                               "Flagged data" = "FlagCode"),
                                     selected = "Site")
                 ),
                 column(3,
                        radioButtons(ns("plot.shape"), "Flagged Data Display:", 
                                     choices=c("None" = 1, 
                                               "Site" = "Site",
                                               "met/hydro filter 1 (select group)" = "met1",
                                               "met/hydro filter 2 (select group)" = "met2",
                                               "Flagged data" = "FlagCode"),
                                     selected = 1)
                 )
               )
      ),

      tabPanel("Table",
               # first row - print button, etc
               fluidRow(br(),
                        br(),
                        actionButton(ns("table.print"), "Print Table")
               ),
               
               # next row
               fluidRow(
                 dataTableOutput(ns("table"))
               ) # end fluid row
               
      ), # end tabpanel
      
      tabPanel("Summary",
               
               column(3,
                      checkboxInput(ns("summary.group.loc"), label = "Group by Location", value = TRUE),
                      
                      checkboxInput(ns("summary.group.depth"), label = "Group by Depth", value = TRUE),
                      
                      radioButtons(ns("summary.group.time"), "Group by:",
                                   choices = c("None" = 1, "Year" = 2, "Season (all years)" = 3, "Month (all years)" = 4, "Season (each year)" = 5, "month (each year)" = 6),
                                   selected = 1)
               ),
               
               column(9,
                      
                      tableOutput(ns("summary"))
               ) # end column
               
      ) # end "summary" tabpanel
    ) # end tabset panel
    
  ) # end taglist
  
} # end UI


#==========================================================================================
# Server side

Res.time <- function(input, output, session, df, df.site) {
  
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
    
    ns <- session$ns
    
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
    
    ns <- session$ns
    
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
    df %>% 
      filter(Parameter %in% c(input$param)) %>% 
      filter(Loc %in% c(input$loc)) %>%
      filter(Depth %in% c(input$depth)) %>%
      filter(Date > input$date[1], Date < input$date[2])
  })
  
  # Render Text
  
  output$text.num <- renderText({
    
    df.react() %>% summarise(n()) %>% paste()
    
  })
  
# Plot Creation
  
  p <- reactive({
    if(input$plot.nplots == "One Plot"){
      p <- ggplot(df.react(), aes(x = Date, y = Result, color = Site)) + #, color = Year
        geom_point(size = 1) +
        labs(title = paste(df.react()$Parameter[1], 'for Sites:', paste(unique(df.react()$Site), collapse = ", ")),
             x = 'Date', y =  df.react()$Parameter[1]) +
        theme_bw() +
        theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))
    }
    else{    
      p <- ggplot(df.react(), aes(x = Date, y = Result)) + #, color = Year
        geom_point(size = 1) +
        labs(title = paste(df.react()$Parameter[1], 'for Sites:', paste(unique(df.react()$Site), collapse = ", ")),
             x = 'Date', y = df.react()$Parameter[1]) +
        facet_grid(Depth~Loc) +
        theme_bw() +
        theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))
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
    
    
    sum.2 %>% summarise(average = mean(Result), 
                                 min = min(Result), 
                                 max = max(Result), 
                                 median = median(Result), 
                                 variance = var(Result), 
                                 `stand.dev.` = sd(Result),
                                 `number of samples` = n())
  }) # end summary
  
  
  
  # Map Color
  
  df.site.react <- reactive({
    
    df.temp <- df.site %>% filter(!is.na(LocationLat), !is.na(LocationLong))
    
    df.temp$Selected <- ifelse(df.temp$Site %in% input$site, "yes", "no")
    
    df.temp
    
  })
  
  colorpal <- reactive({
    
    colorFactor(c("navy", "red"), domain = c("yes", "no"))
    
  })
  
  # Map (original)
  
  output$map <- renderLeaflet({
    
    leaflet(data = df.site %>% filter(!is.na(LocationLat), !is.na(LocationLong))) %>%
      
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      
      fitBounds(~min(LocationLong), ~min(LocationLat), ~max(LocationLong), ~max(LocationLat)) %>%
      
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
  
  # Map - Update for selected colors
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
