##############################################################################################################################
#     Title: Tributary.R
#     Type: Module for Tributaries
#     Description: This script is the UI and server for the Reservior page.
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

#=============================================================================================
# User Interface side

Res.regress.UI <- function(id, df) {
  
ns <- NS(id)
  

tagList(
  
  # Well Panel - Upper section of page (everything surrounded by light blue)
  
  wellPanel(
    
    fluidRow(
      
      # first column
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
               
               leafletOutput(ns("map"), height = 350 )
               
             )#well
             
      ),#col
      
      column(9,
             
             fluidRow(
               
               column(4,
                      
                      wellPanel(
                        
                        selectInput(ns("y.param"), "Water Quality Parameter:",        
                                    choices=levels(factor(df$Parameter)),
                                    selected = factor(df$Parameter[4])),
                        
                        uiOutput(ns("y.range.ui"))
                        
                      )#well
                      
               ),
               
               column(4,
                      
                      wellPanel(
                        
                        strong("X axis Parameter:"),
                        
                        radioButtons(ns("x.option"), label = NULL, choices = c("Water Quality", "Meteorology or Hydrology"), inline = TRUE),
                        
                        # Note: Conditional panels and shiny modules don't work well due to ns() is an R function
                        # and conditional panel condition uses javascript
                        
                        conditionalPanel(condition = paste0("input['", ns("x.option"), "'] == 'Water Quality' "),
                                         
                                         selectInput(ns("x.param"), "Water Quality Parameter:",        
                                                     choices=levels(factor(df$Parameter)),
                                                     selected = factor(df$Parameter[4])),
                                         
                                         uiOutput(ns("x.range.ui"))
                        ),
                        
                        
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
                                         
                        )
                        
                      ) #well
                      
               ),
               
               column(4,
                      
                      wellPanel(
                        
                        uiOutput(ns("date.ui")) 
                        
                      ),#well
                      
                      wellPanel(
                        
                        h4("Number of Samples in Selected Data:", align = "center"),
                        
                        h3(textOutput(ns("text.num")), align = "center")
                        
                      )#well
                      
               )#col
             ),#fluidrow
             
             hr(),
             br(),
             
             fluidRow(
               
               
               
               # new column
               column(4,
                      
                      wellPanel(
                        
                        strong("Meteoro/Hydro Filter 1"),
                        
                        br(), br(),
                        
                        radioButtons(ns("met.option.1"), label = NULL, choices = c("off", "on", "group"), inline = TRUE),
                        
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
                      )
                      
               ),
               column(4,
                      
                      wellPanel(
                        
                        strong("Meteoro/Hydro Filter 2"),
                        
                        br(), br(),
                        
                        radioButtons(ns("met.option.2"), label = NULL, choices = c("off", "on", "group"), inline = TRUE),
                        
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
                      )#well
               ),#col
               
               column(4,
                      
                      wellPanel(
                        
                        strong("Meteoro/Hydro Filter 3"),
                        
                        br(), br(),
                        
                        radioButtons(ns("met.option.3"), label = NULL, choices = c("off", "on", "group"), inline = TRUE),
                        
                        selectInput(ns("met.param.3"), label = NULL, choices = c("Wind Speed", 
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
                        
                      )#well
               )#col
               
             )#fluidrow
      )#col
    )#fluidrow
  ), # end well panel
  
  # New tabset panel for plots, tables, etc. 
  tabsetPanel(
    
    # the "Plot" tab panel where everything related to the plot goes
    tabPanel("Plot", 
             # the actual plot output
             plotlyOutput(ns("plot"), width = "100%", height = 600),
             # area where all plot specific inputs go
             fluidRow(br(), br(),
                      column(2,
                             downloadButton(ns('save.plot'), "Save Plot"),
                             h5('make sure to save with extension ".png" or ".jpg"')
                      ), # end column
                      column(2,
                             radioButtons(ns("plot.regress"), "Regression Lines:", 
                                          choices=c("None",
                                                    "Linear",
                                                    "Linear w/ 95% C.I.",
                                                    "Curve",
                                                    "Curve w/ 95% C.I."))
                      ),
                      column(2,
                             checkboxGroupInput(ns("plot.display"), "Display Options:", 
                                          choices=c("Log Scale X-axis",
                                                    "Log Scale Y-axis",
                                                    "Param Y Performance Standard"))
                      ),
                      column(3,
                             radioButtons(ns("plot.color"), label = "Group with Colors:", 
                                          choices = c("None" = 1, 
                                                      "Location" = "Loc",
                                                      "Depth" = "Depth",
                                                      "met/hydro filter 1 (select group)" = "met1",
                                                      "met/hydro filter 2 (select group)" = "met2",
                                                      "met/hydro filter 3 (select group)" = "met3",
                                                      "Flagged data" = "FlagCode"),
                                          selected = "Loc")
                      ), # end column
                      # new column
                      column(3,
                             radioButtons(ns("plot.shape"), label = "Group with Shapes:", 
                                          choices = c("None" = 1, 
                                                      "Location" = "Loc",
                                                      "Depth" = "Depth",
                                                      "met/hydro filter 1 (select group)" = "met1",
                                                      "met/hydro filter 2 (select group)" = "met2",
                                                      "met/hydro filter 3 (select group)" = "met3",
                                                      "Flagged data" = "FlagCode"),
                                          selected = 1)
                      ) # end column
             ) # end flluid row
    ), # end "plot" tabpanel
    
    # "table" tabpanel
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
    ) # end tabpanel

  )  # end tabsetpanel (plots, stats, etc.)
  
) # end taglist
  
} # end UI


#==========================================================================================
# Server side

Res.regress <- function(input, output, session, df, df.site) {
  
# Y Parameter
  
  # Reactive Text - Units of Parameter Selected (for Parameter Range Text)
  
  #y.param.units <- reactive({ 
  #  df %>%
  #    filter(Parameter %in% input$param) %>%
  #    .$Units %>%
  #    factor() %>%
  #    levels()
  #})
  
  #Parameter Value Range UI
  
  output$y.range.ui <- renderUI({
    
    ns <- session$ns
    
    y.result <- df %>%
      filter(Loc %in% c(input$loc)) %>%
      filter(Parameter %in% input$y.param) %>%
      .$Result
    
    y.param.min <- y.result %>% min(na.rm=TRUE)
    
    y.param.max <- y.result %>% max(na.rm=TRUE)
    
    sliderInput(ns("y.range"), paste("Range (", ")"), #y.param.units() ,
                min = y.param.min, max = y.param.max,
                value = c(y.param.min, y.param.max))
    
  })
  
  
# X Parameter
  
  # Reactive Text - Units of Parameter Selected (for Parameter Range Text)
  
  #x.param.units <- reactive({ 
  #  df %>%
  #    filter(Parameter %in% input$param) %>%
  #    .$Units %>%
  #    factor() %>%
  #    levels() 
  #})
  
  #Parameter Value Range UI
  
  output$x.range.ui <- renderUI({
    
    ns <- session$ns
    
    x.result <- df %>%
      filter(Loc %in% c(input$loc)) %>%
      filter(Parameter %in% input$x.param) %>%
      .$Result
    
    x.param.min <- x.result %>% min(na.rm=TRUE)
    
    x.param.max <- x.result %>% max(na.rm=TRUE)
    
    sliderInput(ns("x.range"), paste("Range (", ")"), #x.param.units() ,
                min = x.param.min, max = x.param.max,
                value = c(x.param.min, x.param.max))
    
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
    
    df.temp <- df %>% 
      filter(Loc %in% c(input$loc)) %>%
      filter(Depth %in% c(input$depth)) %>%
      filter(Date > input$date[1], Date < input$date[2])
    
    df.temp.x <-  df.temp %>% 
      filter(Parameter %in% c(input$x.param)) %>%
      filter(Result > input$x.range[1], Result < input$x.range[2]) %>%
      rename(x.Parameter = Parameter, x.Result = Result) %>%
      select(Site, Loc, Depth, Date, x.Parameter, x.Result)
    
    df.temp.y <-  df.temp %>% 
      filter(Parameter %in% c(input$y.param)) %>%
      filter(Result > input$y.range[1], Result < input$y.range[2]) %>%
      rename(y.Parameter = Parameter, y.Result = Result) %>%
      select(Site, Loc, Depth, Date, y.Parameter, y.Result)
    
    inner_join(df.temp.x, df.temp.y, by = c("Site", "Loc", "Depth", "Date"))
    
  })
  
  # Render Text - For the Number of Samples Collected Output
  
  output$text.num <- renderText({
    
    df.react() %>% summarise(n()) %>% paste()
    
  })
  
  # Plot Creation
  
  p <- reactive({

    # features in which all plot options have in common
    p <- ggplot(df.react(), aes(x = x.Result, y = y.Result)) +
      labs(x = paste(input$x.param), y = paste(input$y.param)) +  # need to Add unit display for plot
      theme_bw() +
      theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))
    
    # Display Options
    
    # Group by both Color and Shape when both selected
    if(input$plot.color != 1 & input$plot.shape != 1){
      p <- p + geom_point(aes_string(color = input$plot.color, shape = input$plot.shape))
      
      # Linear Regression Line - Grouped appropriately by Color and Linetype (instead of color and shape)
      if(input$plot.regress == "Linear"){
        p <- p + geom_smooth(method = "lm", se = FALSE, size = 1.5, aes_string(color = input$plot.color, linetype = input$plot.shape))
      }
      
      # Linear Regression Line w/ C.I.
      if(input$plot.regress == "Linear w/ 95% C.I."){
        p <- p + geom_smooth(method = "lm", size = 1.5, aes_string(color = input$plot.color, linetype = input$plot.shape))
      }
      
      # Curve Regression Line
      if(input$plot.regress == "Curve"){
        p <- p + geom_smooth(method = "loess", se = FALSE, size = 1.5, aes_string(color = input$plot.color, linetype = input$plot.shape))
      }
      
      # Curve Regression Line w/ C.I.
      if(input$plot.regress == "Curve w/ 95% C.I."){
        p <- p + geom_smooth(method = "loess", size = 1.5, aes_string(color = input$plot.color, linetype = input$plot.shape))
      }
    
    # Group by only Color when only color grouping is selected
    } else if (input$plot.color != 1){
      p <- p + geom_point(aes_string(color = input$plot.color))
      
      # Linear Regression Line
      if(input$plot.regress == "Linear"){
        p <- p + geom_smooth(method = "lm", se = FALSE, size = 1.5, aes_string(color = input$plot.color))
      }
      
      # Linear Regression Line w/ C.I.
      if(input$plot.regress == "Linear w/ 95% C.I."){
        p <- p + geom_smooth(method = "lm", size = 1.5, aes_string(color = input$plot.color))
      }
      
      # Curve Regression Line
      if(input$plot.regress == "Curve"){
        p <- p + geom_smooth(method = "loess", se = FALSE, size = 1.5, aes_string(color = input$plot.color))
      }
      
      # Curve Regression Line w/ C.I.
      if(input$plot.regress == "Curve w/ 95% C.I."){
        p <- p + geom_smooth(method = "loess", size = 1.5, aes_string(color = input$plot.color))
      }
      
    } else if (input$plot.shape != 1){
      p <- p + geom_point(aes_string(shape = input$plot.shape))
      
      # Linear Regression Line - Grouped appropriately by Color and Linetype (instead of color and shape)
      if(input$plot.regress == "Linear"){
        p <- p + geom_smooth(method = "lm", se = FALSE, size = 1.5, aes_string(linetype = input$plot.shape))
      }
      
      # Linear Regression Line w/ C.I.
      if(input$plot.regress == "Linear w/ 95% C.I."){
        p <- p + geom_smooth(method = "lm", size = 1.5, aes_string(linetype = input$plot.shape))
      }
      
      # Curve Regression Line
      if(input$plot.regress == "Curve"){
        p <- p + geom_smooth(method = "loess", se = FALSE, size = 1.5, aes_string(linetype = input$plot.shape))
      }
      
      # Curve Regression Line w/ C.I.
      if(input$plot.regress == "Curve w/ 95% C.I."){
        p <- p + geom_smooth(method = "loess", size = 1.5, aes_string(linetype = input$plot.shape))
      }
      
    } else {
      p <- p + geom_point()
      
      # Linear Regression Line - Grouped appropriately by Color and Linetype (instead of color and shape)
      if(input$plot.regress == "Linear"){
        p <- p + geom_smooth(method = "lm", se = FALSE, size = 1.5)
      }
      
      # Linear Regression Line w/ C.I.
      if(input$plot.regress == "Linear w/ 95% C.I."){
        p <- p + geom_smooth(method = "lm", size = 1.5)
      }
      
      # Curve Regression Line
      if(input$plot.regress == "Curve"){
        p <- p + geom_smooth(method = "loess", se = FALSE, size = 1.5)
      }
      
      # Curve Regression Line w/ C.I.
      if(input$plot.regress == "Curve w/ 95% C.I."){
        p <- p + geom_smooth(method = "loess", size = 1.5)
      }
    }
    
    # facet for Sites if no grouping for site is selected and number of sites is greater than 1
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
    
    # Log Scale Options
    if("Log Scale X-axis" %in% input$plot.display){
      p <- p + scale_x_log10()
    }
    
    if("Log Scale Y-axis" %in% input$plot.display){
      p <- p + scale_y_log10()
    }
    
    p
    
  })
  
  # Plot Visual
  
  output$plot <- renderPlotly({
    
    ggplotly(p())
    
  })
  
  # Plot Print
  
  output$save.plot <- downloadHandler(
    filename = function (){paste(input$y.param,' Site(s) ', paste(unique(df.react()$Loc)),' from ', input$date[1],' to ', input$date[2], '.png', sep='')},
    content = function(file) {ggsave(file, plot = p(), device = "png")}, 
    contentType = 'image/png'
  )
  
  
  # Tables
  
  output$table <- renderDataTable(df.react())
  
  # Map Color
  
  df.site.react <- reactive({
    
    df.site.temp <- df.site %>% filter(!is.na(LocationLat), !is.na(LocationLong))
    
    df.site.temp$Selected <- ifelse(df.site.temp$Site %in% input$site, "yes", "no")
    
    df.site.temp
    
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
