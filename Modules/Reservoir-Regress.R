##############################################################################################################################
#     Title: Reservoir-Regress.R
#     Type: Module for DCR Shiny App
#     Description: Regression plots, tables, and summary stats for Tributaries
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. 
#
# To-Do List:
#   1. Add Reactive Parameter Units to the Value Bars. This includes removing the units from the dataframe and
#      making a seperate column for the parameter and units
#   2. Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)

##############################################################################################################################
# User Interface
##############################################################################################################################

Res.regress.UI <- function(id, df) {
  
ns <- NS(id) # see General Note 1
  
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
               leafletOutput(ns("map"), height = 350 )
             ) # end Well Panel
      ), # end Column
      column(9,
             fluidRow(
               column(4,
                      # Y Parameter Selection
                      wellPanel(
                        uiOutput(ns("y.param.ui")),
                        uiOutput(ns("y.range.ui"))
                      ), # end Well Panel
                      uiOutput(ns("text.site.null.ui"))
               ), # end Column
               column(4,
                      # X Parameter Selection
                      wellPanel(
                        strong("X axis Parameter:"), # bold text
                        radioButtons(ns("x.option"), label = NULL, 
                                     choices = c("Water Quality", "Meteorology or Hydrology"), 
                                     inline = TRUE),
                        # SEE GENERAL NOTE 2
                        conditionalPanel(condition = paste0("input['", ns("x.option"), "'] == 'Water Quality' "),
                                         uiOutput(ns("x.param.ui")),
                                         uiOutput(ns("x.range.ui"))
                        ),# end Conditional Panel
                        conditionalPanel(condition = paste0("input['", ns("x.option"), "'] == 'Meteorology or Hydrology' "),
                                         selectInput(ns("x.met.param"), label = NULL, 
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
                                         sliderInput(ns("x.met.range"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                        ) # end Conditional Panel
                      ) # end Well Panel
               ), # end column
               column(4,
                      # Date Selection
                      wellPanel(
                        uiOutput(ns("date.ui"))
                      ), # end well Panel
                      # Number of samples Selected
                      wellPanel(
                        h4(textOutput(ns("text.num.text")), align = "center"),
                        h3(textOutput(ns("text.num")), align = "center")
                      ) # end Well Panel
               ) # end Column
             ), # end fluid Row
             hr(), # horizontal Rule/Line
             br(),
             fluidRow(
               column(4,
                      # MET/HYDRO FILTER 1
                      wellPanel(
                        strong("Meteoro/Hydro Filter 1"), # bold text
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
                      ) # end Well Panel
               ), # end Column
               column(4,
                      #  MET/HYDRO FILTER 2
                      wellPanel(
                        strong("Meteoro/Hydro Filter 2"), # bold text
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
               ), # end Column
               column(4,
                      #  MET/HYDRO FILTER 3
                      wellPanel(
                        strong("Meteoro/Hydro Filter 3"),
                        br(), br(),
                        radioButtons(ns("met.option.3"), label = NULL, 
                                     choices = c("off", "on", "group"), 
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
                      ) # end Well Panel
               ) # end Column
             ) # end Fluid Row
      ) # end Column
    ) # end Fluid Row
  ), # end Well Panel
  
  # Tabset Panel for plots and tables 
  tabsetPanel(
    
    # the "Plot" tab panel where everything related to the plot goes
    tabPanel("Plot", 
             # the actual plot output
             plotlyOutput(ns("plot"), width = "100%", height = 600),
             # area where all plot specific inputs go
             fluidRow(br(), br(),
                      column(3,
                             downloadButton(ns('save.plot'), "Save Plot"),
                             h5('make sure to save with extension ".png" or ".jpg"'),
                             br(),
                             radioButtons(ns("plot.regress"), "Regression Lines:", 
                                          choices=c("None",
                                                    "Linear",
                                                    "Linear w/ 95% C.I.",
                                                    "Curve",
                                                    "Curve w/ 95% C.I."))
                      ),
                      column(3,
                             checkboxGroupInput(ns("plot.display"), "Plot Display Options:", 
                                          choices=c("Log Scale X-axis",
                                                    "Log Scale Y-axis",
                                                    "Param Y Performance Standard")),
                             sliderInput(ns("plot.opacity"), "Opacity:", min = 0, max = 1, value = 1, step = 0.1),
                             sliderInput(ns("plot.jitter"), "Jitter:", min = 0, max = 1, value = 0, step = 0.1)
                      ),
                      column(3,
                             radioButtons(ns("plot.color"), label = "Group with Colors:", 
                                          choices = c("None" = 1, 
                                                      "Location" = "Loc",
                                                      "Depth" = "Depth",
                                                      "Met/hydro filter 1 (select group)" = "met1",
                                                      "Met/hydro filter 2 (select group)" = "met2",
                                                      "Met/hydro filter 3 (select group)" = "met3",
                                                      "Flagged data" = "FlagCode"),
                                          selected = "Loc")
                      ), # end column
                      # new column
                      column(3,
                             radioButtons(ns("plot.shape"), label = "Group with Shapes:", 
                                          choices = c("None" = 1, 
                                                      "Location" = "Loc",
                                                      "Depth" = "Depth",
                                                      "Met/hydro filter 1 (select group)" = "met1",
                                                      "Met/hydro filter 2 (select group)" = "met2",
                                                      "Met/hydro filter 3 (select group)" = "met3",
                                                      "Flagged data" = "FlagCode"),
                                          selected = 1)
                      ) # end column
             ) # end flluid row
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
} # end UI Function


##############################################################################################################################
# Server Function
##############################################################################################################################

Res.regress <- function(input, output, session, df, df.site) {
  
# Y axis Parameter
  
  #Parameter Selection UI
  
  output$y.param.ui <- renderUI({
    
    req(input$loc) # See General Note _
    
    ns <- session$ns
    
    y.param.choices <- df %>%
      filter(Loc %in% c(input$loc)) %>%
      .$Parameter %>%
      factor() %>% 
      levels()
    
    selectInput(ns("y.param"), "Y-axis Parameter:",        
                choices=c(y.param.choices))
    
  })
  
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
    
    req(input$loc) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
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
  
  # Parameter Selection UI
  
  output$x.param.ui <- renderUI({
    
    req(input$loc) # See General Note _
    
    ns <- session$ns
    
    x.param.choices <- df %>%
      filter(Loc %in% c(input$loc)) %>%
      .$Parameter %>%
      factor() %>% 
      levels()
    
    selectInput(ns("x.param"),label = NULL,        
                choices=c(x.param.choices))
    
  })
  
  # Reactive Text - Units of Parameter Selected (for Parameter Range Text)
  
  #x.param.units <- reactive({ 
  #  df %>%
  #    filter(Parameter %in% input$param) %>%
  #    .$Units %>%
  #    factor() %>%
  #    levels() 
  #})
  
# X Parameter Value Range UI
  
  output$x.range.ui <- renderUI({
    
    req(input$loc) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
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
    
    req(input$loc) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    Dates <- df %>% 
      filter(Loc %in% c(input$loc)) %>%
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
    
    req(input$loc) # See General Note 5
    
    # filter by location, depth, and Date adn save
    df.temp <- df %>% 
      filter(Loc %in% c(input$loc),
             Depth %in% c(input$depth),
             Date > input$date[1], Date < input$date[2])
    
    # X Parameter filter and make modifications
    df.temp.x <-  df.temp %>% 
      filter(Parameter %in% c(input$x.param),
             Result > input$x.range[1], Result < input$x.range[2]) %>%
      rename(x.Parameter = Parameter, x.Result = Result) %>%
      select(Site, Loc, Depth, Date, x.Parameter, x.Result)
    
    # Y Parameter filter and make modifications
    df.temp.y <-  df.temp %>% 
      filter(Parameter %in% c(input$y.param),
             Result > input$y.range[1], Result < input$y.range[2]) %>%
      rename(y.Parameter = Parameter, y.Result = Result) %>%
      select(Site, Loc, Depth, Date, y.Parameter, y.Result)
    
    # Join the two X and Y parameters dataframes (Is Site redundant?)
    inner_join(df.temp.x, df.temp.y, by = c("Site", "Loc", "Depth", "Date"))
    
  })
  
  
  # Text - Select Site
  
  output$text.site.null.ui <- renderUI({
    
    req(is.null(input$loc)) # See General Note 1
    wellPanel(
      h2("Select a Site", align = "center")
    )
    
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
  
  
# Jitter Scheme Factor Calculation 
  
  jitter.x <- reactive({
    input$plot.jitter*IQR(df.react()$x.Result)*0.06
  })
  
  jitter.y <- reactive({
    input$plot.jitter*IQR(df.react()$y.Result)*0.06
  })
  

# Plot Creation
  
  p <- reactive({

    # Features in which all plot options have in common
    p <- ggplot(df.react(), aes(x = x.Result, y = y.Result)) +
      labs(x = paste(input$x.param), y = paste(input$y.param)) +  # need to Add unit display for plot
      theme_bw() +
      theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))
    
    # Coloring and Shapes as well as Trendline
    
    # Group by both Color and Shape when both selected
    if(input$plot.color != 1 & input$plot.shape != 1){
      p <- p + geom_point(aes_string(color = input$plot.color, shape = input$plot.shape),
                          alpha = input$plot.opacity,
                          position = position_jitter(width = jitter.x(), height = jitter.y()))
      if(input$plot.regress == "Linear"){
        p <- p + geom_smooth(method = "lm", se = FALSE, size = 1.5, aes_string(color = input$plot.color, linetype = input$plot.shape))
      }
      if(input$plot.regress == "Linear w/ 95% C.I."){
        p <- p + geom_smooth(method = "lm", size = 1.5, aes_string(color = input$plot.color, linetype = input$plot.shape))
      }
      if(input$plot.regress == "Curve"){
        p <- p + geom_smooth(method = "loess", se = FALSE, size = 1.5, aes_string(color = input$plot.color, linetype = input$plot.shape))
      }
      if(input$plot.regress == "Curve w/ 95% C.I."){
        p <- p + geom_smooth(method = "loess", size = 1.5, aes_string(color = input$plot.color, linetype = input$plot.shape))
      }
    }
    # Group by only Color when only color grouping is selected
    else if (input$plot.color != 1){
      p <- p + geom_point(aes_string(color = input$plot.color),
                          alpha = input$plot.opacity,
                          position = position_jitter(width = jitter.x(), height = jitter.y()))
      if(input$plot.regress == "Linear"){
        p <- p + geom_smooth(method = "lm", se = FALSE, size = 1.5, aes_string(color = input$plot.color))
      }
      if(input$plot.regress == "Linear w/ 95% C.I."){
        p <- p + geom_smooth(method = "lm", size = 1.5, aes_string(color = input$plot.color))
      }
      if(input$plot.regress == "Curve"){
        p <- p + geom_smooth(method = "loess", se = FALSE, size = 1.5, aes_string(color = input$plot.color))
      }
      if(input$plot.regress == "Curve w/ 95% C.I."){
        p <- p + geom_smooth(method = "loess", size = 1.5, aes_string(color = input$plot.color))
      }
    }
    # Group by only Shape when only shape grouping is selected 
    else if (input$plot.shape != 1){
      p <- p + geom_point(aes_string(shape = input$plot.shape),
                          alpha = input$plot.opacity,
                          position = position_jitter(width = jitter.x(), height = jitter.y()))
      if(input$plot.regress == "Linear"){
        p <- p + geom_smooth(method = "lm", se = FALSE, size = 1.5, aes_string(linetype = input$plot.shape))
      }
      if(input$plot.regress == "Linear w/ 95% C.I."){
        p <- p + geom_smooth(method = "lm", size = 1.5, aes_string(linetype = input$plot.shape))
      }
      if(input$plot.regress == "Curve"){
        p <- p + geom_smooth(method = "loess", se = FALSE, size = 1.5, aes_string(linetype = input$plot.shape))
      }
      if(input$plot.regress == "Curve w/ 95% C.I."){
        p <- p + geom_smooth(method = "loess", size = 1.5, aes_string(linetype = input$plot.shape))
      }
    }
    # No Grouping Selected
    else {
      p <- p + geom_point(alpha = input$plot.opacity,
                          position = position_jitter(width = jitter.x(), height = jitter.y()))
      if(input$plot.regress == "Linear"){
        p <- p + geom_smooth(method = "lm", se = FALSE, size = 1.5)
      }
      if(input$plot.regress == "Linear w/ 95% C.I."){
        p <- p + geom_smooth(method = "lm", size = 1.5)
      }
      if(input$plot.regress == "Curve"){
        p <- p + geom_smooth(method = "loess", se = FALSE, size = 1.5)
      }
      if(input$plot.regress == "Curve w/ 95% C.I."){
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
  
  
  # Reactive Site Dataframe for Map Coloring, Creating a Selected Column with "Yes" or "No" values
  
  df.site.react <- reactive({
    df.site.temp <- df.site %>% filter(!is.na(LocationLat), !is.na(LocationLong))
    df.site.temp$Selected <- ifelse(df.site.temp$Loc %in% input$loc, "yes", "no")
    df.site.temp
  })
  
  
  # Map Color Scheme - Coloring the Selected Site Markers a different color than the unselected 
  
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
  
} # end Server Function
