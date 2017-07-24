##############################################################################################################################
#     Title: Tributary.R
#     Type: Module for Tributaries
#     Description: This script is the UI and server for the tributary page. Used for "Quabbin", "Ware River", and "Wachusett Tabs"
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

####################################################################################################
# User Interface
#####################################################################################################

Trib.regress.UI <- function(id, df) {

ns <- NS(id)

tagList(
         
         # Well Panel - Upper section of page (everything surrounded by light blue)
         
         wellPanel(
           
           fluidRow(
             
             # first column
             column(3,
                    
                    wellPanel(
                    
                    checkboxGroupInput(ns("site"), "Sites:", 
                                       choices= levels(factor(df$Site)),  #df[df$`Core or EQA` == "Core", "Site"]
                                       selected = factor(df$Site)[1],
                                       inline=TRUE),
                    
                    leafletOutput(ns("map"), height = 450 )
                    
                    )#well
                    
             ),#col

             column(9,
                    
                    fluidRow(
                      
                      column(4,
                             
                             wellPanel(
                               
                               uiOutput(ns("y.param.ui")),
                               
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
                                                
                                                uiOutput(ns("x.param.ui")),
                                                
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
                             )
                             
                      ),
                      column(4,
                             
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
                             )#well
                      ),#col
                      
                      column(4,
                             
                             wellPanel(
                               
                               strong("Meteoro/Hydro Filter 3"),
                               
                               br(), br(),
                               
                               radioButtons(ns("met.option.3"), label = NULL, choices = c("off", "on"), inline = TRUE),
                               
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
         ) # end well panel
) # end taglist

}



##############################################################################################################################
# Server Function
##############################################################################################################################



Trib.regress <- function(input, output, session, df, df.site) {

# Y axis Parameter
  
  #Parameter Selection UI
  
  output$y.param.ui <- renderUI({
    
    ns <- session$ns

    y.param.choices <- df %>%
      filter(Site %in% c(input$site)) %>%
      .$Parameter %>%
      factor() %>% 
      levels()
    
    selectInput(ns("y.param"), "Y-axis Parameter:",        
                choices=c(y.param.choices))

  })
  
  # Reactive Texts
  
  y.param.units <- reactive({ 
    df %>%
      filter(Parameter %in% input$y.param) %>%
      .$Units %>%
      factor() %>%
      levels()
    
  })
  
  #Parameter Value Range UI
  output$y.range.ui <- renderUI({
    
    ns <- session$ns
    
    y.result <- df %>%
      filter(Site %in% c(input$site)) %>%
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
    
    ns <- session$ns
    
    x.param.choices <- df %>%
      filter(Site %in% c(input$site)) %>%
      .$Parameter %>%
      factor() %>% 
      levels()
    
    selectInput(ns("x.param"),label = NULL,        
                choices=c(x.param.choices))
    
  })
  
  # Reactive Texts
  
  x.param.units <- reactive({ 
    df %>%
      filter(Parameter %in% input$x.param) %>%
      .$Units %>%
      factor() %>%
      levels()
    
  })
  
  #Parameter Value Range UI
  output$x.range.ui <- renderUI({
    
    ns <- session$ns
    
    x.result <- df %>%
      filter(Site %in% c(input$site)) %>%
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
    
    ns <- session$ns
    
    Dates <- df %>% 
      filter(Site %in% c(input$site)) %>%
      .$Date
    
    Date.min <- Dates %>% min(na.rm = TRUE)
    Date.max <- Dates %>% max(na.rm = TRUE)
    
    # Date Input
    dateRangeInput(ns("date"), "Date Range:", 
                   start = Date.min, 
                   end = Date.max,
                   min = Date.min,
                   max = Date.max,
                   startview = "year")
    
  })
  

  
  
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
  
  
  
}