##############################################################################################################################
#     Title: Maplot.R
#     Type: Module for DCR Shiny App
#     Description: Geospatial Plots of Tributary Data and Statistics for a Watershed
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. If Statement makes sure that the dataframe is not empty in order for the colorpal to run. 
#      The aim is to prevent potential crash. Further investigation could be: Is this neccesary?, 
#      If so, better alternative? Do we need to assign an else to colorpal (in the case where no colorpal exists (the first click)).
#      Think it's only neccesary in the latter location
#   2. Factor Scheme in which is For the sizing Range of the Circles. Max Value in the determination of the Scale.
#      This allows unity of the largest circle size (corresponding to the max value of the Statistic data selected. 
#      The formula also has multipliing constant "30" to make the circles a reasonable size, and adds "10" to mainly 
#      make sure one cannot make the circles too small and disappear
#   3. The Base Leaflet Map contains the Map Tiles and the Boudary Lat/Long info. This Base Leaflet Map is not dependent
#      on any reactive objects or inputs (except for the map type input). Therefore the base map should not dissapear
#      and be regerated during the any change in Selected Data or Display settings except for when map type is changed.
#   4. If one changes the preffered initial map type (Tile), one needs to change it in the UI and also in the Base Leaflet  
#      Map section of the Server Function. The reason why the input is not directly used in the Base Leaflet Map section  
#      is becuase the Lat/Long coordinates would be reset, which is likely undesired.
#
# To-Do List:
#   1. Make the CSS more localized
#   2. Make a more precise sig fig method for statistic (i.e. Number of Samples should not be rounded (maybe keep all 
#      non-decimal numbers))

##############################################################################################################################
# User Interface
##############################################################################################################################

map.plot.UI <- function(id, df) { 

ns <- NS(id)

tagList(
    sidebarLayout(
      sidebarPanel(width = 3,
        tabsetPanel(
          # Main Panel Options
          tabPanel("Main",
                   br(), br(), # Line Breaks
                   # Parameter Selection
                   selectInput(ns("param"), "Water Quality Parameter:",        
                               choices=c("-Select Parameter-", levels(factor(df$Parameter))),
                               selected = "-Select Parameter-"),
                   hr(), # Horizontal Rule/Line
                   # Statistic Selection
                   selectInput(ns("stat"), "Value Statistic:",        
                               choices=c("average", "minimum", "maximum", 
                                         "median", "1st quartile", "1st quartile",
                                         "variance", "stand.dev.","number of samples"),
                               selected = "average"),
                   hr(),
                   # Date Selection
                   strong("Date Range:"), # bold text
                   br(), br(),
                   wellPanel(
                   # Year
                     selectInput(ns("year"), "Year:", 
                                 choices = c("All Years", rev(year(seq(as.Date("1990-1-1"), Sys.Date(), "years")))), 
                                 selected = "All Years"),
                     # Month
                     selectInput(ns("month"), "Month:", 
                                 choices = c("All Months",
                                             January = 1,
                                             February = 2,
                                             March = 3,
                                             April = 4,
                                             May = 5,
                                             June = 6,
                                             July = 7,
                                             August = 8,
                                             September = 9,
                                             October = 10,
                                             November = 11,
                                             December = 12), 
                                 selected = "All Months")
                   ) # end Well Panel
          ),
          # Display Panel Options
          tabPanel("Display Options",
                   br(), br(),
                   # Map Style
                   selectInput(ns("map.type"), "Map Style:",        
                               choices=c(providers$Stamen.TonerLite,
                                         providers$CartoDB.Positron,
                                         providers$Esri.NatGeoWorldMap),
                               selected = providers$Stamen.TonerLite), # See Note 4
                   hr(), # Horizontal Rule/Line
                   # Plot Style
                   radioButtons(ns("plot.type"), "Plot Style:",        
                               choices=c("Display by Color", "Display by Size"),
                               selected = "Display by Color"),
                   br(),
                   wellPanel(
                     # Color Scheme (Dependent on the Condition of which Plot Style is Selected, see General Note 2)
                     conditionalPanel(condition = paste0("input['", ns("plot.type"), "'] == 'Display by Color' "),
                                      radioButtons(ns("color.dynamic"), "Color Scheme:",        
                                                   choices=rownames(subset(brewer.pal.info, category %in% c("seq", "div"))),
                                                   inline = TRUE)
                     ),
                     conditionalPanel(condition = paste0("input['", ns("plot.type"), "'] == 'Display by Size' "),
                                      radioButtons(ns("color.static"), "Color:",        
                                                   choices=c("black", "blue", "red"),
                                                   selected = "blue")
                     ),
                     br(),
                     # Radius Slider Bar
                     sliderInput(ns("radius"), "Circle Size:",
                                 min = 0, max = 1,
                                 value=0.5),
                     # Opacity Slider Bar
                     sliderInput(ns("opacity"), "Opacity:",
                                 min = 0, max = 1,
                                 value=0.7)
                   ) # end Well Panel
          ) # end tab - Display options
        ), # end tabset
        br(), br(), br(),
        # Save Button
        downloadButton(ns('save.plot'), "Save Plot")
      ), # end sidebar Panel
      mainPanel(width = 9,
        leafletOutput(ns("map"), height = 800)
      ) # end Main Panel
    ) # end sidebarlayout
  ) # end taglist     
} # end UI function

##############################################################################################################################
# Server Function
##############################################################################################################################

map.plot <- function(input, output, session, df, df.site) {

# Reactive Dataframe for data stats
  
  df.react <- reactive({
    
    # Filter by Parameter and remove NAs
    df.temp <- df %>% 
      filter(!is.na(Result), 
             Parameter %in% input$param)
    
    # Filter by Date
    if(input$year != "All Years"){
      df.temp <- df.temp %>% filter(year(Date) == input$year)
    }
    if (input$month != "All Months"){
      df.temp <- df.temp %>% filter(month(Date) == input$month)
    }
    
    # Group by Site and add any statistics (Make sure this matches with UI options)
    df.temp <- df.temp %>% group_by(Site) %>%
      summarise(average = mean(Result), 
                minimum = min(Result), 
                maximum = max(Result), 
                median = median(Result),
                `1st quartile` = quantile(Result, 0.25),
                `3rd quartile` = quantile(Result, 0.75),
                variance = var(Result), 
                `stand.dev.` = sd(Result),
                `number of samples` = n()) %>%
      # Restructuring the Stat Columns into Two new Columns: "Stat" and "Value"
      gather(Stat, Value, -c(Site))
    
    # Create a more condensed Site Location dataframe (with Lat,lomg,site ID)
    df.site.temp <- df.site %>% 
      select(Site, LocationLat, LocationLong)
    
    # Join the two tables together mathched by site - now includes lat/long info
    df.temp <- inner_join(df.temp, df.site.temp, "Site") %>%
      filter(Stat %in% input$stat,
             !is.na(LocationLat), 
             !is.na(LocationLong))
    
    # Setting a 3 digit sig fig for Statistic Values
    df.temp$Value <- signif(df.temp$Value, 3)
    
    # Assiging df.temp to df.react
    df.temp
    
  }) # end df.react
  
  
# Map - Color Pallete
  
  colorpal <- reactive({
    # If statement to prevent potential cras, See Note 1
    if(nrow(df.react()) > 0){
      colorNumeric(palette = input$color.dynamic, range(df.react()$Value))
    }
    }) # end colorpal

  
# Map - Size "Pallete" - For size legend creation.
  
  value.min <- reactive({
    signif(min(df.react()$Value),3)
  })
  
  value.max <- reactive({
    signif(max(df.react()$Value),3)
  })
  
  # Create 8 circles for the legend. change 8 if desired number is different
  value.list <- reactive({
    signif(seq(value.min(), value.max(), length.out = 8), 3)
  })
  
  # Sizing Scheme for the Circles, See Note 2
  value.scale <- reactive({
    ((as.numeric(input$radius)*30)+10)/sqrt(value.max())
  })
  
  diam.list <- reactive({
    2*value.scale()*sqrt(value.list())
  })
  
  
# Base Leaflet Map - See Note 3
  
  output$map <- renderLeaflet({
    leaflet(data = df.site %>% filter(!is.na(LocationLat), !is.na(LocationLong))) %>%
      addProviderTiles(providers$Stamen.TonerLite,  
                       options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(~min((LocationLong)), ~min(LocationLat), ~max(LocationLong), ~max(LocationLat))
  })
  
  
# Circle Legend function
  
  addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
    colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, 
                             "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
    return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
  }
  
  
# Map Proxy - Add the Circle Markers and Legend to the map. This is Reactive to events, unlike the Base Map.
  
  observe({
    
    # if Data Selected is empty, do not add markers. See Note 1
    if(nrow(df.react()) > 0){
    
      # Color
      if(input$plot.type == "Display by Color"){
       
        pal <- colorpal()                                # load colorpal function
        
        leafletProxy("map", data = df.react()) %>%
          clearTiles() %>%
          addProviderTiles(input$map.type,  
                           options = providerTileOptions(noWrap = TRUE)) %>%
          clearMarkers() %>%
          addCircleMarkers(lng = ~LocationLong, 
                     lat = ~LocationLat,
                     radius = input$radius*15+5,          # user selected opacity
                     weight = .5,                         # weight of the outside circle
                     color = "black",                     # color of the outside circle
                     fillColor = ~pal(Value),             # color inside
                     fillOpacity = input$opacity,         # user selected opacity
                     label= ~as.character(Value),         # show Value when hovering
                     popup = ~Site) %>%                   # Show Site name when clicked
          clearControls() %>%
          addLegend(position = "topright",
                    pal = pal, 
                    values = ~Value,
                    title = input$param,
                    opacity = 1)
      }
      # Size
      if(input$plot.type == "Display by Size"){
        
        leafletProxy("map", data = df.react()) %>%
          clearTiles() %>%
          addProviderTiles(input$map.type,  
                           options = providerTileOptions(noWrap = TRUE)) %>%
          clearMarkers() %>%
          addCircleMarkers(lng = ~LocationLong, 
                           lat = ~LocationLat,
                           radius = ~value.scale()*sqrt(Value), # radius function of Value   
                           weight = 2,                          # weight of the outside circle
                           color = input$color.static,          # Color of the outside circle
                           fillColor = input$color.static,      # User selected fill Color
                           fillOpacity = input$opacity,         # user selected opacity
                           label= ~as.character(Value),         # show Value when hovering
                           popup = ~Site) %>%                   # Show Site name when clicked
          clearControls() %>%
          addLegendCustom(colors = c(input$color.static, input$color.static, input$color.static), 
                          labels = value.list(),
                          sizes = diam.list(),
                          opacity = .7)
      }
    # If no data, then clear the existing circleMarkers and legend  
    } else {
      leafletProxy("map", data = df.react()) %>%
        clearMarkers() %>%
        clearControls()
    }
  })
  
} # end Server Funtion

