##############################################################################################################################
#     Title: Maplot.R
#     Type: Module for Tributaries
#     Description: This script is the UI and server for the tributary page. Used for "Quabbin", "Ware River", and "Wachusett Tabs"
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

####################################################################################################
# User Interface
#####################################################################################################
# things to check/do
### make sure NA does not through off statistics  # add NROW(na.omit(dataset)) (maybe) omit rows with NAs in LAt,Long,site, and Result
### make sure program doesn't crash when df.active() = 0
### make the CSS more localized
# when I go from year back to claendar I get an error



map.plot.UI <- function(id, df) { 

ns <- NS(id)

tagList(
  
  fluidRow(br(), br(), br(), br()),
      
    sidebarLayout(
      sidebarPanel(width = 3,
                   

                   
        tabsetPanel(
          tabPanel("Main",
                   
                   
                   br(), br(),
                   
                   selectInput(ns("param"), "Water Quality Parameter:",        
                               choices=c("-Select Parameter-", levels(factor(df$Parameter))),
                               selected = "-Select Parameter-"),
                   
                   hr(),
                   
                   
                   # Statistic
                   selectInput(ns("stat"), "Value Statistic:",        
                               choices=c("average", "minimum", "maximum", 
                                         "median", "1st quartile", "1st quartile",
                                         "variance", "stand.dev.","number of samples"),
                               selected = "average"),
                   
                   hr(),
                   
                   # Date Selection
                   
                   strong("Date Range:"),
                   
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
                   )
                   

            
          ),
          tabPanel("Display Options",
                   
                   br(), br(),
                   
                   selectInput(ns("map.type"), "Map Style:",        
                               choices=c(providers$Stamen.TonerLite,
                                         providers$CartoDB.Positron,
                                         providers$Esri.NatGeoWorldMap),
                               selected = providers$Stamen.TonerLite),
                   
                   hr(),

                   
                   radioButtons(ns("plot.type"), "Plot Style:",        
                               choices=c("Display by Color", "Display by Size"),
                               selected = "Display by Color"),
                   
                   br(),

                   wellPanel(
                     
                     # Note: Conditional panels and shiny modules don't work well due to ns() is an R function
                     # and conditional panel condition uses javascript
                   
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
                   
                   sliderInput(ns("radius"), "Circle Size:",
                               min = 0, max = 1,
                               value=0.5),
                   
                   
                   sliderInput(ns("opacity"), "Opacity:",
                               min = 0, max = 1,
                               value=0.7)
                   
                   )
                  
          )
        ),
                   

   
        br(), br(), br(),
        
        downloadButton(ns('save.plot'), "Save Plot")
        
        # need to have action button becuase leaflet and renderUI 
        # do not work well together due to leaflet trying to run 
        # before renderUI has had time to run yet and thus creates an empty dataframe that leaflet tries to read.
        # Action Button is a good fix (if a button to press is not too much of a hastle)
        
      
      ),
      mainPanel(width = 9,
        leafletOutput(ns("map"), height = 800)
      )
        
    ) # end sidebarlayout
  ) # end taglist     


}


##############################################################################################################################
# Server Function
##############################################################################################################################


map.plot <- function(input, output, session, df, df.site) {
  
# Depending on input$date.option, we'll generate a different UI date component 
  

# Reactive Dataframe for data stats, filtered and grouped by Site
  df.active <- reactive({
    
    # filter by parameter selected
    df.temp <- df %>% filter(Parameter %in% input$param)
    
    #filter by data selected depending on date scheme selected
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
      gather(Stat, Value, -c(Site))
    
    # Create a more condensed Site Location dataframe (with Lat,lomg,site ID)
    df.site.temp <- df.site %>% 
      select(Site, LocationLat, LocationLong)
    
    # Join the two tables together mathched by site - now includes lat/long info
    df.temp <- inner_join(df.temp, df.site.temp, "Site") %>%
      filter(Stat %in% input$stat) %>%
      filter(!is.na(LocationLat), !is.na(LocationLong))
    
    df.temp$Value <- signif(df.temp$Value, 3)
    
    df.temp
    
  })
  

# Map - Color Pallete
  
  # if data selected is empty than do not run. Prevents potential crash
  
  colorpal <- reactive({
    
    if(nrow(df.active()) > 0){
      colorNumeric(palette = input$color.dynamic, range(df.active()$Value))
  }
    
    })

  
# Map - Size "Pallete" - For size legend creation.
  
  value.min <- reactive({
    signif(min(df.active()$Value),3)
  })
  
  value.max <- reactive({
    signif(max(df.active()$Value),3)
  })
  
  # Create 8 circles for the legend. change 8 if desired number is different
  value.list <- reactive({
    signif(seq(value.min(), value.max(), length.out = 8), 3)
  })
  
  value.scale <- reactive({
    ((as.numeric(input$radius)*30)+10)/sqrt(value.max())
  })
  
  diam.list <- reactive({
    2*value.scale()*sqrt(value.list())
  })
  

  
  
# Map - Render Map and Background at General Location (Put the starting Map Type here (i.e Stamen.TonerLite))
  
  output$map <- renderLeaflet({
    
    leaflet(data = df.site %>% filter(!is.na(LocationLat), !is.na(LocationLong))) %>%
      addProviderTiles(providers$Stamen.TonerLite,  
                       options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(~min((LocationLong)), ~min(LocationLat), ~max(LocationLong), ~max(LocationLat))

  })
  
# Circle Legend function
  
  addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
    colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
    
    return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
  }
  
# Map - Add the color circles to the map and legend
  
  observe({
    
    
    # if data selected is empty than do not run. Stops it from crashing
    if(nrow(df.active()) > 0){
    
      # Color Plot 
      if(input$plot.type == "Display by Color"){
        
        pal <- colorpal()
        
        leafletProxy("map", data = df.active()) %>%
          
          clearTiles() %>%
          
          addProviderTiles(input$map.type,  
                           options = providerTileOptions(noWrap = TRUE)) %>%
          
          clearMarkers() %>%
          addCircleMarkers(lng = ~LocationLong, 
                     lat = ~LocationLat,
                     radius = input$radius*15+5,               # user selected opacity
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
        
        leafletProxy("map", data = df.active()) %>%
          
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
      
      # if no data clear the existing circleMarkers and legend  
    } else {
      
      leafletProxy("map", data = df.active()) %>%
        
        clearMarkers() %>%
        
        clearControls()
      
    }
                
  })
  

}

