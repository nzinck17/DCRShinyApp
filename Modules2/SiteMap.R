##############################################################################################################################
#     Title: SiteMap.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Site Map for Site Options and Selected Sites
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

sitemap.UI <- function(id, df) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    leafletOutput(ns("map"), height = 350 )
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

sitemap <- function(input, output, session, df.site, site.list) {
  
  # Reactive Site Dataframe for Map Coloring, Creating a Selected Column with "Yes" or "No" values
  
  df.site.react <- reactive({
    df.temp <- df.site %>% filter(!is.na(LocationLat), !is.na(LocationLong))
    df.temp$Selected <- ifelse(df.temp$LocationLabel %in% site.list(), "yes", "no")
    df.temp
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
                       label= ~LocationLabel,
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

