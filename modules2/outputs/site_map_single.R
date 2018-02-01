##############################################################################################################################
#     Title: site_map_single.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Site Map for Site Options and Selected Sites
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

##############################################################################################################################
# User Interface
##############################################################################################################################

Site_Map_Single_UI <- function(id, df) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    leafletOutput(ns("map"), height = 350 )
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

Site_Map_Single <- function(input, output, session, site) {
  
  
  # Base Leaflet Map - See General Note 3
  
  output$map <- renderLeaflet({
    
    req(site())
    
    leaflet(data = site()) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(lng = ~LocationLong, lat = ~LocationLat,
                       label= ~LocationLabel,
                       popup = ~paste("ID =", Site, "<br/>", 
                                      "Description =", LocationDescription, "<br/>",
                                      "Lat = ", LocationLat, "<br/>", 
                                      "Long = ", LocationLong, "<br/>",
                                      "Elev = ", LocationElevFt, "ft")
                 )
    
  })
  
  
} # end Server Function
