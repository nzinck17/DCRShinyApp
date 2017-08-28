##############################################################################################################################
#     Title: Maplot.R
#     Type: Module for DCR Shiny App
#     Description: Geospatial Plots of Tributary Data and Statistics for a Watershed
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. 
#
# To-Do List:
#   1. Add watershed delineations
#   2. Make use of stations for nutrient data for no overlap
#   3. 

##############################################################################################################################
# User Interface
##############################################################################################################################
Home.UI <- function(id) {

ns <- NS(id)

tagList(
  
  # CSS for map / map legend
  tags$head(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}",
               ".leaflet .legend i{
               border-radius:50%;
               width: 10px;
               height: 10px;
               margin-top: 4px;
                }
          ") # end tags style
  ), # end tags head
  # end CSS
  
  fluidRow( 
    leafletOutput(ns("map"), height = 700)
  ),
  fluidRow( 
    br(), h3("click on a site for info")
  )
)


}


##############################################################################################################################
# Server Function
##############################################################################################################################

Home <- function(input, output, session, df.site) {

  df.site$LocationType[df.site$LocationType == "Nutrient"] <- "Reservoir"
  
# levels (Categories) of Colors and Legend
  
  map.levels <- c("Quabbin Tributary",
                  "Ware River Tributary", 
                  "Wachusett Tributary",
                  "Quabbin Transect",
                  "Wachusett Transect",
                  "Quabbin Reservoir",
                  "Wachusett Reservoir")
  
# Create a new column in df.site for coloring and legend purposes
  
  df.site.react <- reactive({
    df.site.temp <- df.site %>%
      mutate(MapFactor = paste(Watershed, LocationType))

    df.site.temp$MapFactor <- factor(df.site.temp$MapFactor, 
                                        levels = map.levels)
    df.site.temp
  })
  
# Color
  
  colorpal <- reactive({
    
    colorFactor(palette = c("firebrick", 
                            "tomato", 
                            "red2",
                            "orange1",
                            "orange2",
                            "yellow1",
                            "yellow2"),
                domain = factor(map.levels,
                                levels = map.levels),
                ordered = TRUE)
  })  
  
   
# Map
  
  output$map <- renderLeaflet({
    
    pal <- colorpal()
    
    leaflet(data = df.site.react()) %>%
      addProviderTiles(providers$Esri.WorldImagery,  #  Stamen.TonerLite
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(lng = ~LocationLong, lat = ~LocationLat, 
                 label=~LocationLabel,
                 popup = ~paste("ID =", Site, "<br/>", 
                                "Description =", LocationDescription, "<br/>",
                                "Lat = ", LocationLat, "<br/>", 
                                "Long = ", LocationLong, "<br/>",
                                "Elev = ", LocationElevFt, "ft"),
                 color = ~pal(MapFactor),
                 radius = 5,
                 weight = 3,
                 opacity = 0.8,
                 fillOpacity = 0.4
      ) %>%
      addLegend(position = "bottomleft",
                values = ~MapFactor,
                pal = pal,
                opacity = 1,
                na.label = "Not Available",
                title = ""
      )
      
  })
  

} # end Server Function