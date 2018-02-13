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
HOME_UI <- function(id) {

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

HOME <- function(input, output, session, df_site) {

  df_site$LocationType[df_site$LocationType == "Nutrient"] <- "Reservoir"

  # to fix duplicated Reservoir (Nutrient locations due to multiple depths)
  df_site$Site[!is.na(df_site$Station)] <- df_site$Station[!is.na(df_site$Station)]
  df_site <- df_site[!duplicated(df_site[,c("Site", "LocationLat", "LocationLong", "LocationCategory")]),]


# levels (Categories) of Colors and Legend

  map_levels <- c("Quabbin Tributary",
                  "Ware River Tributary",
                  "Wachusett Tributary",
                  "Quabbin Transect",
                  "Wachusett Transect",
                  "Quabbin Reservoir",
                  "Wachusett Reservoir")

# Create a new column in df_site for coloring and legend purposes

  df_site2 <- df_site %>%
      mutate(MapFactor = factor(paste(Watershed, LocationType), levels = map_levels))

# Color

  color_pal <- colorFactor(palette = c("firebrick",
                                       "tomato",
                                       "red2",
                                       "orange1",
                                       "orange2",
                                       "yellow1",
                                       "yellow2"),
                           domain = factor(map_levels, levels = map_levels),
                           ordered = TRUE)

# Map

  output$map <- renderLeaflet({

    pal <- color_pal
    QWW <- readOGR("gis/QuabbinWareWachusettWatersheds.shp") %>%
      spTransform(CRS("+proj=longlat +ellps=GRS80"))

    leaflet(data = df_site2) %>%
      setView(lng = -72.0589, lat = 42.43, zoom = 11) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery,  #  Stamen.TonerLite
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%

      addPolygons(data = QWW,
                  layerId = QWW,
                  color = "#00008B",
                  weight = 1, smoothFactor = 0.5,
                  opacity = 0.7, fillOpacity = .1,
                  fillColor = "#00008B",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%

      addCircleMarkers(
                 lng = ~LocationLong, lat = ~LocationLat,
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
      addLegend(
                position = "bottomleft",
                values = ~MapFactor,
                pal = pal,
                opacity = 1,
                na.label = "Not Available",
                title = ""
      )

  })

} # end Server Function
