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
  fluidRow(
    tags$style(type = "text/css", "#Home-map {height: calc(100vh - 210px) !important;}"),
    leafletOutput(ns("map")) #height = 700
  ),
  fluidRow(
    br(),
    column(3, imageOutput(ns("dcr_image"), height = 80), align = "left"),
    column(6, imageOutput(ns("wave_image"), height = 80), align = "center"),
    column(3, imageOutput(ns("umass_image"), height = 80), align = "right")
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
      addPolygons(data = QWW,
                  layerId = QWW,
                  color = "#00008B",
                  weight = 1, smoothFactor = 0.5,
                  opacity = 0.7, fillOpacity = .1,
                  fillColor = "#00008B",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend(
                position = "bottomleft",
                values = ~MapFactor,
                pal = pal,
                opacity = 1,
                na.label = "Not Available",
                title = ""
      )

  })


  # DCR IMAGE
  output$dcr_image <- renderImage({
    list(src = "images/DCR.jpg",
         width= "160",
         height= "80")
  }, deleteFile = FALSE)

  # WAVE IMAGE
  output$wave_image <- renderImage({
    list(src = "images/WAVE.jpg",
          width= "360",
          height= "80")
  }, deleteFile = FALSE)

  # UMass IMAGE
  output$umass_image <- renderImage({
    list(src = "images/UMass.png",
         width= "240",
         height= "80")
  }, deleteFile = FALSE)

} # end Server Function
