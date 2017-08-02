##############################################################################################################################
#     Title: Home.R
#     Type: Module for Tributaries
#     Description: This script is the UI and server for the tributary page. Used for "Quabbin", "Ware River", and "Wachusett Tabs"
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

####################################################################################################
# User Interface
#####################################################################################################

Home.UI <- function(id) {

ns <- NS(id)

tagList(
  fluidRow( 
    leafletOutput(ns("map"), height = 700)
  )
  #wellPanel(
  #         fluidRow(
  #           column(3,
  #                  radioButtons(ns("site"), "Choose Sampling Site Type:",        
  #                              choices=c("Tributary", "Reservoir"))
  #           ) # end column
  #         )
  #)
)


}


##############################################################################################################################
# Server Function
##############################################################################################################################


Home <- function(input, output, session, df.site) {
  
# Map
  
  output$map <- renderLeaflet({
    leaflet(data = df.site) %>%
      addProviderTiles(providers$Esri.WorldImagery,  #  Stamen.TonerLite
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(lng = ~LocationLong, lat = ~LocationLat, 
                 label=~LocationLabel,
                 popup = ~paste("ID =", Site, "<br/>", 
                                "Description =", LocationDescription, "<br/>",
                                "Lat = ", LocationLat, "<br/>", 
                                "Long = ", LocationLong, "<br/>",
                                "Elev = ", LocationElevFt, "ft"))
  })
  
  
  
}