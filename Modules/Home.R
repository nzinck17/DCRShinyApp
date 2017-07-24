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
         
         # Well Panel - Upper section of page (everything surrounded by light blue)
         
  # Title
  fluidRow(br(), br(), br(), br(), h3("Water Quality Data Viewer Application"),
           h4("Department of Conservation and Recreation ")),
  
  fluidRow( 
    
    leafletOutput(ns("map"), height = 500)
    
  ),
  
  wellPanel(
           
           fluidRow(
             
             # first column
             column(3,
                    
                    # Parameter Input
                    radioButtons(ns("site"), "Choose Sampling Site Type:",        
                                choices=c("Tributary", "Reservoir", "Tributary - Core", "Tributary - EQA"))
                    
             ) # end column
           )
  )
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