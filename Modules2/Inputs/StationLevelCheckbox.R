##############################################################################################################################
#     Title: StationLevelCheckBox.R
#     Type: Module2 for DCR Shiny App
#     Description: Combined Seleciton Widget for Primary and NonPrimary Site seperation with optional NonPrimary Sites Shown.
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#
# To-Do List:

##############################################################################################################################
# User Interface
##############################################################################################################################

station.level.checkbox.UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    # Site Selection
    wellPanel(
      uiOutput(ns("station.ui"))
    ),
    wellPanel(
      uiOutput(ns("level.ui"))
    ) # end Well Panel
  ) # end taglist
  
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

station.level.checkbox <- function(input, output, session, df) { 
  


  ### Station
  
  # Choices
  station.choices <- df$Station %>% factor() %>% levels()
  
  # UI
  output$station.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("station"), "Station:", choices = station.choices)
  })
  
  # Server
  station.selected <- callModule(checkboxSelectAll, "station", choices = station.choices)

  
  
  ### Sampling Level
  
  # Choices
  level.choices <- df$Sampling_Level %>% factor() %>% levels()
  
  # UI
  output$level.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("level"), "Sampling Level:", choices = level.choices)
  })
  
  # Server
  level.selected <- callModule(checkboxSelectAll, "level", choices = level.choices)
  
  
  # Create Site (location Labels) Choices from selected Stations and Levels
  site.selected <- reactive({
    
    df %>%
      filter(Station %in% input$station,
             Sampling_Level %in% input$level) %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
  })

  return(reactive({c(site.selected())}))

} # end Server Function

