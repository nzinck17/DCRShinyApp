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

STATION_LEVEL_CHECKBOX_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    # Site Selection
    wellPanel(
      uiOutput(ns("station_ui"))
    ),
    wellPanel(
      uiOutput(ns("level_ui"))
    ) # end Well Panel
  ) # end taglist
  
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

STATION_LEVEL_CHECKBOX <- function(input, output, session, Df, selectall = FALSE, colwidth = 3) { 
  
  ### Station
  
  # Choice LIST
  Station_Choices <- reactive({
    Df()$Station %>% factor() %>% levels()
  })
  
  Selected1 <- reactive({
    if(selectall == FALSE){
      NULL
    }else{
      Station_Choices()
    }
  })
  
  # UI
  output$station_ui <- renderUI({
    ns <- session$ns # see General Note 1
    CHECKBOX_SELECT_ALL_UI(ns("station"))
  })
  
  # Server
  Station_Selected <- callModule(CHECKBOX_SELECT_ALL, "station",
                                 label = "Station:",
                                 choices = Station_Choices,
                                 selected = Selected1,
                                 colwidth = colwidth)

  
  
  ### Sampling Level
  
  # Choices
  Level_Choices <- reactive({
    Df()$Sampling_Level %>% factor() %>% levels()
  })
    
  Selected2 <- reactive({
    if(selectall == FALSE){
      NULL
    }else{
      level_choices()
    }
  })
  
  # UI
  output$level_ui <- renderUI({
    ns <- session$ns # see General Note 1
    CHECKBOX_SELECT_ALL_UI(ns("level"))
  })
  
  # Server
  Level_Selected <- callModule(CHECKBOX_SELECT_ALL, "level",
                               label = "Sampling Level:",
                               choices = Level_Choices,
                               selected = Selected2,
                               colwidth = colwidth)
  
  
  # Create Site (location Labels) Choices from selected Stations and Levels
  Site_Selected <- reactive({
    
    req(Station_Selected(), Level_Selected())
    
    Df() %>%
      filter(Station %in% Station_Selected(),
             Sampling_Level %in% Level_Selected()) %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
    
  })

  return(reactive({Site_Selected()}))

} # end Server Function

