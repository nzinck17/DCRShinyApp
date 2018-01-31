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

# Note that Argument "df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

station.level.checkbox <- function(input, output, session, df, selectall = FALSE, colwidth = 3) { 
  

  ### Station
  
  # Choices
  station.choices <- reactive({
    df()$Station %>% factor() %>% levels()
  })
  
  selected1 <- reactive({
    if(selectall == FALSE){
      NULL
    }else{
      station.choices()
    }
  })
  
  # UI
  output$station.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("station"))
  })
  
  # Server
  station.selected <- callModule(checkboxSelectAll, "station",
                                 label = "Station:",
                                 choices = station.choices,
                                 selected = selected1,
                                 colwidth = colwidth)

  
  
  ### Sampling Level
  
  # Choices
  level.choices <- reactive({
    df()$Sampling_Level %>% factor() %>% levels()
  })
    
  selected2 <- reactive({
    if(selectall == FALSE){
      NULL
    }else{
      level.choices()
    }
  })
  
  # UI
  output$level.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("level"))
  })
  
  # Server
  level.selected <- callModule(checkboxSelectAll, "level",
                               label = "Sampling Level:",
                               choices = level.choices,
                               selected = selected2,
                               colwidth = colwidth)
  
  
  # Create Site (location Labels) Choices from selected Stations and Levels
  site.selected <- reactive({
    
    req(station.selected(), level.selected())
    
    df() %>%
      filter(Station %in% station.selected(),
             Sampling_Level %in% level.selected()) %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
    
  })

  return(reactive({site.selected()}))

} # end Server Function

