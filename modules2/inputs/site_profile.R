##############################################################################################################################
#     Title: Site_Profile_UI.R
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

SITE_PROFILE_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    fluidRow(
      column(6,
             # Site Selection
             wellPanel(
               CHECKBOX_SELECT_ALL_UI(ns("site"))
             )
      ),
      column(6,
             wellPanel(
               uiOutput(ns("level_ui"))
             ) # end Well Panel
      )
    ) # end FluidRow
  ) # end taglist

} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value.
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

SITE_PROFILE <- function(input, output, session, Df) {

  ns <- session$ns # see General Note 1

  ### Site

  # Choice LIST
  Site_Choices <- reactive({
    Df()$LocationLabel %>% factor() %>% levels()
  })

  # Server
  Site_Selected <- callModule(CHECKBOX_SELECT_ALL, "site",
                                 label = "Site:",
                                 choices = Site_Choices,
                                 colwidth = 3)



  ### Location Depth

  # Choices
  Max_Depth <- reactive({max(Df()$Depthm)})

  # UI
  output$level_ui <- renderUI({
    sliderInput(ns("level"), "Depth Range", min = 0, max = Max_Depth(), value = c(0, Max_Depth()))
  })


  return(reactive({list(Site = Site_Selected(),
                        Depth_Lower = input$level[1],
                        Depth_Upper = input$level[2])}))

} # end Server Function
