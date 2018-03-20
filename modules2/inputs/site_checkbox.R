##############################################################################################################################
#     Title: SiteCheckbox.R
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

SITE_CHECKBOX_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    fluidRow(
      # Site Selection
      wellPanel(
        CHECKBOX_SELECT_ALL_UI(ns("site_primary")),
        br(),
        uiOutput(ns("site_nonprimary_category_ui")),
        CHECKBOX_SELECT_ALL_UI(ns("site_nonprimary"))
      ) # end Well Panel
    )
  ) # end taglist

} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value.
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

SITE_CHECKBOX <- function(input, output, session, df) {
  
  ns <- session$ns # see General Note 1
  
  ### Site - Primary

  # List
  Site_Primary_Choices <- reactive({
    df %>% 
      filter(grepl("Primary", LocationCategory)) %>%
      .$LocationLabel %>% unique()
  })

  # Server
  Site_Primary <- callModule(CHECKBOX_SELECT_ALL, "site_primary",
                             label =  "Primary Active Sites:",
                             Choices = Site_Primary_Choices)



  ### Site - Non Primary Categories

  # List
  site_nonprimary_category_choices <- reactive({
    df %>% 
      filter(LocationCategory != "Primary Active") %>%
      .$LocationCategory %>% factor(exclude = FALSE) %>% levels()
    })

  # UI
  output$site_nonprimary_category_ui <- renderUI({
    
    checkboxGroupInput(ns("site_nonprimary_category"),
                       label = "Show Other Categories:",
                       choices = site_nonprimary_category_choices())
  })
  


  ### Site - NonPrimary Sites

  # List
  Site_Nonprimary_Choices <- reactive({

    df %>%
      filter(LocationCategory %in% input$site_nonprimary_category) %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
  })

  # Server
  Site_Nonprimary <- callModule(CHECKBOX_SELECT_ALL, "site_nonprimary",
                                label = "Sites:",
                                Choices = Site_Nonprimary_Choices)



  ### Return selected site list "site" from callModule
  return(reactive({c(Site_Primary(), Site_Nonprimary())}))

} # end Server Function

