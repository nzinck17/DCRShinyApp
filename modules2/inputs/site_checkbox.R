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
      column(6,
             # Site Selection
             wellPanel(
               uiOutput(ns("site_primary_ui"))
             )
      ),
      column(6,
             wellPanel(
               uiOutput(ns("site_nonprimary_category_ui")),
               uiOutput(ns("site_nonprimary_ui"))
             ) # end Well Panel
      )
    )
  ) # end taglist

} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value.
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

SITE_CHECKBOX <- function(input, output, session, Df) {


  ### Site - Primary

  # List
  Site_Primary_Choices <- reactive({
    # Filter Site List for when primary is in the Location Category name
    Df() %>% filter(grepl("Primary", LocationCategory)) %>%
    .$LocationLabel %>% factor() %>% levels()
  })

  # UI
  output$site_primary_ui <- renderUI({
    ns <- session$ns # see General Note 1
    CHECKBOX_SELECT_ALL_UI(ns("site_primary"))
  })

  # Server
  Site_Primary <- callModule(CHECKBOX_SELECT_ALL, "site_primary",
                             label =  "Primary Active Sites:",
                             choices = Site_Primary_Choices)



  ### Site - Non Primary Categories

  # List
  Site_Nonprimary_Category_Choices <- reactive({
    Df() %>% filter(LocationCategory != "Primary Active") %>%
      .$LocationCategory %>% factor(exclude = FALSE) %>% levels()
  })

  # UI
  output$site_nonprimary_category_ui <- renderUI({
    ns <- session$ns # see General Note 1
    CHECKBOX_SELECT_ALL_UI(ns("site_nonprimary_category"))
  })

  # Server
  Site_Nonprimary_Category <- callModule(CHECKBOX_SELECT_ALL, "site_nonprimary_category",
                                         label = "Show Other Categories:",
                                         choices = Site_Nonprimary_Category_Choices)



  ### Site - NonPrimary Sites

  # List
  Site_Nonprimary_Choices <- reactive({

    Df() %>%
      filter(LocationCategory %in% Site_Nonprimary_Category()) %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
  })

  # UI
  output$site_nonprimary_ui <- renderUI({
    ns <- session$ns # see General Note 1
    CHECKBOX_SELECT_ALL_UI(ns("site_nonprimary"))
  })


  # Server
  Site_Nonprimary <- callModule(CHECKBOX_SELECT_ALL, "site_nonprimary",
                                label = "Sites:",
                                choices = Site_Nonprimary_Choices)



  ### Return selected site list "site" from callModule
  return(reactive({c(Site_Primary(), Site_Nonprimary())}))

} # end Server Function

