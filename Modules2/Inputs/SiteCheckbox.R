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

site.checkbox.UI <- function(id, df) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    # Site Selection
    wellPanel(
      uiOutput(ns("site.primary.ui"))
    ),
    wellPanel(
      uiOutput(ns("site.nonprimary.category.ui")),
      uiOutput(ns("site.nonprimary.ui"))
    ) # end Well Panel
  ) # end taglist
  
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

site.checkbox <- function(input, output, session, df) { 
  


  # Site - Primary
  
  # List
  site.primary.choices <- df %>% filter(LocationCategory == "Primary Active") %>%
    .$LocationLabel %>% factor() %>% levels()
  
  # UI
  output$site.primary.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("site.primary"), "Primary Active Sites:", choices = site.primary.choices)
  })
  
  # Server
  site.primary <- callModule(checkboxSelectAll, "site.primary", choices = site.primary.choices)

  
  
  # Site - Non Primary Categories
  
  # Change LocationCateogory NA to "NA" to show up in App
  df$LocationCategory <- as.character(df$LocationCategory)
  df$LocationCategory[is.na(df$LocationCategory)] <- "NA"
  
  # List
  site.nonprimary.category.choices <- df %>% filter(LocationCategory != "Primary Active") %>%
    .$LocationCategory %>% factor() %>% levels()
  
  # UI
  output$site.nonprimary.category.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("site.nonprimary.category"), "Show Other Categories:", choices = site.nonprimary.category.choices)
  })
  
  # Server
  site.nonprimary.category <- callModule(checkboxSelectAll, "site.nonprimary.category", choices = site.nonprimary.category.choices)
  
  
  
  # Site - NonPrimary Sites
  
  # List
  site.nonprimary.choices <- reactive({
    
    df %>%
      filter(LocationCategory %in% site.nonprimary.category()) %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
  })
  
  # UI
  output$site.nonprimary.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("site.nonprimary"), "Sites:", choices = site.nonprimary.choices())
  })
  
  
  # Server
  site.nonprimary <- callModule(checkboxSelectAll, "site.nonprimary", choices = site.nonprimary.choices())
  
  
  # create a reactive expression for no purpose other than to trigger the eventReactive
  #listen <- reactive({
  #  list(site.primary(), site.nonprimary.category(), site.nonprimary())
  #})
  
  # Create list. eventReactive is used becuase additional to updating on site inputs, 
  # this needs to update on category selection (due to previus req())

  #site <- eventReactive(listen(), {
  #  c(site.primary(), site.nonprimary())
  #})
  
  # Return selected site list "site" from callModule
  return(reactive({c(site.primary(), site.nonprimary())}))

} # end Server Function

