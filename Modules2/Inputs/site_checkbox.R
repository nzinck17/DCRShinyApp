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

site.checkbox.UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    fluidRow(
      column(6,
             # Site Selection
             wellPanel(
               uiOutput(ns("site.primary.ui"))
             )
      ),
      column(6,
             wellPanel(
               uiOutput(ns("site.nonprimary.category.ui")),
               uiOutput(ns("site.nonprimary.ui"))
             ) # end Well Panel
      )
    )
  ) # end taglist
  
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

site.checkbox <- function(input, output, session, df, selectall = FALSE, colwidth = 3) { 
  

  ### Site - Primary
  
  # List
  site.primary.choices <- reactive({
    df() %>% filter(LocationCategory == "Primary Active") %>%
    .$LocationLabel %>% factor() %>% levels()
  })
  
  selected <- reactive({
    if(selectall == FALSE){
      NULL
    }else{
      site.primary.choices()
    }
  })

  # UI
  output$site.primary.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("site.primary"))
  })
  
  # Server
  site.primary <- callModule(checkboxSelectAll, "site.primary",
                             label =  "Primary Active Sites:",
                             choices = site.primary.choices,
                             selected = selected,
                             colwidth = colwidth)

  
  
  ### Site - Non Primary Categories
  
  site.nonprimary.category.choices <- reactive({
  
  #df.temp <- df()
  
  # Change LocationCateogory NA to "NA" to show up in App
  

  #df.temp$LocationCategory <- as.character(df.temp$LocationCategory)
  #df.temp$LocationCategory[is.na(df.temp$LocationCategory)] <- "NA"
  
  # List
  df() %>% filter(LocationCategory != "Primary Active") %>%
    .$LocationCategory %>% factor(exclude = FALSE) %>% levels()
  
  })
  
  # UI
  output$site.nonprimary.category.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("site.nonprimary.category"))
  })
  
  # Server
  site.nonprimary.category <- callModule(checkboxSelectAll, "site.nonprimary.category",
                                         label = "Show Other Categories:",
                                         choices = site.nonprimary.category.choices,
                                         colwidth = colwidth)
  
  
  
  ### Site - NonPrimary Sites
  
  # List
  site.nonprimary.choices <- reactive({
    
    df() %>%
      filter(LocationCategory %in% site.nonprimary.category()) %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
  })
  
  # UI
  output$site.nonprimary.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("site.nonprimary"))
  })
  
  
  # Server
  site.nonprimary <- callModule(checkboxSelectAll, "site.nonprimary",
                                label = "Sites:",
                                choices = site.nonprimary.choices,
                                colwidth = colwidth)
  
  
  
  ### Return selected site list "site" from callModule
  return(reactive({c(site.primary(), site.nonprimary())}))

} # end Server Function

