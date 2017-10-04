##############################################################################################################################
#     Title: ParamCheckbox.R
#     Type: Module2 for DCR Shiny App
#     Description: Parameter sCheckbox with Historical at end and Select All feature
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#
# To-Do List:

##############################################################################################################################
# User Interface
##############################################################################################################################

param.checkbox.UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    # Parameter Selection
    wellPanel(
      uiOutput(ns("type.ui")),
      br(),
      uiOutput(ns("range.ui"))
    ) # end Well Panel
  ) # end taglist
  
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

param.checkbox <- function(input, output, session, df) { 
  

  # Non Historical (when a Parameter has been used  in the last 5 years). See General Note 6
  parameters.non.historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  
  # Parameters which have data at any Site (in the mofule's df) within 5 years.
  param.new.choices <- df %>%
    filter(Parameter %in% parameters.non.historical) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  
  # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
  param.old.choices <- df %>%
    filter(!(Parameter %in% parameters.non.historical)) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  # Combine new and old
  param.choices <- c(param.new.choices, param.old.choices)
  
  
  # Parameter - Selection UI
  output$type.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("type"), "Parameter: ", choices=c(param.choices))
  })
  
  type <- callModule(checkboxSelectAll, "type", choices = param.choices, colwidth = 2)
  
  
  # Parameter Value Range Bar UI
  
  output$range.ui <- renderUI({
    
    ns <- session$ns # see General Note 1
    
    result <- df %>%
      filter(Parameter %in% type()) %>%
      .$Result
    
    param.min <- result %>% min(na.rm=TRUE)
    
    param.max <- result %>% max(na.rm=TRUE)
    
    sliderInput(ns("range"), " Value Range",
                min = param.min, max = param.max,
                value = c(param.min, param.max))
    
  })
  
  return(list(type = reactive({type()}),
              range.min = reactive({input$range[1]}), 
              range.max = reactive({input$range[2]})))

} # end Server Function

