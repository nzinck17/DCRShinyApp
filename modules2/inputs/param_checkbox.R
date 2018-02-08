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

PARAM_CHECKBOX_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    # Parameter Selection
    wellPanel(
      uiOutput(ns("type_ui")),
      br(),
      uiOutput(ns("range_ui"))
    ) # end Well Panel
  ) # end taglist
  
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

PARAM_CHECKBOX <- function(input, output, session, Df, selectall = FALSE, colwidth = 3) { 
  

  # Non Historical (when a Parameter has been used  in the last 5 years). See General Note 6
  parameters_non_historical <- reactive({
    Df() %>%
      filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
      .$Parameter %>%
      factor() %>%
      levels()
  })

  
  
  # Parameters which have data at any Site (in the mofule's df) within 5 years.
  param_new_choices <- reactive({
    Df() %>%
      filter(Parameter %in% parameters_non_historical()) %>%
      .$Parameter %>%
      factor() %>%
      levels()
  })

  
  
  # Parameters which do NOT have data at any Site (in the mofule's Df) within 5 years.
  param_old_choices <- reactive({
    Df() %>%
      filter(!(Parameter %in% parameters_non_historical())) %>%
      .$Parameter %>%
      factor() %>%
      levels()
  })

  
  # Combine new and old
  param_choices <- reactive({
    c(param_new_choices(), param_old_choices())
  })
  
  
  # Select All to Start?
  selected <- reactive({
    if(selectall == FALSE){
      NULL
    }else{
      param_choices()
    }
  })
  
  
  # Parameter - Selection UI
  output$type_ui <- renderUI({
    ns <- session$ns # see General Note 1
    CHECKBOX_SELECT_ALL_UI(ns("type"))
  })
  
  Type <- callModule(CHECKBOX_SELECT_ALL, "type",
                     label = "Parameters:",
                     choices = param_choices,
                     selected = selected,
                     colwidth = colwidth)
  
  
  # Parameter Value Range Bar UI
  
  output$range_ui <- renderUI({
    
    ns <- session$ns # see General Note 1
    
    result <- Df() %>%
      filter(Parameter %in% Type()) %>%
      .$Result
    
    param_min <- result %>% min(na.rm=TRUE)
    
    param_max <- result %>% max(na.rm=TRUE)
    
    sliderInput(ns("range"), " Value Range",
                min = param_min, max = param_max,
                value = c(param_min, param_max))
    
  })
  
  # Retuens a list of reactive expressions.
  return(list(Type = reactive({Type()}), # could also just write Type = Type
              Range_Min = reactive({input$range[1]}), 
              Range_Max = reactive({input$range[2]})))

} # end Server Function

