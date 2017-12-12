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

# Note that Argument "df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

param.checkbox <- function(input, output, session, df, selectall = FALSE, colwidth = 3) { 
  

  # Non Historical (when a Parameter has been used  in the last 5 years). See General Note 6
  parameters.non.historical <- reactive({
    df() %>%
      filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
      .$Parameter %>%
      factor() %>%
      levels()
  })

  
  
  # Parameters which have data at any Site (in the mofule's df) within 5 years.
  param.new.choices <- reactive({
    df() %>%
      filter(Parameter %in% parameters.non.historical()) %>%
      .$Parameter %>%
      factor() %>%
      levels()
  })

  
  
  # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
  param.old.choices <- reactive({
    df() %>%
      filter(!(Parameter %in% parameters.non.historical())) %>%
      .$Parameter %>%
      factor() %>%
      levels()
  })

  
  # Combine new and old
  param.choices <- reactive({
    c(param.new.choices(), param.old.choices())
  })
  
  
  # Select All to Start?
  selected <- reactive({
    if(selectall == FALSE){
      NULL
    }else{
      param.choices()
    }
  })
  
  
  # Parameter - Selection UI
  output$type.ui <- renderUI({
    ns <- session$ns # see General Note 1
    checkboxSelectAll.UI(ns("type"))
  })
  
  type <- callModule(checkboxSelectAll, "type",
                     label = "Parameters:",
                     choices = param.choices,
                     selected = selected,
                     colwidth = colwidth)
  
  
  # Parameter Value Range Bar UI
  
  output$range.ui <- renderUI({
    
    ns <- session$ns # see General Note 1
    
    result <- df() %>%
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

