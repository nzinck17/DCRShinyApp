##############################################################################################################################
#     Title: ParamSelect.R
#     Type: Module2 for DCR Shiny App
#     Description: Parameter selection with Historical at end and Select All feature
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#
# To-Do List:

##############################################################################################################################
# User Interface
##############################################################################################################################

param.select.UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    # Parameter Selection
    wellPanel(
      uiOutput(ns("type.ui")),
      uiOutput(ns("range.ui"))
    ) # end Well Panel
  ) # end taglist
  
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Arguments "df"  and "site" need to be reactive expressions, not resolved values. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

param.select <- function(input, output, session, df, site) { 
  

  # Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6

  parameters.non.historical <- reactive({
    df() %>%
      filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
      .$Parameter %>%
      factor() %>%
      levels()
  })

  
  
  # Parameter Choice List
  
  param.choices <- reactive({
    
    if(!is.null(site())){
      
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    param.new.choices <- df() %>%
      filter(LocationLabel %in% c(site()),
             Parameter %in% parameters.non.historical()) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    param.old.choices <- df() %>%
      filter(LocationLabel %in% c(site()),
             !(Parameter %in% parameters.non.historical())) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Cmbine lists (recent parameters first and then old parameters)
    c(param.new.choices, param.old.choices)
    
    }
    
  })
  


    
    
  # Parameter Selection UI
  
  
  output$type.ui <- renderUI({
    ns <- session$ns # see General Note 1
    selectInput(ns("type"), "Parameter:", choices=c(param.choices()), multiple = TRUE)
  })
  
  
  
  # To fill back in previously selected
  
  observe({
    
    # save the Parameter Type input for when the site selection changes. Isolate so does not cause reactivity
    isolate({
        save.selected <- input$type
    })
    
    # If Site list is changed but not empty then generate a Select Input with the... 
    # parameters for that site and autoselect previous selected parameter 
    if(!is.null(site())){
      
      updateSelectInput(session, inputId = "type", label = "Parameter:", 
                        choices=c(param.choices()),
                        selected = save.selected)
    
      # If site list is empty than make a parameter list of just the previously listed item to save it.
    } else {
      updateSelectInput(session, inputId = "type", label = "Parameter:", 
                        choices= save.selected,
                        selected = save.selected)
    }
    
  })
  
  
  # Units Texts for Selected Parameter
  
  units <- reactive({
    
    df() %>%
      filter(Parameter %in% input$type) %>%
      .$Units %>%
      factor() %>%
      levels()
    
  })
  

  
  # Parameter Value Range Bar UI
  
  output$range.ui <- renderUI({
    
    ns <- session$ns # see General Note 1
    
    if(!is.null(site())){
      
      result <- df() %>%
        filter(LocationLabel %in% c(site()),
               Parameter %in% input$type) %>%
        .$Result
      
      param.min <- result %>% min(na.rm=TRUE)
      
      param.max <- result %>% max(na.rm=TRUE)
      
      sliderInput(ns("range"), paste("Range (", units() , ")"),
                  min = param.min, max = param.max,
                  value = c(param.min, param.max))
    
  }
    
  })
  

  return(list(type = reactive({input$type}), 
              units = reactive({units()}), 
              range.min = reactive({input$range[1]}), 
              range.max = reactive({input$range[2]})))
  
  
} # end Server Function

