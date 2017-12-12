##############################################################################################################################
#     Title: DateSelect.R
#     Type: Module2 for DCR Shiny App
#     Description: Parameter selection with Historical at end and Select All feature
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:

# To-Do List:

##############################################################################################################################
# User Interface
##############################################################################################################################

date.select.UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    # Parameter Selection
    wellPanel(
      uiOutput(ns("date.ui"))
    ) # end Well Panel
  ) # end taglist
  
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argumetns "df" and "site" need to be reactive expressions, not resolved values. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

date.select <- function(input, output, session, df, site, hidden = FALSE) { 

  # Min and Max Dates for Sites Selected
  
  Date.min <- reactive({
    if(!is.null(site())){
      df() %>% filter(LocationLabel %in% c(site())) %>%
        .$Date %>% min(na.rm=TRUE)
    } else {
      df() %>% .$Date %>% min(na.rm=TRUE)
    }
  })
  
  Date.max <- reactive({
    if(!is.null(site())){
      df() %>% filter(LocationLabel %in% c(site())) %>%
        .$Date %>% max(na.rm=TRUE)
    } else {
      df() %>% .$Date %>% max(na.rm=TRUE)
    }
  })
  
  
  # Date Selection UI
  
  output$date.ui <- renderUI({
    
    ns <- session$ns # see General Note 1

    # Date Input
    dateRangeInput(ns("date"), "Date Range:", 
                   start = Date.min(), 
                   end = Date.max(),  
                   min = Date.min(),
                   max = Date.max(),
                   startview = "year")
    
  })
  
  
  # To fill back in previously selected
  
  observe({
    
    # save the Parameter Type input for when the site selection changes. Isolate so does not cause reactivity
    isolate({
        save.selected.lower <- input$date[1]
        save.selected.upper <- input$date[2]
    })
    
    # If Site list is changed but not empty then generate a Select Input with the... 
    # date range for that site(s) and autoselect previous selected date range
    if(!is.null(site())){
      
      updateDateRangeInput(session, inputId = "date", label = "Date Range:", 
                      start = save.selected.lower,
                      end = save.selected.upper,
                      min=Date.min(),
                      max=Date.max())
    
      # If site list is empty than make a date range of the previously selected date range to save it.
    } else {
      updateDateRangeInput(session, inputId = "date", label = "Date Range:", 
                      start = save.selected.lower,
                      end = save.selected.upper,
                      min= save.selected.lower,
                      max= save.selected.upper)
    }
    
  })
  

  
  return(list(lower = reactive({input$date[1]}), 
              upper = reactive({input$date[2]})))
  

  
} # end Server Function

