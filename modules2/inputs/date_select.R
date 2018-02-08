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

DATE_SELECT_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    # Parameter Selection
    wellPanel(
      uiOutput(ns("date_ui"))
    ) # end Well Panel
  ) # end taglist
  
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argumetns "Df" and "Site" need to be reactive expressions, not resolved values. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

DATE_SELECT <- function(input, output, session, Df, Site, hidden = FALSE) { 

  # Min and Max Dates for Sites Selected
  
  Date_Min <- reactive({
    if(!is.null(Site())){
      Df() %>% filter(LocationLabel %in% c(Site())) %>%
        .$Date %>% min(na.rm=TRUE)
    } else {
      Df()$Date %>% min(na.rm=TRUE)
    }
  })
  
  Date_Max <- reactive({
    if(!is.null(Site())){
      Df() %>% filter(LocationLabel %in% c(Site())) %>%
        .$Date %>% max(na.rm=TRUE)
    } else {
      Df()$Date %>% max(na.rm=TRUE)
    }
  })
  
  
  # Date Selection UI
  
  output$date_ui <- renderUI({
    
    ns <- session$ns # see General Note 1

    # Date Input
    dateRangeInput(ns("date"), "Date Range:", 
                   start = Date_Min(), 
                   end = Date_Max(),  
                   min = Date_Min(),
                   max = Date_Max(),
                   startview = "year")
    
  })
  
  
  # To fill back in previously selected
  
  observe({
    
    # save the Parameter Type input for when the Site selection changes. Isolate so does not cause reactivity
    isolate({
        save_selected_lower <- input$date[1]
        save_selected_upper <- input$date[2]
    })
    
    # If Site list is changed but not empty then generate a Select Input with the... 
    # date range for that Site(s) and autoselect previous selected date range
    if(!is.null(Site())){
      
      updateDateRangeInput(session, inputId = "date", label = "Date Range:", 
                      start = save_selected_lower,
                      end = save_selected_upper,
                      min = Date_Min(),
                      max = Date_Max())
    
      # If Site list is empty than make a date range of the previously selected date range to save it.
    } else {
      updateDateRangeInput(session, inputId = "date", label = "Date Range:", 
                      start = save_selected_lower,
                      end = save_selected_upper,
                      min = save_selected_lower,
                      max = save_selected_upper)
    }
    
  })
  

  
  return(list(Lower = reactive({input$date[1]}), 
              Upper = reactive({input$date[2]})))
  

  
} # end Server Function

