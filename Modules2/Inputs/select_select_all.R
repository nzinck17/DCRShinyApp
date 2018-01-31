##############################################################################################################################
#     Title: SelectInputSelectAll.R
#     Type: Module2 for DCR Shiny App
#     Description: Parameter selection with Historical at end and Select All feature
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:

# To-Do List:

##############################################################################################################################
# User Interface
##############################################################################################################################

selectInputSelectAll.UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    uiOutput(ns("selectinput.ui")),
    uiOutput(ns("actionbuttons"))
  )
}
    
##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argumetns "choices" and "selected" need to be reactive expressions, not resolved values. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

selectInputSelectAll <- function(input, output, session, label, choices, selected = reactive(NULL), colwidth = 3, hidden = FALSE) { 
  
  
  output$selectinput.ui <- renderUI({
    ns <- session$ns # see General Note 1
    selectInput(inputId = ns("selectinput"), label = label, choices = choices(), selected = selected(), multiple = TRUE)
  })
  
  ns <- session$ns # see General Note 1
  
  ### Update the Checkbox basaed on Action Buttons
  observeEvent(input$select.all, {
    updateSelectInput(session = session, inputId = "selectinput", label = NULL, choices = choices(), selected = choices())
  })
  
  observeEvent(input$select.def, {
    updateSelectInput(session = session, inputId = "selectinput", label = NULL, choices = choices(), selected = selected())
  })
  
  observeEvent(input$unselect.all, {
    updateSelectInput(session = session, inputId = "selectinput", label = NULL, choices = choices())
  })
  
  
  # Create Action Buttons to Select All/ Unselect All/ Select Default
  output$actionbuttons <- renderUI({
    # If selected is not NULL or ALL
    if(is.null(selected()) | length(choices()) == length(selected())) {
      # Size Buttons according to column width (input)
      if(colwidth >= 3){
        tagList(
          fluidRow(actionButton(inputId = ns("select.all"), label = "Select All", width = "48%"), 
                   actionButton(inputId = ns("unselect.all"), label = "Unselect All", width = "48%"))
        )
      }else if(colwidth == 2){
        tagList(
          fluidRow(actionButton(inputId = ns("select.all"), label = "Select All", width = "98%")), 
          fluidRow(actionButton(inputId = ns("unselect.all"), label = "Unselect All", width = "98%"))
        )
      } else {
        tagList(
          fluidRow(actionButton(inputId = ns("select.all"), label = "Select All", width = "98%")), 
          fluidRow(actionButton(inputId = ns("unselect.all"), label = "Unselect All", width = "98%"))
        )
      }
      # If Default Selection - Add a third button
    } else {
      if(colwidth >= 3){
        tagList(
          fluidRow(actionButton(inputId = ns("select.all"), label = "Select All", width = "32%"),
                   actionButton(inputId = ns("select.def"), label = "Select Default", width = "32%"), 
                   actionButton(inputId = ns("unselect.all"), label = "Unselect All", width = "32%"))
        )
      } else {
        tagList(
          fluidRow(actionButton(inputId = ns("select.all"), label = "Select All", width = "98%")),
          fluidRow(actionButton(inputId = ns("select.def"), label = "Select Default", width = "98%")), 
          fluidRow(actionButton(inputId = ns("unselect.all"), label = "Unselect All", width = "98%"))
        )
      }
    }
  })
  

  # Getoptions(SuspendedWhenHidden = FALSE) to selectinput.ui so when input are on serperate tabs, uiOUtput (by renderUI) does not suspend
  # this is used in Filter Tab becuase this is hidden when other tabs are open yet use this info
  if(hidden == TRUE){
    outputOptions(output, "selectinput.ui", suspendWhenHidden = FALSE)
  }
  
  
  
  return(reactive({input$selectinput}))
  
}

