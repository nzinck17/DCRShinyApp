
checkboxSelectAll.UI <- function(id, label, choices, selected = NULL) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    checkboxGroupInput(inputId = ns("checkbox"), label = label, choices = choices, selected = selected),
    uiOutput(ns("actionbuttons"))
  )
}
    
    

checkboxSelectAll <- function(input, output, session, choices, selected = NULL, colwidth = 3) { 
  
  ns <- session$ns # see General Note 1
  
  # If No Default Selection
  if(is.null(selected)) {
    
    if(colwidth >= 3){
      output$actionbuttons <- renderUI({
        tagList(
          fluidRow(actionButton(inputId = ns("select.all"), label = "Select All", width = "48%"), 
                   actionButton(inputId = ns("unselect.all"), label = "Unselect All", width = "48%"))
        )
      })
    } else if(colwidth == 2){
      output$actionbuttons <- renderUI({
        tagList(
          fluidRow(actionButton(inputId = ns("select.all"), label = "Select All", width = "98%")), 
          fluidRow(actionButton(inputId = ns("unselect.all"), label = "Unselect All", width = "98%"))
        )
      })
    } else {
      output$actionbuttons <- renderUI({
        tagList(
          fluidRow(actionButton(inputId = ns("select.all"), label = "Select All", width = "98%")), 
          fluidRow(actionButton(inputId = ns("unselect.all"), label = "Unselect All", width = "98%"))
        )
      })
    }
  
  # If Default Selection
  } else {
    
    if(colwidth >= 3){
      output$actionbuttons <- renderUI({
        tagList(
          fluidRow(actionButton(inputId = ns("select.all"), label = "Select All", width = "32%"),
                   actionButton(inputId = ns("select.def"), label = "Select Default", width = "32%"), 
                   actionButton(inputId = ns("unselect.all"), label = "Unselect All", width = "32%"))
        )
      })
    } else if(colwidth >= 3){
      output$actionbuttons <- renderUI({
        tagList(
          fluidRow(actionButton(inputId = ns("select.all"), label = "Select All", width = "98%")),
          fluidRow(actionButton(inputId = ns("select.def"), label = "Select Default", width = "98%")), 
          fluidRow(actionButton(inputId = ns("unselect.all"), label = "Unselect All", width = "98%"))
        )
      })
    } else {
      output$actionbuttons <- renderUI({
        tagList(
          fluidRow(actionButton(inputId = ns("select.all"), label = "Select All", width = "98%")),
          fluidRow(actionButton(inputId = ns("select.def"), label = "Select Default", width = "98%")), 
          fluidRow(actionButton(inputId = ns("unselect.all"), label = "Unselect All", width = "98%"))
        )
      })
    }
  }

  
  observeEvent(input$select.all, {
    updateCheckboxGroupInput(session = session, inputId = "checkbox", label = NULL, choices = choices, selected = choices)
  })
  
  observeEvent(input$select.def, {
    updateCheckboxGroupInput(session = session, inputId = "checkbox", label = NULL, choices = choices, selected = selected)
  })
  
  observeEvent(input$unselect.all, {
    updateCheckboxGroupInput(session = session, inputId = "checkbox", label = NULL, choices = choices)
  })
  
}

