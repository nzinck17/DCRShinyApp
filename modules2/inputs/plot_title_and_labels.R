##############################################################################################################################
#     Title: plot_title_and_labels.R
#     Type: Module2
#     Description: Time Series plot (for non-depth dependent data)
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#   1. 

# To-Do List:
#   1. 

##############################################################################################################################
# User Interface
##############################################################################################################################

PLOT_TITLE_AND_LABELS_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    wellPanel(
      fluidRow(
        column(3,
               radioButtons(ns("title"), "Title Options:",
                            choices= c("None", "Auto", "Custom"),
                            selected = "Auto"),
               textInput(ns("title_text"), "")
        ), # end column
        column(3,
               radioButtons(ns("x_lab"), "X Label Options:",
                            choices= c("None", "Auto", "Custom"),
                            selected = "Auto"),
               textInput(ns("x_lab_text"), "")
        ), # end column
        column(3,
               radioButtons(ns("y_lab"), "Y Label Options:",
                            choices= c("None", "Auto", "Custom"),
                            selected = "Auto"),
               textInput(ns("y_lab_text"), "")
        ), # end column
        column(3,
               conditionalPanel(
                 condition = "sec_y_axis == TRUE", 
                 radioButtons(ns("y2_lab"), "Y Label Options:",
                              choices= c("None", "Auto", "Custom"),
                              selected = "Auto"),
                 textInput(ns("y2_lab_text"), "")
               )
        ) # end column
      )
    )
  )
}


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

PLOT_TITLE_AND_LABELS <- function(input, output, session, P, Title_Auto, X_Lab_Auto, Y_Lab_Auto, sec_y_axis = FALSE, Y2_Lab_Auto = NULL) {
  
  ns <- session$ns # see General Note 1
  
  P1 <- reactive({
    
    p <- P()
    
    # Title and Axis Lables
    
    # Title
    if(input$title == "None"){
      p <- p + ggtitle("")
    }
    if(input$title == "Auto"){
      p <- p + ggtitle(Title_Auto())
    }
    if(input$title == "Custom"){
      p <- p + ggtitle(input$title_text)
    }
    
    p <- p + theme(plot.title = element_text(hjust = 0.5))
    
    # X Axis Label
    if(input$x_lab == "None"){
      p <- p + xlab("")
    }
    if(input$x_lab == "Auto"){
      p <- p + xlab(X_Lab_Auto())
    }
    if(input$x_lab == "Custom"){
      p <- p + xlab(input$x_lab_text)
    }
    
    # Y Axis Label
    if(input$y_lab == "None"){
      p <- p + ylab("")
    }
    if(input$y_lab == "Auto"){
      p <- p + ylab(Y_Lab_Auto())
    }
    if(input$y_lab == "Custom"){
      p <- p + ylab(input$y_lab_text)
    }
    
    p
    
  })
  
  output$sec_y_axis_ui <- renderUI({
    if(sec_y_axis == TRUE){
      tagList(
        radioButtons(ns("y2_lab"), "Y Label Options:",
                     choices= c("None", "Auto", "Custom"),
                     selected = "Auto"),
        textInput(ns("y2_lab_text"), "")
      )
    }
  })
  
  # Do not suspend rendering when Main tab is not selected
  outputOptions(output, "sec_y_axis_ui", suspendWhenHidden = FALSE)
  
  # Secondary Y-axis
  Y2_Lab <- reactive({
    req(input$y2_lab)
    if(sec_y_axis == TRUE){
      if(input$y2_lab == "None"){
        ""
      }
      if(input$y2_lab == "Auto"){
        Y2_Lab_Auto()
      }
      if(input$y2_lab == "Custom"){
        input$y2_lab_text
      }else{
        "the renderUI is delayed!"
      }
    }
  })

  
  return(list(Plot = P1, Y2_Lab = Y2_Lab))
  
} # end Server Function



