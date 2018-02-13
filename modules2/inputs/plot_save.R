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

PLOT_TITLE_AND_LABELS_UI <- function(id, sec_y_axis = FALSE) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    
    fluidRow(
  )
}


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

PLOT_TITLE_AND_LABELS <- function(input, output, session, P, Title_Auto, X_Lab_Auto, Y_Lab_Auto, Y2_Lab_Auto = NULL) {
  
  ns <- session$ns # see General Note 1
  

  
} # end Server Function


