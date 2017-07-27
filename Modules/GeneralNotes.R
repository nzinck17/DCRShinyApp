##############################################################################################################################
#     Title: GeneralNotes.R
#     Description: General Notes for DCR Shiny Apps. Notes in the Shiny App Files will refer to these General notes
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################
#     General Notes: 
#       1. Leaflet and RenderUI do not work well together. If this is needed, an action 
#          button is a solution but from my experience still causes some problems
#       2. Conditional panels and shiny modules don't work well togethr due to the ns() wrapper 
#          requrement. Javascript must be used in the conditions of the conditional panel.
