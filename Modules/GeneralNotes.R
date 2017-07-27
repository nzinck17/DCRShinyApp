##############################################################################################################################
#     Title: GeneralNotes.R
#     Description: General Notes for DCR Shiny Apps. Notes in the Shiny App Files will refer to these General notes
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################
#     General Notes:
#       1. Shiny Modules have a special required format. Everything in the UI must be surrounded by taglist. Every Input and
#          Output object must be wrapped in ns(__), this gives each object a unique name by locating that name within a
#          namespace identified by the inputted "id". This allows the use of identical locally named Input/output objects,
#          becuse globally they are different. When using RenderUI, one must also use the ns() wrap
#       2. Leaflet and RenderUI do not work well together. If this is needed, an action 
#          button is a solution but from my experience still causes some problems
#       3. Conditional panels and shiny modules don't work well togethr due to the ns() wrapper 
#          requrement. Javascript must be used in the conditions of the conditional panel.
#       4. When the Base Leaflet Map contains the Map Tiles and the Circle Markers for the Site Locations. It is not neccesary to 
#          to specify the starting min/max Lat/Long coordinates becuase the location of the markers tell it where to start.