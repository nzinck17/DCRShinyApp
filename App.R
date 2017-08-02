##############################################################################################################################
#     Title: App.R
#     Type: Master file for DCR Shiny App
#     Description: This Shiny App contains the "master" script for the app. The app contains a ui and server component
#           and sources R scripts from the App folder
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. 
#
# To-Do List:
#   1. 

####################################################################################################
# Load Libraries and Script (Sources, Modules, and Functions)
#####################################################################################################

# load libraries
library(shiny)
library(tidyr)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(DT)
library(RODBC)
#library(DBI)
#library(odbc)

### Run/Source Scripts that load data

source("Sources/LoadMSAccessData.R")
#source("Sources/LoadRDS.R")

### Load Modules
#https://raw.githubusercontent.com/nzinck17/DCRShinyApp/master/

source("Modules/Home.R")
source("Modules/Tributary-Time.R")
source("Modules/Tributary-Regress.R")
source("Modules/Reservoir-Time.R")
source("Modules/Reservoir-Regress.R")
source("Modules/Profile-Heatmap.R")
source("Modules/Profile-Line.R")
source("Modules/Profile-Summary.R")
source("Modules/MapPlot.R")

# Load Functions

#Relative Paths
source("Functions/GetSeasons.R")
source("Functions/circleSizeLegend.R")

###################################################################################
##################################  User Interface  ###############################
###################################################################################

ui <- navbarPage("DCR", position = "fixed-top", inverse = TRUE, collapsible = TRUE,

##########################################
tags$head(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}",
             ".leaflet .legend i{
             border-radius:50%;
             width: 10px;
             height: 10px;
             margin-top: 4px;
             }
             ")
  ),

######################################################                   
# PG 1 - Home Page            

tabPanel("Home",
  
  # Title       
  fluidRow(br(), br(), br(), br(), h2("Water Quality Data Management System", align = "center")),
  fluidRow(br(), h3("Department of Conservation and Recreation", align = "center"), br()),
  
    Home.UI("Home")
  
),
######################################################                   
# PG 2 - Tributary Water Quality Data             
                 
tabPanel("Trib - Time",

  # Title
  fluidRow(br(), br(), br(), br(), h2("Tributary Time-Series Analysis", align = "center"), br()),
  
  # Tabset Panels
  tabsetPanel("tab.trib.time",
      tabPanel("Quabbin Tribs",
               Trib.time.UI("Quabbin Trib Time", df.trib.quab)
      ),
      tabPanel("Ware River Tribs",
               Trib.time.UI("Ware River Trib Time", df.trib.ware)
      ),
      tabPanel("Wachusett Tribs",
               Trib.time.UI("Wachusett Trib Time", df.trib.wach)
      ),
      tabPanel("All Tribs",
               Trib.time.UI("All Tribs Time", df.trib.all)
      )
  )# end Tributary tabset
  
),# end Tributary tabpanel (page)
   

######################################################                   
# PG 3 - Tributary Regression Alalysis           

tabPanel("Trib - Regress",
         
         # Title
         fluidRow(br(), br(), br(), br(), h2("Tributary Regression Analysis", align = "center"), br()),
         
         # Tabset Panels
         tabsetPanel("tab.trib.regress",
                     tabPanel("Quabbin Tribs",
                              Trib.regress.UI("Quabbin Trib Regress", df.trib.quab)
                     ),
                     tabPanel("Ware River Tribs",
                              Trib.regress.UI("Ware River Trib Regress", df.trib.ware)
                     ),
                     tabPanel("Wachusett Tribs",
                              Trib.regress.UI("Wachusett Trib Regress", df.trib.wach)
                     ),
                     tabPanel("All Tribs",
                              Trib.regress.UI("All Tribs Regress", df.trib.all)
                     )
         )# end Tributary tabset
         
),# end Tributary tabpanel (page)

#############################################################
# PG 4 - Reservoir Time Series
  
tabPanel("Reservoir - Time",
   
   # Title
   fluidRow(br(), br(), br(), br(), h3("Reservoir Water Quality Data Viewer"), br()),
   
   # Tabset Panels
   tabsetPanel("tab.res",
     tabPanel("Quabbin Res",
              Res.time.UI("Quabbin Res Time", df.res.quab)
     ),
     tabPanel("Wachusett Res",
              Res.time.UI("Wachusett Res Time", df.res.wach)
     )
   ) # end Tabset Panels
   
 ),  # end Tabpanel (page)

#############################################################
# PG 5 - Reservoir Water Quality

tabPanel("Reservoir Regress",
         
   # Title
   fluidRow(br(), br(), br(), br(), h3("Reservoir Water Quality Data Viewer"), br()),
   
   # Tabset Panels
   tabsetPanel("tab.res",
               
       tabPanel("Quabbin Res",
                Res.regress.UI("Quabbin Res Regress", df.res.quab)
       ),
       tabPanel("Wachusett Res Regress",
                Res.regress.UI("Wachusett Res Regress", df.res.wach)
       )      
   ) # end Tabset Panels
   
),  # end Tabpanel (page)

#######################################################################
# PG 6 - Profile

tabPanel("Profile Data",
        
  # Title
  fluidRow(column(1),column(8, br(), br(), br(), br(), h3("Reservoir Water Quality Data Viewer"), br())),
  
  navlistPanel(widths = c(2, 10),
    "Quabbin Reservoir",
    tabPanel("Heatmap Plot custom",
             fluidRow(column(1),column(8, h4("Quabbin Reservoir Yearly Profile Data Plotted as 'heatmap' style"))),
             prof.heatmap.UI("Quabbin Profile Heatmap", df.profile.quab)
    ),
    tabPanel("Heatmap Plot standard"),
    tabPanel("Line Plot custom",
            fluidRow(column(1),column(8, h4("Quabbin Reservoir Profile Data Plotted as line plots"))),
            prof.line.UI("Quabbin Profile Line", df.profile.quab)
    ),
    tabPanel("Line Plot standard"),
    tabPanel("Summary and Table", br(),
             fluidRow(column(1),column(8, h4("Quabbin Reservoir Profile Data Summary and Table"))),
             prof.summary.UI("Quabbin Profile Summary", df.profile.quab)
             ),
    "Wachusett Reservoir",
    tabPanel("Heatmap Plot custom",
             fluidRow(column(1),column(8, h4("Wachusett Reservoir Yearly Profile Data Plotted as 'heatmap' style"))),
             prof.heatmap.UI("Wachusett Profile Heatmap", df.profile.wach)
    ),
    tabPanel("Heatmap Plot standard"),
    tabPanel("Line Plot custom",
            fluidRow(column(1),column(8, h4("Wachusett Reservoir Profile Data Plotted as line plots"))),
            prof.line.UI("Wachusett Profile Line", df.profile.wach)
    ),
    tabPanel("Line Plot standard"),
    tabPanel("Summary and Table", br(),
             fluidRow(column(1),column(8, h4("Wachusett Reservoir Profile Data Summary and Table"))),
             prof.summary.UI("Wachusett Profile Summary", df.profile.wach)
    )
  )# end navlist
  
),# end tabpanel (page)


####################################################################
# PG 7 - Aquatic Bio

tabPanel("Aquatic Bio",
         
         # Title
         fluidRow(br(), br(), br(), br(), h3("Hydrology and Meteorology Data Viewer")
                  
                  
         )
),

#######################################################
# PG 8a - Map Plots

tabPanel("Map Quab",
         
         map.plot.UI("Quabbin MapPlot", df = df.trib.quab)

),

#######################################################
# PG 8b - Map Plots

tabPanel("Map Ware",
         
         map.plot.UI("Ware River MapPlot", df = df.trib.ware)
       
),

#######################################################
# PG 8c - Map Plots

tabPanel("Map Wach", 
         
         map.plot.UI("Wachusett MapPlot", df = df.trib.wach)
       
),

####################################################################
# PG 9 - Hydrology/Meteorology

tabPanel("Met/Hydro",
         
         # Title
         fluidRow(br(), br(), br(), br(), h3("Hydrology and Meteorology Data Viewer")
                  
        )
),  

#########################################################
# PG 10 - Reports

tabPanel("Reports",
         # Title
         fluidRow(br(), br(), br(), br(), h3("Annual Water Quality Report")
                  
         )
         
),

#######################################################
# PG 8 - Input Data

# Give the page a title
tabPanel("Input Data",
         
         fluidRow(br(), br(), br(), br(), h3("Input Data (Maybe make seperate)"),
                  br(), h4("button to make sure all sites in list are accounted for in Site Table"),
                  br(), h4("button to make sure all Parameters in list are accounted for in Parameter Table")
                  )
         

)

#######################################################

) # end UI
           
########################################################################################
################################    Server   ###########################################
########################################################################################

server <- function(input, output) {
  
######################################################                   
# PG 1 - Tributary Water Quality Data   
  
  callModule(Home, "Home", df.site = df.wach.site)
 
######################################################                   
# PG 2 - Tributary Time  

  callModule(Trib.time, "Quabbin Trib Time", df = df.trib.quab, df.site = df.quab.site)
  callModule(Trib.time, "Ware River Trib Time", df = df.trib.ware, df.site = df.ware.site)
  callModule(Trib.time, "Wachusett Trib Time", df = df.trib.wach, df.site = df.wach.site)
  callModule(Trib.time, "All Trib Time", df = df.trib.all, df.site = df.wach.site)
  
######################################################                   
# PG 3 - Tributary Regression  
  
  callModule(Trib.regress, "Quabbin Trib Regress", df = df.trib.quab, df.site = df.quab.site)
  callModule(Trib.regress, "Ware River Trib Regress", df = df.trib.ware, df.site = df.ware.site)
  callModule(Trib.regress, "Wachusett Trib Regress", df = df.trib.wach, df.site = df.wach.site)
  callModule(Trib.regress, "All Trib Regress", df = df.trib.all, df.site = df.wach.site)
  
#############################################################
# PG 4 - Reservoir Time
  
  callModule(Res.time, "Quabbin Res Time", df = df.res.quab, df.site = df.quab.site)
  callModule(Res.time, "Wachusett Res Time", df = df.res.wach, df.site = df.wach.site)
  
#############################################################
# PG 5 - Reservoir Regression
  
  callModule(Res.regress, "Quabbin Res Regress", df = df.res.quab, df.site = df.quab.site)
  callModule(Res.regress, "Wachusett Res Regress", df = df.res.wach, df.site = df.wach.site)
  
#######################################################################
# PG 6 - Profile/Bouy Data for Reservoirs

  callModule(prof.heatmap, "Quabbin Profile Heatmap", df = df.profile.quab)
  callModule(prof.heatmap, "Wachusett Profile Heatmap", df = df.profile.wach)
  callModule(prof.line, "Quabbin Profile Line", df = df.profile.quab)
  callModule(prof.line, "Wachusett Profile Line", df = df.profile.wach)
  callModule(prof.summary, "Quabbin Profile Summary", df = df.profile.quab)
  callModule(prof.summary, "Wachusett Profile Summary", df = df.profile.wach)

####################################################################
# PG 8 - Tributary Water Quality Data    

  callModule(map.plot, "Quabbin MapPlot", df = df.trib.quab, df.site = df.quab.site)
  callModule(map.plot, "Ware River MapPlot", df = df.trib.ware, df.site = df.ware.site)
  callModule(map.plot, "Wachusett MapPlot", df = df.trib.wach, df.site = df.wach.site)

  
######################################################                   

# PG 5 - Hydrology/Meteorology/Statistics
  
  
} # end server function

#combines the user interface and server (it's a must)
shinyApp(ui = ui, server = server)






