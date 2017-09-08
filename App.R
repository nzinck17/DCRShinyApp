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

#### NOTE - Libraries will now be installed and loaded in the LaunchAppGitHub.R script - this can be deleted from app.r

# load libraries
# library(shiny)
# library(tidyr)
# library(ggplot2)
# library(plotly)
# library(dplyr)
# library(lubridate)
# library(leaflet)
# library(RColorBrewer)
# library(DT)
# library(akima)
# #library(RODBC)
# library(DBI)
# library(odbc)


### Run/Source Scripts that load data

source("Sources/LoadMSAccessData.R")
source("Sources/Settings.R")

### Load Primary Modules

source("Modules/Home.R")
source("Modules/Time.R")
source("Modules/Regress.R")
source("Modules/Time-Depth.R")
source("Modules/Regress-Depth.R")
source("Modules/Profile-Heatmap.R")
source("Modules/Profile-Line.R")
source("Modules/Profile-Summary.R")
source("Modules/MapPlot.R")
source("Modules/Report-AWQ.R")
source("Modules/Report-MWQ.R")
source("Modules/Report-Custom.R")

### Load Secondary Modules (Module that goes inside a module)

source("Modules2/Plot-Time.R")
source("Modules2/Plot-Time-Depth.R")
source("Modules2/Plot-Regress.R")
source("Modules2/Plot-Regress-Depth.R")
source("Modules2/Plot-Profline-Custom.R")
#source("Modules2/Plot-Profline-Standard.R")
#source("Modules2/Plot-Profline-Custom.R")
#source("Modules2/Plot-Profline-Standard.R")
source("Modules2/Summary.R")
source("Modules2/Summary-Depth.R")
#source("Modules2/Summary-Profile.R")
source("Modules2/SiteMap.R")

 ### Load Functions

source("Functions/GetSeasons.R")
source("Functions/circleSizeLegend.R")

###################################################################################
##################################  User Interface  ###############################
###################################################################################

ui <- navbarPage("DCR", position = "fixed-top", inverse = TRUE, collapsible = TRUE,

######################################################
# PG 1 - Home Page

tabPanel("Home",

  # Title
  fluidRow(br(), br(), br(), br(), h2("Water Quality Data Management System", align = "center")),
  fluidRow(h3("Department of Conservation and Recreation", align = "center"), br()),
  Home.UI("Home")

),


######################################################
# PG 2 - Tributary Water Quality Data

tabPanel("Tributary",

  # Title
  fluidRow(br(), br(), br(), br(), h2("Tributary Water Quality Data", align = "center"), br()),

  navlistPanel(widths = c(2, 10),
               tabPanel("Time-Series",
                        fluidRow(column(10, h4("Tributary Time-Series Analysis", align = "center")), column(2)),
                        tabsetPanel(
                          tabPanel("Quabbin", time.UI("Quabbin Trib Time", df.trib.quab)),
                          tabPanel("Ware River", time.UI("Ware River Trib Time", df.trib.ware)),
                          tabPanel("Wachusett", time.UI("Wachusett Trib Time", df.trib.wach)),
                          tabPanel("All Tribs", time.UI("All Tribs Time", df.trib.all))
                        ) # end tabset Panel
               ), # end tabpanel
               tabPanel("Regression",
                        fluidRow(column(10, h4("Tributary Regression Analysis", align = "center")), column(2)),
                        tabsetPanel(
                          tabPanel("Quabbin", regress.UI("Quabbin Trib Regress", df.trib.quab)),
                          tabPanel("Ware River", regress.UI("Ware River Trib Regress", df.trib.ware)),
                          tabPanel("Wachusett", regress.UI("Wachusett Trib Regress", df.trib.wach)),
                          tabPanel("All Tribs", regress.UI("All Tribs Regress", df.trib.all))
                        ) # end tabset Panel
               ) # end tabpanel
  ) # end navlist panel

), # end Tributary tabpanel (page)


#############################################################
# PG 3 - Reservoir

tabPanel("Reservoir",

   # Title
   fluidRow(br(), br(), br(), br(), h2("Reservoir Water Quality Data", align = "center"), br()),

   navlistPanel(widths = c(2, 10),

                "Transect",
                tabPanel("Time-Series",
                         fluidRow(column(10, h4("Transect Time-Series Analysis", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", time.UI("Quabbin Transect Time", df.tran.quab)),
                           tabPanel("Wachusett", time.UI("Wachusett Transect Time", df.tran.wach))
                         ) # end tabset Panel
                ), # end tabpanel
                tabPanel("Regression",
                         fluidRow(column(10, h4("Transect Regression Analysis", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", regress.UI("Quabbin Transect Regress", df.tran.quab)),
                           tabPanel("Wachusett", regress.UI("Wachusett Transect Regress", df.tran.wach))
                         ) # end tabset panel
                ), # end tabpanel

                "Nutrient",
                tabPanel("Time-Series",
                         fluidRow(column(10, h4("Nutrient Time-Series Analysis", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", time.depth.UI("Quabbin Nutrient Time", df.nut.quab)),
                           tabPanel("Wachusett", time.depth.UI("Wachusett Nutrient Time", df.nut.wach))
                         )
                ),
                tabPanel("Regression",
                         fluidRow(column(10, h4("Nutrient Regression Analysis", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", regress.depth.UI("Quabbin Nutrient Regress", df.nut.quab)),
                           tabPanel("Wachusett", regress.depth.UI("Wachusett Nutrient Regress", df.nut.wach))
                         )
                ),

                "Profile",
                tabPanel("Heat Map",
                         fluidRow(column(10, h4("Profile Heatmap", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", prof.heatmap.UI("Quabbin Profile Heatmap", df.prof.quab)),
                           tabPanel("Wachusett", prof.heatmap.UI("Wachusett Profile Heatmap", df.prof.wach))
                         )
                ),
                tabPanel("Line Plot",
                         fluidRow(column(10, h4("Profile Line Plot", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", prof.line.UI("Quabbin Profile Line", df.prof.quab)),
                           tabPanel("Wachusett", prof.line.UI("Wachusett Profile Line", df.prof.wach))
                         )
                ),
                tabPanel("Table and Summary",
                         fluidRow(column(10, h4("Profile Summary", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", prof.summary.UI("Quabbin Profile Summary", df.prof.quab)),
                           tabPanel("Wachusett", prof.summary.UI("Wachusett Profile Summary", df.prof.wach))
                         )
                ),

                "AquaBio",
                tabPanel("Heatmap"),
                tabPanel("Line Plot")
   ) # end navlist

 ),  # end Tabpanel (page)


#######################################################
#4 -  Map

tabPanel("Map Plot",

         # Title
         fluidRow(br(), br(), br(), br(), h2("Map Plot", align = "center"), br()),

         navlistPanel(widths = c(2, 10),
                      "Tributaries",
                      tabPanel("Quabbin", map.plot.UI("Quabbin Trib MapPlot", df = df.trib.quab)),
                      tabPanel("Ware River", map.plot.UI("Ware River Trib MapPlot", df = df.trib.quab)),
                      tabPanel("Wachusett", map.plot.UI("Wachusett Trib MapPlot", df = df.trib.quab)),
                      "Reservoir Transect",
                      tabPanel("Quabbin", map.plot.UI("Quabbin Transect MapPlot", df = df.tran.quab)),
                      tabPanel("Wachusett", map.plot.UI("Wachusett Transect MapPlot", df = df.tran.quab))
         ) # end navlist

), # end tabpanel (page)


####################################################################
# PG 5 - Hydrology/Meteorology

tabPanel("Met/Hydro",

         # Title
         fluidRow(br(), br(), br(), br(), h2("Hydrology and Meteorology Data", align = "center"), br())
),

####################################################################
# PG 6 - Forestry

tabPanel("Forestry",

         # Title
         fluidRow(br(), br(), br(), br(), h2("Forestry Data", align = "center"), br())
),

#########################################################
# PG 6 - Reports

tabPanel("Report",
         # Title
         fluidRow(br(), br(), br(), br(), h2("Report Generation Tool", align = "center"), br()),

         navlistPanel(widths = c(2, 10),
                      "Preset Reports",
                      tabPanel("Annual WQ",
                               fluidRow(column(10, h4("Annual Water Quality Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", report.awq.UI("Quabbin AWQ Report", df.trib.quab)),
                                 tabPanel("Wachusett", report.awq.UI("Wachusett AWQ Report", df.trib.wach))
                               ) # end tabset Panel
                      ), # end tabpanel
                      tabPanel("Monthly WQ",
                               fluidRow(column(10, h4("Monthly Water Quality Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", report.mwq.UI("Quabbin MWQ Report", df.trib.quab)),
                                 tabPanel("Wachusett", report.mwq.UI("Wachusett MWQ Report", df.trib.wach))
                               ) # end tabset panel
                      ), # end tabpanel
                      "Custom Reports",
                      tabPanel("Tributary",
                               fluidRow(column(10, h4("Tributary Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", report.custom.UI("Quabbin Trib Custom Report", df.trib.quab)),
                                 tabPanel("Ware River", report.custom.UI("Ware River Trib Custom Report", df.trib.ware)),
                                 tabPanel("Wachusett", report.custom.UI("Wachusett Trib Custom Report", df.trib.wach))
                               )
                      ),
                      tabPanel("Reservoir",
                               fluidRow(column(10, h4("Reservoir Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin",report.custom.UI("Quabbin Res Custom Report", df.res.quab)),
                                 tabPanel("Wachusett",report.custom.UI("Wachusett Res Custom Report", df.res.wach))
                               )
                      ),
                      tabPanel("Profile",
                               fluidRow(column(10, h4("Profile Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin",report.custom.UI("Quabbin Profile Custom Report", df.prof.quab)),
                                 tabPanel("Wachusett",report.custom.UI("Wachusett Profile Custom Report", df.prof.wach))
                               )
                      ),
                      tabPanel("Phytoplankton",
                               fluidRow(column(10, h4("Phytoplankton Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", report.custom.UI("Quabbin Phyto Custom Report", df.prof.quab)),
                                 tabPanel("Wachusett", report.custom.UI("Wachusett Phyto Custom Report", df.prof.wach))
                               )
                      )
         ) # end navlist

), # end tabpanel (page)

#######################################################
# PG 8 - Input Data

# Give the page a title
tabPanel("Input Data",

         fluidRow(br(), br(), br(), br(), h2("Input Data", align = "center"), br(),
                  br(), h4("button to make sure all sites in list are accounted for in Site Table"),
                  br(), h4("button to make sure all Parameters in list are accounted for in Parameter Table")
                  )


)

#######################################################

) # end UI

########################################################################################
################################    Server   ###########################################
########################################################################################

server <- function(input, output, session) {

######################################################
# PG 1 - Tributary Water Quality Data

  callModule(Home, "Home", df.site = df.all.site)

######################################################
# PG 2 - Tributary

  # Time Series
  callModule(time, "Quabbin Trib Time", df = df.trib.quab, df.site = df.trib.quab.site)
  callModule(time, "Ware River Trib Time", df = df.trib.ware, df.site = df.trib.ware.site)
  callModule(time, "Wachusett Trib Time", df = df.trib.wach, df.site = df.trib.wach.site)
  callModule(time, "All Trib Time", df = df.trib.all, df.site = df.trib.all.site)

  # Regression
  callModule(regress, "Quabbin Trib Regress", df = df.trib.quab, df.site = df.trib.quab.site)
  callModule(regress, "Ware River Trib Regress", df = df.trib.ware, df.site = df.trib.ware.site)
  callModule(regress, "Wachusett Trib Regress", df = df.trib.wach, df.site = df.trib.wach.site)
  callModule(regress, "All Trib Regress", df = df.trib.all, df.site = df.trib.all.site)

#############################################################
# PG 3 - Reservoir

  # Transect
  callModule(time, "Quabbin Transect Time", df = df.tran.quab, df.site = df.tran.quab.site)
  callModule(time, "Wachusett Transect Time", df = df.tran.wach, df.site = df.tran.wach.site)

  callModule(regress, "Quabbin Transect Regress", df = df.tran.quab, df.site = df.tran.quab.site)
  callModule(regress, "Wachusett Transect Regress", df = df.tran.wach, df.site = df.tran.wach.site)

  # Nutrient
  callModule(time.depth, "Quabbin Nutrient Time", df = df.nut.quab, df.site = df.nut.quab.site)
  callModule(time.depth, "Wachusett Nutrient Time", df = df.nut.wach, df.site = df.nut.wach.site)

  callModule(regress.depth, "Quabbin Nutrient Regress", df = df.nut.quab, df.site = df.nut.quab.site)
  callModule(regress.depth, "Wachusett Nutrient Regress", df = df.nut.wach, df.site = df.nut.wach.site)

  # Profile
  callModule(prof.heatmap, "Quabbin Profile Heatmap", df = df.prof.quab)
  callModule(prof.heatmap, "Wachusett Profile Heatmap", df = df.prof.wach)

  callModule(prof.line, "Quabbin Profile Line", df = df.prof.quab)
  callModule(prof.line, "Wachusett Profile Line", df = df.prof.wach)

  callModule(prof.summary, "Quabbin Profile Summary", df = df.prof.quab)
  callModule(prof.summary, "Wachusett Profile Summary", df = df.prof.wach)

  # AquaBio


####################################################################
# PG 8 - Map Plot

  callModule(map.plot, "Quabbin Trib MapPlot", df = df.trib.quab, df.site = df.trib.quab.site)
  callModule(map.plot, "Ware River Trib MapPlot", df = df.trib.ware, df.site = df.trib.ware.site)
  callModule(map.plot, "Wachusett Trib MapPlot", df = df.trib.wach, df.site = df.trib.wach.site)
  callModule(map.plot, "Quabbin Transect MapPlot", df = df.tran.quab, df.site = df.tran.quab.site)
  callModule(map.plot, "Wachusett Transect MapPlot", df = df.tran.wach, df.site = df.tran.wach.site)


######################################################

# PG 5 - Hydrology/Meteorology/Statistics

####################################################################
# PG 8 - Reports

  callModule(report.awq, "Quabbin AWQ Report", df.trib = df.trib.quab, df.res = df.res.quab, df.prof = df.prof.quab, df.site = df.quab.site)
  callModule(report.awq, "Wachusett AWQ Report", df.trib = df.trib.wach, df.res = df.res.wach, df.prof = df.prof.wach, df.site = df.wach.site)
  callModule(report.mwq, "Quabbin MWQ Report", df.trib = df.trib.quab, df.res = df.res.quab, df.prof = df.prof.quab, df.site = df.quab.site)
  callModule(report.mwq, "Wachusett MWQ Report", df.trib = df.trib.wach, df.res = df.res.wach, df.prof = df.prof.wach, df.site = df.wach.site)
  callModule(report.custom, "Quabbin Trib Custom Report", df = df.trib.quab, df.site = df.trib.quab.site)
  callModule(report.custom, "Ware River Trib Custom Report", df = df.trib.ware, df.site = df.trib.ware.site)
  callModule(report.custom, "Wachusett Trib Custom Report", df = df.trib.quab, df.site = df.trib.quab.site)
  callModule(report.custom, "Quabbin Res Custom Report", df = df.res.quab, df.site = df.res.quab.site)
  callModule(report.custom, "Wachusett Res Custom Report", df = df.res.quab, df.site = df.res.quab.site)
  callModule(report.custom, "Quabbin Profile Custom Report", df = df.prof.quab, df.site = df.res.quab.site)
  callModule(report.custom, "Wachusett Profile Custom Report", df = df.prof.wach, df.site = df.res.wach.site)
  callModule(report.custom, "Quabbin Phyto Custom Report", df = df.prof.quab, df.site = df.res.quab.site)
  callModule(report.custom, "Wachusett Phyto Custom Report", df = df.prof.wach, df.site = df.res.wach.site)
# Code to stop app when browser session window closes
session$onSessionEnded(function() {
      stopApp()
    })

} # end server function

#combines the user interface and server (it's a must)
shinyApp(ui = ui, server = server)






