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
source("Modules/Export-WQ.R")
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
source("Modules2/CheckboxSelectAll.R")

 ### Load Functions

source("Functions/GetSeasons.R")
source("Functions/circleSizeLegend.R")

###################################################################################
##################################  User Interface  ###############################
###################################################################################

ui <- navbarPage("DCR", position = "fixed-top", inverse = TRUE, collapsible = TRUE,

######################################################
# Home Page

tabPanel("Home",

  # Title
  fluidRow(br(), br(), br(), br(), h2("Water Quality Data Management System", align = "center")),
  fluidRow(h3("Department of Conservation and Recreation", align = "center"), br()),
  Home.UI("Home")

),


######################################################
# Tributary Water Quality Data

tabPanel("Tributary",

  # Title
  fluidRow(br(), br(), br(), br(), h2("Tributary Water Quality Data", align = "center"), br()),

  navlistPanel(widths = c(2, 10),
               tabPanel("Time-Series",
                        fluidRow(column(10, h4("Tributary Time-Series Analysis", align = "center")), column(2)),
                        tabsetPanel(
                          tabPanel("Quabbin", time.UI("mod.trib.quab.time", df.trib.quab)),
                          tabPanel("Ware River", time.UI("mod.trib.ware.time", df.trib.ware)),
                          tabPanel("Wachusett", time.UI("mod.trib.wach.time", df.trib.wach)),
                          tabPanel("All Tribs", time.UI("mod.trib.all.time", df.trib.all))
                        ) # end tabset Panel
               ), # end tabpanel
               tabPanel("Regression",
                        fluidRow(column(10, h4("Tributary Regression Analysis", align = "center")), column(2)),
                        tabsetPanel(
                          tabPanel("Quabbin", regress.UI("mod.trib.quab.regr", df.trib.quab)),
                          tabPanel("Ware River", regress.UI("mod.trib.ware.regr", df.trib.ware)),
                          tabPanel("Wachusett", regress.UI("mod.trib.wach.regr", df.trib.wach)),
                          tabPanel("All Tribs", regress.UI("mod.trib.all.regr", df.trib.all))
                        ) # end tabset Panel
               ) # end tabpanel
  ) # end navlist panel

), # end Tributary tabpanel (page)


#############################################################
# Reservoir

tabPanel("Reservoir",

   # Title
   fluidRow(br(), br(), br(), br(), h2("Reservoir Water Quality Data", align = "center"), br()),

   navlistPanel(widths = c(2, 10),
                "Bacteria",
                tabPanel("Time-Series",
                         fluidRow(column(10, h4("Bacteria Time-Series Analysis", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", time.UI("mod.bact.quab.time", df.bact.quab)),
                           tabPanel("Wachusett", time.UI("mod.bact.wach.time", df.bact.wach))
                         ) # end tabset Panel
                ), # end tabpanel
                tabPanel("Regression",
                         fluidRow(column(10, h4("Bacteria Regression Analysis", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", regress.UI("mod.bact.quab.regr", df.bact.quab)),
                           tabPanel("Wachusett", regress.UI("mod.bact.wach.regr", df.bact.wach))
                         ) # end tabset panel
                ), # end tabpanel

                "Chemistry",
                tabPanel("Time-Series",
                         fluidRow(column(10, h4("Chemical Time-Series Analysis", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", time.depth.UI("mod.chem.quab.time", df.chem.quab)),
                           tabPanel("Wachusett", time.depth.UI("mod.chem.wach.time", df.chem.wach))
                         )
                ),
                tabPanel("Regression",
                         fluidRow(column(10, h4("Chemical Regression Analysis", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", regress.depth.UI("mod.chem.quab.regr", df.chem.quab)),
                           tabPanel("Wachusett", regress.depth.UI("mod.chem.wach.regr", df.chem.wach))
                         )
                ),

                "Profile (physicochemical)",
                tabPanel("Heat Map",
                         fluidRow(column(10, h4("Profile Heatmap", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", prof.heatmap.UI("mod.prof.quab.heat", df.prof.quab)),
                           tabPanel("Wachusett", prof.heatmap.UI("mod.prof.wach.heat", df.prof.wach))
                         )
                ),
                tabPanel("Line Plot",
                         fluidRow(column(10, h4("Profile Line Plot", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", prof.line.UI("mod.prof.quab.line", df.prof.quab)),
                           tabPanel("Wachusett", prof.line.UI("mod.prof.wach.line", df.prof.wach))
                         )
                ),
                tabPanel("Table and Summary",
                         fluidRow(column(10, h4("Profile Summary", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", prof.summary.UI("mod.prof.quab.sum", df.prof.quab)),
                           tabPanel("Wachusett", prof.summary.UI("mod.prof.wach.sum", df.prof.wach))
                         )
                # ),
                # tabPanel("Biological",
                #          fluidRow(column(10, h4("Phytoplankton Plots", align = "center")), column(2)),
                #          tabsetPanel(
                #            tabPanel("Wachusett", phyto.plots.UI("mod.phyto.wach.line", df.phyto.wach))
                #          )
                )

   ) # end navlist

 ),  # end Tabpanel (page)


#######################################################
# Map

tabPanel("Map Plot",

         # Title
         fluidRow(br(), br(), br(), br(), h2("Map Plot", align = "center"), br()),

         navlistPanel(widths = c(2, 10),
                      "Tributaries",
                      tabPanel("Quabbin", map.plot.UI("mod.trib.quab.map", df = df.trib.quab)),
                      tabPanel("Ware River", map.plot.UI("mod.trib.ware.map", df = df.trib.ware)),
                      tabPanel("Wachusett", map.plot.UI("mod.trib.wach.map", df = df.trib.wach)),
                      tabPanel("All Tribs", map.plot.UI("mod.trib.all.map", df = df.trib.all)),
                      "Reservoir Bacteria",
                      tabPanel("Quabbin", map.plot.UI("mod.bact.quab.map", df = df.bact.quab)),
                      tabPanel("Wachusett", map.plot.UI("mod.bact.wach.map", df = df.bact.wach)),
                      "Reservoir Chemical",
                      tabPanel("Quabbin", map.plot.UI("mod.chem.quab.map", df = df.chem.quab)),
                      tabPanel("Wachusett", map.plot.UI("mod.chem.wach.map", df = df.chem.wach))
         ) # end navlist

), # end tabpanel (page)


####################################################################
# Hydrology/Meteorology

tabPanel("Met/Hydro",

         # Title
         fluidRow(br(), br(), br(), br(), h2("Hydrology and Meteorology Data", align = "center"), br())
),

####################################################################
# Forestry

tabPanel("Forestry",

         # Title
         fluidRow(br(), br(), br(), br(), h2("Forestry Data", align = "center"), br())
),

####################################################################
# Export

tabPanel("Export Data",

         # Title
         fluidRow(br(), br(), br(), br(), h2("Filter and Export Data", align = "center"), br()),

         navlistPanel(widths = c(2, 10),
                      "Water Quality Data",
                      tabPanel("Tributary",
                               fluidRow(column(10, h4("Filter and Export for Tributary WQ Data", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", export.wq.UI("mod.trib.quab.exp", df.trib.quab.exp, col.trib.quab.ware)),
                                 tabPanel("Ware River", export.wq.UI("mod.trib.ware.exp", df.trib.ware.exp, col.trib.quab.ware)),
                                 tabPanel("Wachusett", export.wq.UI("mod.trib.wach.exp", df.trib.wach.exp, col.trib.wach))
                               ) # end tabset Panel
                      ),
                      tabPanel("Bacteria (Res)",
                               fluidRow(column(10, h4("Filter and Export for Reservoir Bacteria WQ Data", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", export.wq.UI("mod.bact.quab.exp", df.bact.quab.exp, col = col.bact.quab)),
                                 tabPanel("Wachusett", export.wq.UI("mod.bact.wach.exp", df.bact.wach.exp, col = col.bact.wach))
                               ) # end tabset Panel
                      ),
                      tabPanel("Chemical (Res)",
                               fluidRow(column(10, h4("Filter and Export for Reservoir Chemical WQ Data", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", export.wq.UI("mod.chem.quab.exp", df.chem.quab.exp, col = col.chem.quab)),
                                 tabPanel("Wachusett", export.wq.UI("mod.chem.wach.exp", df.chem.wach.exp, col = col.chem.wach))
                               ) # end tabset Panel
                      ),
                      tabPanel("Profile (Res)",
                               fluidRow(column(10, h4("Filter and Export for Reservoir Profile WQ Data", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", export.wq.UI("mod.prof.quab.exp", df.prof.quab.exp, col = col.prof.quab)),
                                 tabPanel("Wachusett", export.wq.UI("mod.prof.wach.exp", df.prof.wach.exp, col = col.prof.wach))
                               ) # end tabset Panel
                      ),
                      "Hydro and Met",
                      tabPanel("Hydro/Met Data",
                               fluidRow(column(10, h4("Filter and Export for Hydro and Met Data", align = "center")), column(2))
                      ),
                      "Sampling Info",
                      tabPanel("Site Locations",
                               fluidRow(column(10, h4("Filter and Export for Site Location Data", align = "center")), column(2))
                      ),
                      tabPanel("Parameters",
                               fluidRow(column(10, h4("Filter and Export for Parameter Data", align = "center")), column(2))
                      )
         ) # end navlist
),


#########################################################
# Reports

tabPanel("Report",
         # Title
         fluidRow(br(), br(), br(), br(), h2("Report Generation Tool", align = "center"), br()),

         navlistPanel(widths = c(2, 10),
                      "Preset Reports",
                      tabPanel("Annual WQ",
                               fluidRow(column(10, h4("Annual Water Quality Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", report.awq.UI("mod.quab.awq", df.trib.quab)),
                                 tabPanel("Wachusett", report.awq.UI("mod.wach.awq", df.trib.wach))
                               ) # end tabset Panel
                      ), # end tabpanel
                      tabPanel("Monthly WQ",
                               fluidRow(column(10, h4("Monthly Water Quality Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", report.mwq.UI("mod.quab.mwq", df.trib.quab)),
                                 tabPanel("Wachusett", report.mwq.UI("mod.wach.mwq", df.trib.wach))
                               ) # end tabset panel
                      ), # end tabpanel
                      "Custom Reports",
                      tabPanel("Tributary",
                               fluidRow(column(10, h4("Tributary Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", report.custom.UI("mod.trib.quab.rep", df.trib.quab)),
                                 tabPanel("Ware River", report.custom.UI("mod.trib.ware.rep", df.trib.ware)),
                                 tabPanel("Wachusett", report.custom.UI("mod.trib.wach.rep", df.trib.wach))
                               )
                      ),
                      tabPanel("Bacteria (Res)",
                               fluidRow(column(10, h4("Reservoir Bacteria Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", report.custom.UI("mod.bact.quab.rep", df.bact.quab)),
                                 tabPanel("Wachusett", report.custom.UI("mod.bact.wach.rep", df.bact.wach))
                               )
                      ),
                      tabPanel("Chemical (Res)",
                               fluidRow(column(10, h4("Reservoir Chemical Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", report.custom.UI("mod.chem.quab.rep", df.chem.quab)),
                                 tabPanel("Wachusett", report.custom.UI("mod.chem.wach.rep", df.chem.wach))
                               )
                      ),
                      tabPanel("Profile (Res)",
                               fluidRow(column(10, h4("Profile Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", report.custom.UI("mod.prof.quab.rep", df.prof.quab)),
                                 tabPanel("Wachusett", report.custom.UI("mod.prof.wach.rep", df.prof.wach))
                               )
                      ),
                      tabPanel("Phytoplankton",
                               fluidRow(column(10, h4("Phytoplankton Custom Reports", align = "center")), column(2))
                      )
         ) # end navlist

) # end tabpanel (page)

#######################################################

) # end UI

########################################################################################
################################    Server   ###########################################
########################################################################################

server <- function(input, output, session) {

######################################################
# Tributary Water Quality Data

  callModule(Home, "Home", df.site = df.all.site)

######################################################
# Tributary

  # Time Series
  callModule(time, "mod.trib.quab.time", df = df.trib.quab, df.site = df.trib.quab.site)
  callModule(time, "mod.trib.ware.time", df = df.trib.ware, df.site = df.trib.ware.site)
  callModule(time, "mod.trib.wach.time", df = df.trib.wach, df.site = df.trib.wach.site)
  callModule(time, "mod.trib.all.time", df = df.trib.all, df.site = df.trib.all.site)

  # Regression
  callModule(regress, "mod.trib.quab.regr", df = df.trib.quab, df.site = df.trib.quab.site)
  callModule(regress, "mod.trib.ware.regr", df = df.trib.ware, df.site = df.trib.ware.site)
  callModule(regress, "mod.trib.wach.regr", df = df.trib.wach, df.site = df.trib.wach.site)
  callModule(regress, "mod.trib.all.regr", df = df.trib.all, df.site = df.trib.all.site)

#############################################################
# Reservoir

  # Bacteria
  callModule(time, "mod.bact.quab.time", df = df.bact.quab, df.site = df.bact.quab.site)
  callModule(time, "mod.bact.wach.time", df = df.bact.wach, df.site = df.bact.wach.site)

  callModule(regress, "mod.bact.quab.regr", df = df.bact.quab, df.site = df.bact.quab.site)
  callModule(regress, "mod.bact.wach.regr", df = df.bact.wach, df.site = df.bact.wach.site)

  # Chemical
  callModule(time.depth, "mod.chem.quab.time", df = df.chem.quab, df.site = df.chem.quab.site)
  callModule(time.depth, "mod.chem.wach.time", df = df.chem.wach, df.site = df.chem.wach.site)

  callModule(regress.depth, "mod.chem.quab.regr", df = df.chem.quab, df.site = df.chem.quab.site)
  callModule(regress.depth, "mod.chem.wach.regr", df = df.chem.wach, df.site = df.chem.wach.site)

  # Profile (physicochemical)
  callModule(prof.heatmap, "mod.prof.quab.heat", df = df.prof.quab)
  callModule(prof.heatmap, "mod.prof.wach.heat", df = df.prof.wach)

  callModule(prof.line, "mod.prof.quab.line", df = df.prof.quab)
  callModule(prof.line, "mod.prof.wach.line", df = df.prof.wach)

  callModule(prof.summary, "mod.prof.quab.sum", df = df.prof.quab)
  callModule(prof.summary, "mod.prof.wach.sum", df = df.prof.wach)

  # AquaBio


####################################################################
# Map Plot

  # Trib
  callModule(map.plot, "mod.trib.quab.map", df = df.trib.quab, df.site = df.trib.quab.site)
  callModule(map.plot, "mod.trib.ware.map", df = df.trib.ware, df.site = df.trib.ware.site)
  callModule(map.plot, "mod.trib.wach.map", df = df.trib.wach, df.site = df.trib.wach.site)
  callModule(map.plot, "mod.trib.all.map", df = df.trib.all, df.site = df.trib.all.site)

  # Bacteria
  callModule(map.plot, "mod.bact.quab.map", df = df.bact.quab, df.site = df.bact.quab.site)
  callModule(map.plot, "mod.bact.wach.map", df = df.bact.wach, df.site = df.bact.wach.site)

  # Chemical
  callModule(map.plot, "mod.chem.quab.map", df = df.chem.quab, df.site = df.chem.quab.site)
  callModule(map.plot, "mod.chem.wach.map", df = df.chem.wach, df.site = df.chem.wach.site)

####################################################################

# Hydrology/Meteorology/Statistics

####################################################################
# Export

  callModule(export.wq, "mod.trib.quab.exp", df = df.trib.quab.exp, df.site = df.trib.quab.site, col = col.trib.quab.ware)
  callModule(export.wq, "mod.trib.ware.exp", df = df.trib.ware.exp, df.site = df.trib.ware.site, col = col.trib.quab.ware)
  callModule(export.wq, "mod.trib.wach.exp", df = df.trib.wach.exp, df.site = df.trib.wach.site, col = col.trib.wach)
  callModule(export.wq, "mod.bact.quab.exp", df = df.bact.quab.exp, df.site = df.bact.quab.site, col = col.bact.quab)
  callModule(export.wq, "mod.bact.wach.exp", df = df.bact.wach.exp, df.site = df.bact.wach.site, col = col.bact.wach)
  callModule(export.wq, "mod.chem.quab.exp", df = df.chem.quab.exp, df.site = df.chem.quab.site, col = col.chem.quab)
  callModule(export.wq, "mod.chem.wach.exp", df = df.chem.wach.exp, df.site = df.chem.wach.site, col = col.chem.wach)
  callModule(export.wq, "mod.prof.quab.exp", df = df.prof.quab.exp, df.site = df.chem.quab.site, col = col.prof.quab) # fix site
  callModule(export.wq, "mod.prof.wach.exp", df = df.prof.wach.exp, df.site = df.chem.wach.site, col = col.prof.wach) # fix site

  callModule(export.wq, "Test 1", df = df.prof.wach.exp, df.site = df.chem.wach.site, col = col.prof.wach) # fix site
  #callModule(export.wq, "Test 2", df = df.prof.wach.exp, df.site = df.chem.wach.site, col = col.prof.wach) # fix site
  #callModule(export.wq, "Test 3", df = df.prof.wach.exp, df.site = df.chem.wach.site, col = col.prof.wach) # fix site

  ####################################################################
# Reports

  callModule(report.awq, "mod.quab.awq", df.trib = df.trib.quab, df.res = df.res.quab, df.prof = df.prof.quab, df.site = df.quab.site)
  callModule(report.awq, "mod.wach.awq", df.trib = df.trib.wach, df.res = df.res.wach, df.prof = df.prof.wach, df.site = df.wach.site)
  callModule(report.mwq, "mod.quab.mwq", df.trib = df.trib.quab, df.res = df.res.quab, df.prof = df.prof.quab, df.site = df.quab.site)
  callModule(report.mwq, "mod.wach.mwq", df.trib = df.trib.wach, df.res = df.res.wach, df.prof = df.prof.wach, df.site = df.wach.site)
  callModule(report.custom, "mod.trib.quab.rep", df = df.trib.quab, df.site = df.trib.quab.site)
  callModule(report.custom, "mod.trib.ware.rep", df = df.trib.ware, df.site = df.trib.ware.site)
  callModule(report.custom, "mod.trib.wach.rep", df = df.trib.quab, df.site = df.trib.quab.site)
  callModule(report.custom, "mod.bact.quab.rep", df = df.res.quab, df.site = df.bact.quab.site)
  callModule(report.custom, "mod.bact.wach.rep", df = df.res.quab, df.site = df.bact.quab.site)
  callModule(report.custom, "mod.chem.quab.rep", df = df.res.quab, df.site = df.chem.quab.site)
  callModule(report.custom, "mod.chem.wach.rep", df = df.res.quab, df.site = df.chem.quab.site)
  callModule(report.custom, "mod.prof.quab.rep", df = df.prof.quab, df.site = df.res.quab.site)
  callModule(report.custom, "mod.prof.wach.rep", df = df.prof.wach, df.site = df.res.wach.site)

#######################################################################

# Code to stop app when browser session window closes
session$onSessionEnded(function() {
      stopApp()
    })

} # end server function

#combines the user interface and server (it's a must)
shinyApp(ui = ui, server = server)






