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


#### NOTE - Shiny must be installed and loaded in the LaunchAppGitHub.R script - any other packages requred should be listed below
packages <- c("rmarkdown", "knitr", "tidyverse", "lubridate", "plotly", "leaflet", "RColorBrewer", 
              "DT", "akima", "odbc", "DBI", "scales", "stringr", "cowplot", "shinythemes")
ipak(packages)
### Run/Source Scripts that load data

source("Sources/LoadMSAccessData.R")
#source("Sources/Settings.R")

### Load Primary Modules

source("Modules/Home.R")
source("Modules/Time.R")
source("Modules/Regress.R")
source("Modules/Phyto.R")
source("Modules/Time-Depth.R")
source("Modules/Regress-Depth.R")
source("Modules/Profile-Heatmap.R")
source("Modules/Profile-Line.R")
source("Modules/Profile-Summary.R")
source("Modules/MapPlot.R")
source("Modules/Filter-WQ.R")
source("Modules/Metadata.R")
source("Modules/Report-AWQ.R")
source("Modules/Report-MWQ.R")
source("Modules/Report-Custom.R")

### Load Secondary Modules (Modules used inside a primary module)

# Inputs
source("Modules2/Inputs/SiteCheckbox.R")
source("Modules2/Inputs/StationLevelCheckbox.R")
source("Modules2/Inputs/ParamSelect.R")
source("Modules2/Inputs/ParamCheckbox.R")
source("Modules2/Inputs/DateSelect.R")
source("Modules2/Inputs/CheckboxSelectAll.R")
source("Modules2/Inputs/SelectInputSelectAll.R")

# Outputs
source("Modules2/Outputs/Plot-Time-Mod.R")
source("Modules2/Outputs/Plot-Time-Depth.R")
source("Modules2/Outputs/Plot-Regress.R")
source("Modules2/Outputs/Plot-Regress-Depth.R")
source("Modules2/Outputs/Plot-Profline-Custom.R")
#source("Modules2/Outputs/Plot-Profline-Standard.R")
#source("Modules2/Outputs/Plot-Heatmap-Custom.R")
#source("Modules2/Outputs/Plot-Heatmap-Standard.R")
source("Modules2/Outputs/Summary.R")
source("Modules2/Outputs/Summary-Depth.R")
#source("Modules2/Outputs/Summary-Profile.R")

# UI
source("Modules2/UI/SiteMap.R")


### Load Functions

source("Functions/GetSeasons.R")
source("Functions/circleSizeLegend.R")
source("Functions/Plots/Plot-Time-Func.R")
source("Functions/PhytoPlots.R")

###################################################################################
##################################  User Interface  ###############################
###################################################################################

ui <- navbarPage("DCR", position = "fixed-top", inverse = TRUE, collapsible = TRUE, theme = shinytheme("cerulean"), #

######################################################
# Home Page

tabPanel("Home",

  # Title
  fluidRow(br(), br(), br(), br(), h2("Water Quality Data Management System", align = "center")),
  fluidRow(h3("Department of Conservation and Recreation", align = "center"), br()),
  Home.UI("Home")

),


####################################################################
# Filter

tabPanel("Filter",
         
         # Title
         fluidRow(br(), br(), br(), br(), h2("Filter Data", align = "center"), br()),
         
         navlistPanel(widths = c(2, 10),
                      "Water Quality Data",
                      tabPanel("Tributary",
                               fluidRow(column(10, h4("Filter and Export for Tributary WQ Data", align = "center")), column(2)),
                               filter.wq.UI("mod.trib.filter")
                      ),
                      tabPanel("Bacteria (Res)",
                               fluidRow(column(10, h4("Filter and Export for Reservoir Bacteria WQ Data", align = "center")), column(2)),
                               filter.wq.UI("mod.bact.filter")
                      ),
                      tabPanel("Chemical (Res)",
                               fluidRow(column(10, h4("Filter and Export for Reservoir Chemical WQ Data", align = "center")), column(2)),
                               filter.wq.UI("mod.chem.filter")
                      ),
                      tabPanel("Profile (Res)",
                               fluidRow(column(10, h4("Filter and Export for Reservoir Profile WQ Data", align = "center")), column(2)),
                               filter.wq.UI("mod.prof.filter")
                      ),
                      "Hydro and Met",
                      tabPanel("Hydro/Met Data",
                               fluidRow(column(10, h4("Filter and Export for Hydro and Met Data", align = "center")), column(2))
                      )
         ) # end navlist
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
                          tabPanel("Quabbin", time.UI("mod.trib.quab.time")),
                          tabPanel("Ware River", time.UI("mod.trib.ware.time")),
                          tabPanel("Wachusett", time.UI("mod.trib.wach.time"))#,
                          #tabPanel("All Tribs", time.UI("mod.trib.all.time"))
                        ) # end tabset Panel
               ), # end tabpanel
               tabPanel("Regression",
                        fluidRow(column(10, h4("Tributary Regression Analysis", align = "center")), column(2)),
                        tabsetPanel(
                          tabPanel("Quabbin", regress.UI("mod.trib.quab.regr", df.trib.quab)),
                          tabPanel("Ware River", regress.UI("mod.trib.ware.regr", df.trib.ware)),
                          tabPanel("Wachusett", regress.UI("mod.trib.wach.regr", df.trib.wach))#,
                          #tabPanel("All Tribs", regress.UI("mod.trib.all.regr", df.trib.all))
                        ) # end tabset Panel
               ), # end tabpanel
               tabPanel("MetaData",
                        fluidRow(column(10, h4("Tributary MetaData", align = "center")), column(2)),
                        tabsetPanel(
                          tabPanel("Quabbin & Ware", metadata.UI("mod.trib.quab.meta")),
                          tabPanel("Wachusett", metadata.UI("mod.trib.wach.meta"))
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
                           tabPanel("Wachusett", time.UI("mod.bact.wach.time"))
                         ) # end tabset Panel
                ), # end tabpanel
                tabPanel("Regression",
                         fluidRow(column(10, h4("Bacteria Regression Analysis", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Wachusett", regress.UI("mod.bact.wach.regr", df.bact.wach))
                         ) # end tabset panel
                ), # end tabpanel
                tabPanel("MetaData",
                         fluidRow(column(10, h4("Tributary MetaData", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Wachusett", metadata.UI("mod.bact.wach.meta"))
                         ) # end tabset Panel
                ), # end tabpanel

                "Chemistry",
                tabPanel("Time-Series",
                         fluidRow(column(10, h4("Chemical Time-Series Analysis", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", time.depth.UI("mod.chem.quab.time")),
                           tabPanel("Wachusett", time.depth.UI("mod.chem.wach.time"))
                         )
                ),
                tabPanel("Regression",
                         fluidRow(column(10, h4("Chemical Regression Analysis", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", regress.depth.UI("mod.chem.quab.regr", df.chem.quab)),
                           tabPanel("Wachusett", regress.depth.UI("mod.chem.wach.regr", df.chem.wach))
                         )
                ),
                tabPanel("Metadata",
                         fluidRow(column(10, h4("Chemical MetaData", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", metadata.UI("mod.chem.quab.meta")),
                           tabPanel("Wachusett", metadata.UI("mod.chem.wach.meta"))
                         ) # end tabset Panel
                ), # end tabpanel

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
                ),
                tabPanel("Metadata",
                         fluidRow(column(10, h4("Profile MetaData", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Quabbin", metadata.UI("mod.prof.quab.meta")),
                           tabPanel("Wachusett", metadata.UI("mod.prof.wach.meta"))
                         ) # end tabset Panel
                ), # end tabpanel
                
                
                "Biological",
                tabPanel("Phytoplankton",
                         fluidRow(column(10, h4("Phytoplankton Plots and Data", align = "center")), column(2)),
                         tabsetPanel(
                           tabPanel("Wachusett", Phyto.UI("mod.phyto.wach.plots", df.phyto.wach))
                         )
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
                      tabPanel("Wachusett", map.plot.UI("mod.bact.wach.map", df = df.bact.wach)),
                      "Reservoir Chemical",
                      tabPanel("Quabbin", map.plot.UI("mod.chem.quab.map", df = df.chem.quab)),
                      tabPanel("Wachusett", map.plot.UI("mod.chem.wach.map", df = df.chem.wach))
         ) # end navlist

), # end tabpanel (page)


###################################################################
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

###################################################################
# Filter
  
  df.trib.filtered <- callModule(filter.wq, "mod.trib.filter", dfs = list(df.trib.wach, df.trib.quab, df.trib.ware))
  df.bact.filtered <- callModule(filter.wq, "mod.bact.filter", dfs = list(df.bact.wach))
  df.chem.filtered <- callModule(filter.wq, "mod.chem.filter", dfs = list(df.chem.wach, df.chem.quab))
  df.prof.filtered <- callModule(filter.wq, "mod.prof.filter", dfs = list(df.prof.quab, df.prof.wach))  # fix site

         
######################################################
# Tributary

  # Time Series
  callModule(time, "mod.trib.quab.time", df.full = df.trib.quab, df.filtered = df.trib.filtered[[2]], df.site = df.trib.quab.site)
  callModule(time, "mod.trib.ware.time", df.full = df.trib.ware, df.filtered = df.trib.filtered[[3]], df.site = df.trib.ware.site)
  callModule(time, "mod.trib.wach.time", df.full = df.trib.wach, df.filtered = df.trib.filtered[[1]], df.site = df.trib.wach.site)
  #callModule(time, "mod.trib.all.time", df = df.trib.all, df.site = df.trib.all.site)

  # Regression
  callModule(regress, "mod.trib.quab.regr", df = df.trib.quab, df.site = df.trib.quab.site)
  callModule(regress, "mod.trib.ware.regr", df = df.trib.ware, df.site = df.trib.ware.site)
  callModule(regress, "mod.trib.wach.regr", df = df.trib.wach, df.site = df.trib.wach.site)
  #callModule(regress, "mod.trib.all.regr", df = df.trib.all, df.site = df.trib.all.site)
  
  # Metadata
  callModule(metadata, "mod.trib.quab.meta", df.site = df.trib.quab.site, df.param = df.trib.quab.site, df.flag = df.trib.quab.site)
  callModule(metadata, "mod.trib.wach.meta", df.site = df.trib.wach.site, df.param = df.trib.wach.site, df.flag = df.trib.wach.site)

#############################################################
# Reservoir

  # Bacteria
  callModule(time, "mod.bact.wach.time", df.full = df.bact.wach, df.filtered = df.bact.filtered[[1]], df.site = df.bact.wach.site)

  callModule(regress, "mod.bact.wach.regr", df = df.bact.wach, df.site = df.bact.wach.site)

  callModule(metadata, "mod.bact.wach.meta", df.site = df.bact.wach.site, df.param = df.bact.wach.site, df.flag = df.bact.wach.site)
  
  # Chemical
  callModule(time.depth, "mod.chem.quab.time", df.full = df.chem.quab, df.filtered = df.chem.filtered[[2]], df.site = df.chem.quab.site)
  callModule(time.depth, "mod.chem.wach.time", df.full = df.chem.wach, df.filtered = df.chem.filtered[[1]], df.site = df.chem.wach.site)

  callModule(regress.depth, "mod.chem.quab.regr", df = df.chem.quab, df.site = df.chem.quab.site)
  callModule(regress.depth, "mod.chem.wach.regr", df = df.chem.wach, df.site = df.chem.wach.site)

  callModule(metadata, "mod.chem.quab.meta", df.site = df.chem.quab.site, df.param = df.chem.quab.site, df.flag = df.chem.quab.site)
  callModule(metadata, "mod.chem.wach.meta", df.site = df.chem.wach.site, df.param = df.chem.wach.site, df.flag = df.chem.wach.site)
  
  # Profile (physicochemical)
  callModule(prof.heatmap, "mod.prof.quab.heat", df = df.prof.quab)
  callModule(prof.heatmap, "mod.prof.wach.heat", df = df.prof.wach)

  callModule(prof.line, "mod.prof.quab.line", df = df.prof.quab)
  callModule(prof.line, "mod.prof.wach.line", df = df.prof.wach)

  callModule(prof.summary, "mod.prof.quab.sum", df = df.prof.quab)
  callModule(prof.summary, "mod.prof.wach.sum", df = df.prof.wach)

  callModule(metadata, "mod.prof.quab.meta", df.site = df.prof.quab.site, df.param = df.prof.quab.site, df.flag = df.prof.quab.site)
  callModule(metadata, "mod.prof.wach.meta", df.site = df.prof.wach.site, df.param = df.prof.wach.site, df.flag = df.prof.wach.site)
  
  # AquaBio
  callModule(Phyto, "mod.phyto.wach.plots", df = df.phyto.wach)
  #callModule(phyto.summary, "mod.phyto.wach.plots", df = df.phyto.wach)

####################################################################
# Map Plot

  # Trib
  callModule(map.plot, "mod.trib.quab.map", df.full = df.trib.quab, df.filtered = df.trib.filtered[[2]], df.site = df.trib.quab.site)
  callModule(map.plot, "mod.trib.ware.map", df.full = df.trib.ware, df.filtered = df.trib.filtered[[3]], df.site = df.trib.ware.site)
  callModule(map.plot, "mod.trib.wach.map", df.full = df.trib.wach, df.filtered = df.trib.filtered[[1]], df.site = df.trib.wach.site)
  callModule(map.plot, "mod.trib.all.map", df.full = df.trib.all, df.site = df.trib.all.site)

  # Bacteria
  callModule(map.plot, "mod.bact.wach.map", df.full = df.bact.wach, df.filtered = df.trib.filtered[[1]], df.site = df.bact.wach.site)

  # Chemical
  callModule(map.plot, "mod.chem.quab.map", df.full = df.chem.quab, df.filtered = df.trib.filtered[[2]], df.site = df.chem.quab.site)
  callModule(map.plot, "mod.chem.wach.map", df.full = df.chem.wach, df.filtered = df.trib.filtered[[1]], df.site = df.chem.wach.site)

####################################################################
# Hydrology/Meteorology/Statistics

####################################################################
# Reports

  callModule(report.awq, "mod.quab.awq", df.trib = df.trib.quab, df.chem = df.chem.quab, df.prof = df.prof.quab, df.site = df.quab.site)
  callModule(report.awq, "mod.wach.awq", df.trib = df.trib.wach, df.chem = df.chem.wach, df.prof = df.prof.wach, df.site = df.wach.site)
  callModule(report.mwq, "mod.quab.mwq", df.trib = df.trib.quab, df.chem = df.chem.quab, df.prof = df.prof.quab, df.site = df.quab.site)
  callModule(report.mwq, "mod.wach.mwq", df.trib = df.trib.wach, df.chem = df.chem.wach, df.prof = df.prof.wach, df.site = df.wach.site)
  callModule(report.custom, "mod.trib.quab.rep", df = df.trib.quab, df.site = df.trib.quab.site)
  callModule(report.custom, "mod.trib.ware.rep", df = df.trib.ware, df.site = df.trib.ware.site)
  callModule(report.custom, "mod.trib.wach.rep", df = df.trib.wach, df.site = df.trib.wach.site)
  callModule(report.custom, "mod.bact.wach.rep", df = df.bact.wach, df.site = df.bact.wach.site)
  callModule(report.custom, "mod.chem.quab.rep", df = df.chem.quab, df.site = df.chem.quab.site)
  callModule(report.custom, "mod.chem.wach.rep", df = df.chem.wach, df.site = df.chem.wach.site)
  callModule(report.custom, "mod.prof.quab.rep", df = df.prof.quab, df.site = df.prof.quab.site)
  callModule(report.custom, "mod.prof.wach.rep", df = df.prof.wach, df.site = df.prof.wach.site)

#######################################################################

# Code to stop app when browser session window closes
session$onSessionEnded(function() {
      stopApp()
    })

} # end server function

#combines the user interface and server (it's a must)
shinyApp(ui = ui, server = server)






