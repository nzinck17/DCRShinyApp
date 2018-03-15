 ##############################################################################################################################
#     Title: app.R
#     Type: Master file for DCR Shiny App
#     Description: This Shiny App contains the "master" script for the app. The app contains a ui and server component
#           and sources R scripts from the App folder
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#   1_
#
# To-Do List:
#   1_

####################################################################################################
# Load Libraries and Script (Sources, Modules, and Functions)
#####################################################################################################

## ipak function
 ipak <- function(pkg){
   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
   if (length(new.pkg))
     install.packages(new.pkg, dependencies = TRUE, repos="http://cran.rstudio.com/")
   sapply(pkg, require, character.only = TRUE)
 }

#### NOTE - Shiny must be installed and loaded in the LaunchAppGitHub.R script - any other packages requred should be listed below
packages <- c("shiny", "rmarkdown", "knitr", "tidyverse", "lubridate", "plotly", "leaflet", "RColorBrewer",
              "DT", "akima", "odbc", "DBI", "scales", "stringr", "cowplot", "shinythemes","rgdal", "reshape2")
ipak(packages)

## Fetch all of the cached rds data for the app:
    # Directory with saved .rds files
    datadir <- config[1]

    # Make a list of all the .rds files using full path
    rds_files <- list.files(datadir,full.names = TRUE ,pattern = "\\.rds$")

    # create an object that contains all of the rds files
    data <- lapply(rds_files, readRDS)

    # Make a list of the df names by eliminating extension from files
    df_names <- gsub(".rds", "", list.files(datadir))

    # name each df in the data object appropriately
    names(data) <- df_names
    # Extract each element of the data object into the global environment
    list2env(data ,.GlobalEnv)

#source("sources/Settings.R")

    tab_selected = "Wachusett"

### Format Changes - ! to be moved to Update_Wave !

  df_chem_quab <- df_chem_quab %>% rename(LocationDepth = Sampling_Level)

### Load Primary Modules

source("modules1/home.R")
source("modules1/filter_wq.R")
source("modules1/time_wq.R")
source("modules1/time_depth_wq.R")
source("modules1/correlation_wq.R")
source("modules1/correlation_depth_wq.R")
source("modules1/distribution_wq.R")
source("modules1/metadata.R")
source("modules1/profile_heatmap.R")
source("modules1/profile_line.R")
source("modules1/profile_table_stats.R")
source("modules1/phyto.R")
source("modules1/map_plot.R")
source("modules1/report_AWQ.R")
source("modules1/report_MWQ.R")
source("modules1/report_custom.R")

### Load Secondary Modules (Modules used inside a primary module)

# Inputs
source("modules2/inputs/site_checkbox.R")
source("modules2/inputs/station_level_checkbox.R")
source("modules2/inputs/site_profile.R")
source("modules2/inputs/param_select.R")
source("modules2/inputs/param_checkbox.R")
source("modules2/inputs/date_select.R")
source("modules2/inputs/checkbox_select_all.R")
source("modules2/inputs/select_select_all.R")
source("modules2/inputs/plot_theme_and_hlines.R")
source("modules2/inputs/plot_text_and_vlines_time.R")
source("modules2/inputs/plot_text_and_vlines_corr.R")
source("modules2/inputs/plot_title_and_labels.R")
source("modules2/inputs/plot_save.R")

# Outputs
source("modules2/outputs/plot_time_wq.R")
source("modules2/outputs/plot_time_depth_wq.R")
source("modules2/outputs/plot_corr_wq.R")
source("modules2/outputs/plot_corr_depth_wq.R")
source("modules2/outputs/plot_corr_matrix_wq.R")
source("modules2/outputs/plot_profline_custom.R")
#source("modules2/outputs/plot-Profline-Standard.R")
#source("modules2/outputs/plot-Heatmap-Custom.R")
#source("modules2/outputs/plot-Heatmap-Standard.R")
source("modules2/outputs/stats_time_wq.R")
source("modules2/outputs/stats_time_depth_wq.R")
#source("Modules2/outputs/Summary-Profile.R")
source("modules2/outputs/site_map.R")
source("modules2/outputs/site_map_single.R")


### Load Functions

source("functions/stat_functions.R")
source("functions/phyto_plots.R")

###################################################################################
##################################  User Interface  ###############################
###################################################################################
#               font-family: 'Lobster', cursive;
ui <- tagList(
  # Creates padding at top for navBar space due to "fixed-top" position
  tags$style(type='text/css',
             'body {padding-top: 70px;}',
             'h2 {
               font-family: "Arial Black";
               font-weight: 500;
               line-height: 1.1;
               color: #0C4B91;
             }'
             ),

  navbarPage(NULL, position = "fixed-top", inverse = TRUE, collapsible = TRUE, theme = shinytheme("cerulean"),
             windowTitle = "WAVE", footer = uiOutput("footer_ui"),

######################################################
# Home Page

tabPanel("Home",

         fluidRow(
           column(3, imageOutput("dcr_image", height = 80), align = "left"),
           column(6, imageOutput("wave_image1", height = 80), align = "center"),
           column(3, imageOutput("umass_image", height = 80), align = "right")
         ),
         HOME_UI("home")

),


######################################################
# Tributary Water Quality Data

tabPanel("Tributary",


  # Title
  fluidRow(
           column(2, imageOutput("wave_image3", height = 50), align = "center"),
           column(10, h2("Tributary Water Quality Data", align = "center"))
  ),
  tabsetPanel(
    tabPanel("Quabbin",
             navlistPanel(widths = c(2, 10),
                          tabPanel("Select / Filter Data", FILTER_WQ_UI("mod_trib_quab_filter")),
                          tabPanel("-- Plots",
                                   tabsetPanel(
                                     tabPanel("Time-Series Scatter", PLOT_TIME_WQ_UI("mod_trib_quab_plot_time")),
                                     tabPanel("Correlation Scatter", PLOT_CORR_WQ_UI("mod_trib_quab_plot_corr")),
                                     tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_trib_quab_plot_dist"))
                                   )
                          ),
                          tabPanel("-- Statistics",
                                   tabsetPanel(
                                     tabPanel("Summary Statistics", STAT_TIME_WQ_UI("mod_trib_quab_stat_sum")),
                                     tabPanel("Temporal Statistics", fluidRow(h5("Mann-Kendall Stats to come"))),
                                     tabPanel("Correlation Matrix", PLOT_CORR_MATRIX_WQ_UI("mod_trib_quab_stat_cormat"))
                                   )
                          ),
                          tabPanel("Geospatial", MAP_PLOT_UI("mod_trib_quab_map", df = df_trib_quab)),
                          tabPanel("Metadata", METADATA_UI("mod_trib_quab_meta"))
             ) # end navlist panel
    ),
    tabPanel("Ware",
             navlistPanel(widths = c(2, 10),
                          tabPanel("Select / Filter Data", FILTER_WQ_UI("mod_trib_ware_filter")),
                          tabPanel("-- Plots",
                                   tabsetPanel(
                                     tabPanel("Time-Series Scatter", PLOT_TIME_WQ_UI("mod_trib_ware_plot_time")),
                                     tabPanel("Correlation Scatter", PLOT_CORR_WQ_UI("mod_trib_ware_plot_corr")),
                                     tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_trib_ware_plot_dist"))
                                   )
                          ),
                          tabPanel("-- Statistics",
                                   tabsetPanel(
                                     tabPanel("Summary Statistics", STAT_TIME_WQ_UI("mod_trib_ware_stat_sum")),
                                     tabPanel("Temporal Statistics", fluidRow(h5("Mann-Kendall Stats to come"))),
                                     tabPanel("Correlation Matrix", PLOT_CORR_MATRIX_WQ_UI("mod_trib_ware_stat_cormat"))
                                   )
                          ),
                          tabPanel("Geospatial", MAP_PLOT_UI("mod_trib_ware_map", df = df_trib_ware)),
                          tabPanel("Metadata", fluidRow(h5("See Quabbin Tab. Can add data here in future")))
             ) # end navlist panel
    ),
    tabPanel("Wachusett",
             navlistPanel(widths = c(2, 10),
                          tabPanel("Select / Filter Data", FILTER_WQ_UI("mod_trib_wach_filter")),
                          tabPanel("-- Plots",
                                   tabsetPanel(
                                     tabPanel("Time-Series Scatter", PLOT_TIME_WQ_UI("mod_trib_wach_plot_time")),
                                     tabPanel("Correlation Scatter", PLOT_CORR_WQ_UI("mod_trib_wach_plot_corr")),
                                     tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_trib_wach_plot_dist"))
                                   )
                          ),
                          tabPanel("-- Statistics",
                                   tabsetPanel(
                                     tabPanel("Summary Statistics", STAT_TIME_WQ_UI("mod_trib_wach_stat_sum")),
                                     tabPanel("Temporal Statistics", fluidRow(h5("Mann-Kendall Stats to come"))),
                                     tabPanel("Correlation Matrix", PLOT_CORR_MATRIX_WQ_UI("mod_trib_wach_stat_cormat"))
                                   )
                          ),
                          tabPanel("Geospatial", MAP_PLOT_UI("mod_trib_wach_map", df = df_trib_wach)),
                          tabPanel("MetaData", METADATA_UI("mod_trib_wach_meta"))
             ) # end navlist panel
    ),
    selected = tab_selected
  )

), # end Tributary tabpanel (page)


#############################################################
# Reservoir

tabPanel("Reservoir",

   # Title
   fluidRow(
            column(2, imageOutput("wave_image4", height = 50), align = "center"),
            column(10, h2("Reservoir Water Quality Data", align = "center"))
   ),
   tabsetPanel(
     tabPanel("Quabbin",
              navlistPanel(widths = c(2, 10),
                           "Chemical",
                           tabPanel("Select / Filter data", FILTER_WQ_UI("mod_chem_quab_filter")),
                           tabPanel("-- Plots",
                                    tabsetPanel(
                                      tabPanel("Time-Series Scatter", PLOT_TIME_WQ_UI("mod_chem_quab_plot_time")),
                                      tabPanel("Correlation Scatter", PLOT_CORR_WQ_UI("mod_chem_quab_plot_corr")),
                                      tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_chem_quab_plot_dist"))
                                    )
                           ),
                           tabPanel("-- Statistics",
                                    tabsetPanel(
                                      tabPanel("Summary Statistics", STAT_TIME_DEPTH_WQ_UI("mod_chem_quab_stat_sum")),
                                      tabPanel("Temporal Statistics", fluidRow(h5("Mann-Kendall Stats to come"))),
                                      tabPanel("Correlation Matrix", PLOT_CORR_MATRIX_WQ_UI("mod_chem_quab_stat_cormat"))
                                    )
                           ),
                           tabPanel("Metadata", METADATA_UI("mod_chem_quab_meta")),
                           "Profile",
                           tabPanel("Heat Map", PROF_HEATMAP_UI("mod_prof_quab_heat", df_prof_wach)),
                           tabPanel("Line Plot", PROF_LINE_UI("mod_prof_quab_line", df_prof_wach)),
                           tabPanel("Table and Summary", PROF_TABLE_STAT_UI("mod_prof_quab_sum", df_prof_wach)),
                           tabPanel("Metadata", METADATA_UI("mod_prof_quab_meta"))
              ) # end navlist panel
     ),
     tabPanel("Wachusett",
              navlistPanel(widths = c(2, 10),
                           "Bacteria",
                           tabPanel("Select / Filter Data", FILTER_WQ_UI("mod_bact_wach_filter")),
                           tabPanel("-- Plots",
                                    tabsetPanel(
                                      tabPanel("Time-Series Scatter", PLOT_TIME_WQ_UI("mod_bact_wach_plot_time")),
                                      tabPanel("Correlation Scatter", PLOT_CORR_WQ_UI("mod_bact_wach_plot_corr")),
                                      tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_bact_wach_plot_dist"))
                                    )
                           ),
                           tabPanel("-- Statistics",
                                    tabsetPanel(
                                      tabPanel("Summary Statistics", STAT_TIME_WQ_UI("mod_bact_wach_stat_sum")),
                                      tabPanel("Temporal Statistics", fluidRow(h5("Mann-Kendall Stats to come"))),
                                      tabPanel("Correlation Matrix", PLOT_CORR_MATRIX_WQ_UI("mod_bact_wach_stat_cormat"))
                                    )
                           ),
                           tabPanel("Geospatial", MAP_PLOT_UI("mod_bact_wach_map", df = df_bact_wach)),
                           tabPanel("Metadata", METADATA_UI("mod_bact_wach_meta")),
                           "Chemical",
                           tabPanel("Select / Filter Data", FILTER_WQ_UI("mod_chem_wach_filter")),
                           tabPanel("-- Plots",
                                    tabsetPanel(
                                      tabPanel("Time-Series Scatter", PLOT_TIME_WQ_UI("mod_chem_wach_plot_time")),
                                      tabPanel("Correlation Scatter", PLOT_CORR_WQ_UI("mod_chem_wach_plot_corr")),
                                      tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_chem_wach_plot_dist"))
                                    )
                           ),
                           tabPanel("-- Statistics",
                                    tabsetPanel(
                                      tabPanel("Summary Statistics", STAT_TIME_DEPTH_WQ_UI("mod_chem_wach_stat_sum")),
                                      tabPanel("Temporal Statistics", fluidRow(h5("Mann-Kendall Stats to come"))),
                                      tabPanel("Correlation Matrix", PLOT_CORR_MATRIX_WQ_UI("mod_chem_wach_stat_cormat"))
                                    )
                           ),
                           tabPanel("Metadata", METADATA_UI("mod_chem_wach_meta")),
                           "Profile",
                           tabPanel("Select / Filter Data", FILTER_WQ_UI("mod_prof_wach_filter")),
                           tabPanel("-- Plots",
                                    tabsetPanel(
                                      tabPanel("Heat Map", PROF_HEATMAP_UI("mod_prof_wach_heat", df_prof_wach)),
                                      tabPanel("Line Plot", PROF_LINE_UI("mod_prof_wach_line", df_prof_wach)),
                                      tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_prof_wach_plot_dist"))
                                    )
                           ),
                           tabPanel("Table and Summary", PROF_TABLE_STAT_UI("mod_prof_wach_sum", df_prof_wach)),
                           tabPanel("Metadata", METADATA_UI("mod_prof_wach_meta")),
                           "Biological",
                           tabPanel("Phytoplankton", PHYTO_UI("mod_phyto_wach_plots", df_phyto_wach))
              ) # end navlist panel
     ),
     selected = tab_selected
   )
 ),  # end Tabpanel (page)

###################################################################
# Hydrology/Meteorology

tabPanel("Met/Hydro",

         # Title
         fluidRow(
                  column(2, imageOutput("wave_image6", height = 50), align = "center"),
                  column(10, h2("Hydrology and Meteorology Data", align = "center"))
         )
),

####################################################################
# Forestry

tabPanel("Forestry",

         # Title
         fluidRow(
                  column(2, imageOutput("wave_image7", height = 50), align = "center"),
                  column(10, h2("Forestry Data", align = "center"))
         )
),

#########################################################
# Reports

tabPanel("Report",

         # Title
         fluidRow(
                  column(2, imageOutput("wave_image8", height = 50), align = "center"),
                  column(10, h2("Report Generation", align = "center"))
         ),
         navlistPanel(widths = c(2, 10),
                      "Preset Reports",
                      tabPanel("Annual WQ",
                               fluidRow(column(10, h4("Annual Water Quality Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", REPORT_AWQ_UI("mod_quab_awq", df_trib_quab)),
                                 tabPanel("Wachusett", REPORT_AWQ_UI("mod_wach_awq", df_trib_wach))
                               ) # end tabset Panel
                      ), # end tabpanel
                      tabPanel("Monthly WQ",
                               fluidRow(column(10, h4("Monthly Water Quality Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", REPORT_MWQ_UI("mod_quab_mwq", df_trib_quab)),
                                 tabPanel("Wachusett", REPORT_MWQ_UI("mod_wach_mwq", df_trib_wach))
                               ) # end tabset panel
                      ), # end tabpanel
                      "Custom Reports",
                      tabPanel("Tributary",
                               fluidRow(column(10, h4("Tributary Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", REPORT_CUSTOM_UI("mod_trib_quab_rep", df_trib_quab)),
                                 tabPanel("Ware River", REPORT_CUSTOM_UI("mod_trib_ware_rep", df_trib_ware)),
                                 tabPanel("Wachusett", REPORT_CUSTOM_UI("mod_trib_wach_rep", df_trib_wach))
                               )
                      ),
                      tabPanel("Bacteria (Res)",
                               fluidRow(column(10, h4("Reservoir Bacteria Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Wachusett", REPORT_CUSTOM_UI("mod_bact_wach_rep", df_bact_wach))
                               )
                      ),
                      tabPanel("Chemical (Res)",
                               fluidRow(column(10, h4("Reservoir Chemical Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", REPORT_CUSTOM_UI("mod_chem_quab_rep", df_chem_quab)),
                                 tabPanel("Wachusett", REPORT_CUSTOM_UI("mod_chem_wach_rep", df_chem_wach))
                               )
                      ),
                      tabPanel("Profile (Res)",
                               fluidRow(column(10, h4("Profile Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", REPORT_CUSTOM_UI("mod_prof_quab_rep", df_prof_quab)),
                                 tabPanel("Wachusett", REPORT_CUSTOM_UI("mod_prof_wach_rep", df_prof_wach))
                               )
                      ),
                      tabPanel("Phytoplankton",
                               fluidRow(column(10, h4("Phytoplankton Custom Reports", align = "center")), column(2))
                      )
         ) # end navlist

) # end tabpanel (page)

#########################################################
) # end tagList

) # end UI

########################################################################################
################################    Server   ###########################################
########################################################################################

server <- function(input, output, session) {

######################################################
# Home

  callModule(HOME, "home", df_site = df_all_site)


######################################################
# Tributary

  ### Quabbin

  # Filter
  Df_Trib_Quab <- callModule(FILTER_WQ, "mod_trib_quab_filter", df = df_trib_quab, df_site = df_trib_quab_site, type = "wq")

  # Plots
  callModule(PLOT_TIME_WQ, "mod_trib_quab_plot_time", Df = Df_Trib_Quab$Long)
  callModule(PLOT_CORR_WQ, "mod_trib_quab_plot_corr", Df = Df_Trib_Quab$Long)
  callModule(DISTRIBUTION_WQ, "mod_trib_quab_plot_dist", Df = Df_Trib_Quab$Long)

  # Stats
  callModule(STAT_TIME_WQ, "mod_trib_quab_stat_sum", Df = Df_Trib_Quab$Long)  # Push in $Stats and get rid in module
  # temp stats
  callModule(PLOT_CORR_MATRIX_WQ, "mod_trib_quab_stat_cormat", Df = Df_Trib_Quab$Wide)

  # Geospatial
  callModule(MAP_PLOT, "mod_trib_quab_map", df = df_trib_quab, df_site = df_trib_quab_site)

  # MetaData
  callModule(METADATA, "mod_trib_quab_meta", df = df_trib_quab, df_site = df_trib_quab_site, df_param = df_quab_param)


  ### Ware

  # Filter
  Df_Trib_Ware <- callModule(FILTER_WQ, "mod_trib_ware_filter", df = df_trib_ware, df_site = df_trib_ware_site, type = "wq")

  # Plots
  callModule(PLOT_TIME_WQ, "mod_trib_ware_plot_time", Df = Df_Trib_Ware$Long)
  callModule(PLOT_CORR_WQ, "mod_trib_ware_plot_corr", Df = Df_Trib_Ware$Long)
  callModule(DISTRIBUTION_WQ, "mod_trib_ware_plot_dist", Df = Df_Trib_Ware$Long)

  # Stats
  callModule(STAT_TIME_WQ, "mod_trib_ware_stat_sum", Df = Df_Trib_Ware$Long)  # Push in $Stats and get rid in module
  # temp stats
  callModule(PLOT_CORR_MATRIX_WQ, "mod_trib_ware_stat_cormat", Df = Df_Trib_Ware$Wide)

  # Geospatial
  callModule(MAP_PLOT, "mod_trib_ware_map", df = df_trib_ware, df_site = df_trib_ware_site)


  ### Wachusett

  # Filter
  Df_Trib_Wach <- callModule(FILTER_WQ, "mod_trib_wach_filter", df = df_trib_wach, df_site = df_trib_wach_site,
                             df_flags = df_flags, df_flag_sample_index = df_flag_sample_index, type = "wq")

  # Plots
  callModule(PLOT_TIME_WQ, "mod_trib_wach_plot_time", Df = Df_Trib_Wach$Long)
  callModule(PLOT_CORR_WQ, "mod_trib_wach_plot_corr", Df = Df_Trib_Wach$Long)
  callModule(DISTRIBUTION_WQ, "mod_trib_wach_plot_dist", Df = Df_Trib_Wach$Long)

  # Stats
  callModule(STAT_TIME_WQ, "mod_trib_wach_stat_sum", Df = Df_Trib_Wach$Long)  # Push in $Stats and get rid in module
  # temp stats
  callModule(PLOT_CORR_MATRIX_WQ, "mod_trib_wach_stat_cormat", Df = Df_Trib_Wach$Wide)

  # Geospatial
  callModule(MAP_PLOT, "mod_trib_wach_map", df = df_trib_wach, df_site = df_trib_wach_site)

  # MetaData
  callModule(METADATA, "mod_trib_wach_meta", df = df_trib_wach, df_site = df_trib_wach_site, df_param = df_wach_param)



#############################################################
# Reservoir


##### Quabbin #####

  ### Chemical

  # Filter
  Df_Chem_Quab <- callModule(FILTER_WQ, "mod_chem_quab_filter", df = df_chem_quab, df_site = df_chem_quab_site, type = "wq_depth")

  # Plots
  callModule(PLOT_TIME_WQ, "mod_chem_quab_plot_time", Df = Df_Chem_Quab$Long) # Update to Depth specific plot
  callModule(PLOT_CORR_WQ, "mod_chem_quab_plot_corr", Df = Df_Chem_Quab$Long) # Update to Depth Specific Plot
  callModule(DISTRIBUTION_WQ, "mod_chem_quab_plot_dist", Df = Df_Chem_Quab$Long) # Depth Specific?

  # Stats
  callModule(STAT_TIME_DEPTH_WQ, "mod_chem_quab_stat_sum", Df = Df_Chem_Quab$Long) # Push in $Stats and get rid in module
  # temp stats
  callModule(PLOT_CORR_MATRIX_WQ, "mod_chem_quab_stat_cormat", Df = Df_Chem_Quab$Wide)

  # MetaData
  callModule(METADATA, "mod_chem_quab_meta", df = df_chem_quab, df_site = df_chem_quab_site, df_param = df_chem_param)

  ### Profile

  # Add a Filter Tab???????? - probably

  # Heatmap
  callModule(PROF_HEATMAP, "mod_prof_quab_heat", df = df_prof_quab)

  # Line Plot
  callModule(PROF_LINE, "mod_prof_quab_line", df = df_prof_quab)
  # Table and Stats
  callModule(PROF_TABLE_STAT, "mod_prof_quab_sum", df = df_prof_quab)

  # Change the Data Frames to Profile Data
  callModule(METADATA, "mod_prof_quab_meta", df = df_prof_quab, df_site = df_prof_quab_site, df_param = df_quab_param)



##### Wachusett #####

  ### Bacteria

  # Filter
  Df_Bact_Wach <- callModule(FILTER_WQ, "mod_bact_wach_filter", df = df_bact_wach, df_site = df_bact_wach_site, type = "wq")

  # Plots
  callModule(PLOT_TIME_WQ, "mod_bact_wach_plot_time", Df = Df_Bact_Wach$Long)
  callModule(PLOT_CORR_WQ, "mod_bact_wach_plot_corr", Df = Df_Bact_Wach$Long)
  callModule(DISTRIBUTION_WQ, "mod_bact_wach_plot_dist", Df = Df_Bact_Wach$Long)

  # Stats
  callModule(STAT_TIME_WQ, "mod_bact_wach_stat_sum", Df = Df_Bact_Wach$Long)  # Push in $Stats and get rid in module
  # temp stats
  callModule(PLOT_CORR_MATRIX_WQ, "mod_bact_wach_stat_cormat", Df = Df_Bact_Wach$Wide)

  # Geospatial
  callModule(MAP_PLOT, "mod_bact_wach_map", df = df_bact_wach, df_site = df_bact_wach_site)

  # MetaData
  callModule(METADATA, "mod_bact_wach_meta", df = df_bact_wach, df_site = df_bact_wach_site, df_param = df_wach_param)

  ### Chemical

  # Filter
  Df_Chem_Wach <- callModule(FILTER_WQ, "mod_chem_wach_filter", df = df_chem_wach, df_site = df_chem_wach_site, type = "wq_depth")

  # Plots
  callModule(PLOT_TIME_WQ, "mod_chem_wach_plot_time", Df = Df_Chem_Wach$Long) # Update to Depth specific plot
  callModule(PLOT_CORR_WQ, "mod_chem_wach_plot_corr", Df = Df_Chem_Wach$Long) # Update to Depth Specific Plot
  callModule(DISTRIBUTION_WQ, "mod_chem_wach_plot_dist", Df = Df_Chem_Wach$Long) # Depth Specific?

  # Stats
  callModule(STAT_TIME_DEPTH_WQ, "mod_chem_wach_stat_sum", Df = Df_Chem_Wach$Long) # Push in $Stats and get rid in module
  # temp stats
  callModule(PLOT_CORR_MATRIX_WQ, "mod_chem_wach_stat_cormat", Df = Df_Chem_Wach$Wide)

  # MetaData
  callModule(METADATA, "mod_chem_wach_meta", df = df_chem_wach, df_site = df_chem_wach_site, df_param = df_chem_param)

  ### Profile

  # Filter
  Df_Prof_Wach <- callModule(FILTER_WQ, "mod_prof_wach_filter", df = df_prof_wach, df_site = df_prof_wach_site, type = "profile")

  # Plots
  callModule(PROF_HEATMAP, "mod_prof_wach_heat", df = df_prof_wach)
  callModule(PROF_LINE, "mod_prof_wach_line", df = df_prof_wach)
  callModule(DISTRIBUTION_WQ, "mod_prof_wach_plot_dist", Df = Df_Prof_Wach$Long) # Depth Specific?

  # Table and Stats
  callModule(PROF_TABLE_STAT, "mod_prof_wach_sum", df = df_prof_wach)

  # Change the Data Frames to Profile Data
  callModule(METADATA, "mod_prof_wach_meta", df = df_prof_wach, df_site = df_prof_wach_site, df_param = df_wach_param)

  ### AquaBio

  callModule(PHYTO, "mod_phyto_wach_plots", df = df_phyto_wach)


####################################################################
# Hydrology/Meteorology/Statistics

####################################################################
# Reports

  callModule(REPORT_AWQ, "mod_quab_awq", df_trib = df_trib_quab, df_chem = df_chem_quab, df_prof = df_prof_quab, df_site = df_trib_quab_site)
  callModule(REPORT_AWQ, "mod_wach_awq", df_trib = df_trib_wach, df_chem = df_chem_wach, df_prof = df_prof_wach, df_site = df_trib_wach_site)
  callModule(REPORT_MWQ, "mod_quab_mwq", df_trib = df_trib_quab, df_chem = df_chem_quab, df_prof = df_prof_quab, df_site = df_trib_quab_site)
  callModule(REPORT_MWQ, "mod_wach_mwq", df_trib = df_trib_wach, df_chem = df_chem_wach, df_prof = df_prof_wach, df_site = df_trib_wach_site)
  callModule(REPORT_CUSTOM, "mod_trib_quab_rep", df = df_trib_quab, df_site = df_trib_quab_site)
  callModule(REPORT_CUSTOM, "mod_trib_ware_rep", df = df_trib_ware, df_site = df_trib_ware_site)
  callModule(REPORT_CUSTOM, "mod_trib_wach_rep", df = df_trib_wach, df_site = df_trib_wach_site)
  callModule(REPORT_CUSTOM, "mod_bact_wach_rep", df = df_bact_wach, df_site = df_bact_wach_site)
  callModule(REPORT_CUSTOM, "mod_chem_quab_rep", df = df_chem_quab, df_site = df_chem_quab_site)
  callModule(REPORT_CUSTOM, "mod_chem_wach_rep", df = df_chem_wach, df_site = df_chem_wach_site)
  callModule(REPORT_CUSTOM, "mod_prof_quab_rep", df = df_prof_quab, df_site = df_prof_quab_site)
  callModule(REPORT_CUSTOM, "mod_prof_wach_rep", df = df_prof_wach, df_site = df_prof_wach_site)

#######################################################################
# Footer

  output$footer_ui <- renderUI({

    update_date <- "Coming Soon"

    text_db <- paste("Data last updated:", update_date)

    tagList(
      hr(),
      column(4,
             strong(text_db),
             br()
      ),
      column(8,
             tags$div(tags$em("Created by Nick Zinck, University of Massachusetts; and Dan Crocker, Massachusetts Department of Conservation and Recreation"), align = "right"),
             br()
      )
    )
  })

#######################################################################
# Images

  # DCR IMAGE
  output$dcr_image <- renderImage({
    list(src = "images/DCR.jpg",
         width= "160",
         height= "80")
  }, deleteFile = FALSE)

  # UMass IMAGE
  output$umass_image <- renderImage({
    list(src = "images/UMass.png",
         width= "240",
         height= "80")
  }, deleteFile = FALSE)

  # WAVE IMAGE 1
  output$wave_image1 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "360",
         height= "80")
  }, deleteFile = FALSE)

  # WAVE IMAGE 2
  output$wave_image2 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)

  # WAVE IMAGE 3
  output$wave_image3 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)

  # WAVE IMAGE 4
  output$wave_image4 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)

  # WAVE IMAGE 5
  output$wave_image5 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)


  # WAVE IMAGE 6
  output$wave_image6 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)


  # WAVE IMAGE 7
  output$wave_image7 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)


  # WAVE IMAGE 8
  output$wave_image8 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)






# Code to stop app when browser session window closes
session$onSessionEnded(function() {
      stopApp()
    })

} # end server function

#combines the user interface and server (it's a must)
shinyApp(ui = ui, server = server)






