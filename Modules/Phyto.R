##############################################################################################################################
#     Title: Plot-Phyto.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Time series plots for phytoplankton data
#     Written by: Dan Crocker, Fall 2017
##############################################################################################################################

# Notes:
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:
#
#App location: Reservoir --> Biological --> Phytoplankton --> Wachusett
# 4 Tab Panels Phytoplankton, Taxa, Historical, Filter-Export
# Panel Outlines:
# Phytoplankton
# 3 fluid rows
# Row 1 has Data filter - Location Dropdown, Year dropdown, 2 slider bars 0-35 meters, button to create chart
# Row 2 is a big plot
# Row 3 has plot options

##############################################################################################################################
# User Interface
##############################################################################################################################

Phyto.UI <- function(id,df) {

  ns <- NS(id) # see General Note 1

  tagList(
    tabsetPanel(
        tabPanel("Phytoplankton Overview Plot",
             # Data filter for plot
             fluidRow(column(5, # fr2 - Site and Year Input
                             selectInput(ns("site"), "Station(s):",
                                         choices = df.phyto.wach %>% .$Station %>% levels() %>% paste(),
                                         multiple = TRUE,
                                         width = '200',
                                         selected = c("BN3417","CI3409")),
                             selectInput(ns("year"), "Year:",
                                         choices = df.phyto.wach %>%  .$Year %>%  unique() %>% sort(decreasing = TRUE),
                                         width = '200',
                                         selected = 1)
             ), # End Column
             column(1), # Spacer Column
             column(5, # Depth Range sliders
                    sliderInput(ns("Depth1"), "Epilimnion Depth Range (meters):",
                                min = 0, max = 4, value = c(1,4), step = 0.5),
                    sliderInput(ns("Depth2"), "Epi-Metalimnion Depth Range (meters):",
                                min = 6, max = 14, value = c(6,14), step = 0.5)
             ) # End Column
             ), # End fluid row
             fluidRow(br()),
             fluidRow(
               column(12, # fr3 - Plot
                  plotOutput(ns("PhytoPlot"), width = "100%", height = 600)
               )
             ), # End fr
               fluidRow(br(),
                  column(2,
                  downloadButton(ns('save.plot'), "Save Plot")
                  ),
                  # column(2,
                  # radioButtons(ns("plot.save.size"), "Plot Size:",
                  #                                choices= c("small", "medium","large"))
                  # ),
                  column(2,
                  radioButtons(ns("plot.save.type"), "File Type:",
                                                 choices= c("pdf","jpg","png"))
                  )
                  # column(2,
                  # checkboxGroupInput(ns("plot.save.grid"), "Gridline Override:",
                  #                     choices= c("major gridlines", "minor gridlines"))
                  # ) # end column
                ) # End fr
          ), # End Tab Panel Sub
        tabPanel("Taxa Plots",
                 # Function Args
                 fluidRow(column(4, # Sites
                                 selectInput(ns("taxasite"), "Station(s):",
                                             choices = df.phyto.wach %>% .$Station %>% levels() %>% paste(),
                                             multiple = TRUE,
                                             width = '200',
                                             selected = c("BN3417","CI3409"))
                          ), # End Col
                          column(4, # Year
                                 selectInput(ns("taxayear"), "Year:",
                                             choices = df.phyto.wach %>%  .$Year %>%  unique() %>% sort(decreasing = TRUE),
                                             width = '200',
                                             selected = 1)
                          ) # End Col
                 ), # End fr
                 fluidRow(br()),
                 fluidRow(column(9, plotOutput(ns("taxaplot1"), width = "100%", height = 400))),
                 fluidRow(column(9, plotOutput(ns("taxaplot2"), width = "100%", height = 400))),
                 fluidRow(column(9, plotOutput(ns("taxaplot3"), width = "100%", height = 400))),
                 fluidRow(column(9, plotOutput(ns("taxaplot4"), width = "100%", height = 400))),
                 fluidRow(column(9, plotOutput(ns("taxaplot5"), width = "100%", height = 400))),
                 fluidRow(column(9, plotOutput(ns("taxaplot6"), width = "100%", height = 400))),
                 fluidRow(column(9, plotOutput(ns("taxaplot7"), width = "100%", height = 400))),
                 fluidRow(column(9, plotOutput(ns("taxaplot8"), width = "100%", height = 400))),
                 fluidRow(column(9, plotOutput(ns("taxaplot9"), width = "100%", height = 400))),
                 fluidRow(column(9, plotOutput(ns("taxaplot10"), width = "100%", height = 400))),
                 fluidRow(column(9, plotOutput(ns("taxaplot11"), width = "100%", height = 400))),
                 fluidRow(column(9, plotOutput(ns("taxaplot12"), width = "100%", height = 400))),
                 fluidRow(column(9, plotOutput(ns("taxaplot13"), width = "100%", height = 400))),
                 fluidRow(column(4, "Other (Select Taxa):",
                          selectInput(ns("taxa14"), "Taxa:",
                                      choices = df.phyto.wach %>%  .$Taxa %>%  unique() %>% sort(decreasing = FALSE),
                                      width = '200',
                                      selected = NULL)
                          ), # End Col
                          column(4,br(),
                          selectInput(ns("taxa14year"), "Year:",
                                      choices = df.phyto.wach %>%  .$Year %>%  unique() %>% sort(decreasing = TRUE),
                                      width = '200',
                                      selected = 1)
                          ) # End Col
                 ), # End fr
                 fluidRow(column(9, plotOutput(ns("taxaplot14"), width = "100%", height = 400)))
        ), # End Tab Panel Sub
        tabPanel("Historical Comparison Plots",
                 fluidRow(column(3, # Sites
                                 selectInput(ns("histtaxa"), "Taxa:",
                                             choices = df.phyto.wach %>%  .$Taxa %>%  unique() %>% sort(decreasing = FALSE),
                                             width = '200',
                                             selected = 1),
                                 selectInput(ns("histlocs"), "Station(s):",
                                             choices = df.phyto.wach %>% .$Station %>% levels() %>% paste(),
                                             multiple = TRUE,
                                             width = '200',
                                             selected = c("BN3417","CI3409")),
                                 selectInput(ns("histyear"), " Comparison Year:",
                                             choices = df.phyto.wach %>%  .$Year %>%  unique() %>% sort(decreasing = TRUE),
                                             width = '200',
                                             selected = 1),
                                 radioButtons(ns("stat"), "Stat for Comparison Year:",
                                              choices= c("Minimum" = "min_val", "Average" = "ave_val", "Maximum" = "max_val"),
                                              selected = "ave_val")
                        ), # End Col
                        column(5,
                               sliderInput(ns("yg1"), "Year Grouping 1 (Min-Max):",
                                           min = min(df.phyto.wach$Year), max = max(df.phyto.wach$Year), value = c(2012,2016), step = 1, sep = ""),
                               sliderInput(ns("yg2"), "Year Grouping 2 (Min-Max):",
                                           min = min(df.phyto.wach$Year), max = max(df.phyto.wach$Year), value = c(2007,2016), step = 1, sep = ""),
                               sliderInput(ns("yg3"), "Year Grouping 3 (Min-Max):",
                                           min = min(df.phyto.wach$Year), max = max(df.phyto.wach$Year), value = c(1988,2016), step = 1, sep = "")
                        ), # End Col
                        column(3,
                               radioButtons(ns("stat1"), "Stat for Year Group 1:",
                                            choices= c("Minimum" = "min_val", "Average" = "ave_val", "Maximum" = "max_val"),
                                            selected = "ave_val"),
                               radioButtons(ns("stat2"), "Stat for Year Group 2:",
                                            choices= c("Minimum" = "min_val", "Average" = "ave_val", "Maximum" = "max_val"),
                                            selected = "ave_val"),
                               radioButtons(ns("stat3"), "Stat for Year Group 3:",
                                            choices= c("Minimum" = "min_val", "Average" = "ave_val", "Maximum" = "max_val"),
                                            selected = "ave_val")
                        ) #End Col
                 ), # End fr
                 fluidRow(column(9, plotOutput(ns("histplot"), width = "100%", height = 600)))
        ), # End Tab Panel Sub
        tabPanel("Filter/Export Data",
                 fluidRow(column(4, # Sites
                                 selectInput(ns("filtersite"), "Station(s):",
                                             choices = df.phyto.wach %>% .$Station %>% levels() %>% paste(),
                                             multiple = TRUE,
                                             width = '200',
                                             selected = c("BN3417","CI3409"))
                        ) # End Col
                 ) # End fr
        ) # End Tab Panel Sub
    ) # End Tabset Panel
  ) # end taglist
} # end UI

##############################################################################################################################
# Server Function
##############################################################################################################################

Phyto <- function(input, output, session, df) {



  #   ps <- reactive({
  #
  #   # Save Options
  #     # Size dependent? Change size for saving?
  #     p <- p + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))
  #
  #     # Gridlines for saving options
  #     if("major gridlines" %in% input$plot.save.grid){
  #       p <- p + theme(panel.grid.major = element_line())
  #     }
  #     if("minor gridlines" %in% input$plot.save.grid){
  #       p <- p + theme(panel.grid.minor = element_line())
  #     }
  #   p
  # })
  # FN <-  paste0("Phytoplankton-", input$year,"_", format(Sys.time(), "%Y%m%d"),".", input$plot.save.type)
     output$PhytoPlot <- renderPlot({
       p <- phytoplot(df = df,
                      locs = input$site,
                      vyear = input$year,
                      epi_min = input$Depth1[1],
                      epi_max = input$Depth1[2],
                      em_min = input$Depth2[1],
                      em_max = input$Depth2[2])

       p <- p + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))

       # # Gridlines for saving options
       # if("major gridlines" %in% input$plot.save.grid){
       #   p <- p + theme(panel.grid.major = element_line())
       # }
       # if("minor gridlines" %in% input$plot.save.grid){
       #   p <- p + theme(panel.grid.minor = element_line())
       # }
       p
       #ggsave(FN, p, device = input$plot.save.type)
    })

     # Total_Diatoms 128 dodgerblue
     output$taxaplot1 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Total_Diatoms", color = "dodgerblue")
       p
     })
     # Asterionella 430 lightskyblue
     output$taxaplot2 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Asterionella", color = "lightskyblue")
       p
       })
     # Cyclotella 441 lightsteelblue3
     output$taxaplot3 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Cyclotella", color = "lightsteelblue3")
       p
     })
     # Total_Chlorophytes 472 mediumseagreen
     output$taxaplot4 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Total_Chlorophytes", color = "mediumseagreen")
       p
     })
     # Total_Chrysophytes 144 gold2
     output$taxaplot5 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Total_Chrysophytes", color = "gold2")
       p
     })
     # Chrysosphaerella 78 darkgoldenrod3
     output$taxaplot6 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Chrysosphaerella", color = "darkgoldenrod3")
       p
     })
     # Dinobryon 145 gold3
     output$taxaplot7 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Dinobryon", color = "gold3")
       p
     })
     # Synura 149 goldenrod2
     output$taxaplot8 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Synura", color = "goldenrod2")
       p
     })
     # Uroglenopsis 573 sandybrown
     output$taxaplot9 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Uroglenopsis", color = "sandybrown")
       p
     })
     # Total_Cyanophytes 475 mediumturquoise
     output$taxaplot10 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Total_Cyanophytes", color = "mediumturquoise")
       p
     })
     # Anabaena 518 palegreen4
     output$taxaplot11 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Anabaena", color = "palegreen4")
       p
     })
     # Microcystis 523 paleturquoise4
     output$taxaplot12 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Microcystis", color = "paleturquoise4")
       p
     })
     # Grand_Total_Algae 24 black
     output$taxaplot13 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxayear, taxa = "Grand_Total_Algae", color = "black")
       p
     })
     # Other (Select dropdown) 604 slategrey
     output$taxaplot14 <- renderPlot({
       p <- taxaplot(df = df,locs = input$taxasite, vyear = input$taxa14year, taxa = input$taxa14, color = "slategrey")
       p
     })
    observe({print(input$yg1[1])})

      output$histplot <- renderPlot({
       p <- historicplot(df = df,
                      taxa = input$histtaxa,
                      locs = input$histlocs,
                      vyear= input$histyear,
                      yg1min = input$yg1[1],
                      yg1max = input$yg1[2],
                      yg2min = input$yg2[1],
                      yg2max = input$yg2[2],
                      yg3min = input$yg3[1],
                      yg3max = input$yg3[2],
                      stat = input$stat,
                      stat1= input$stat1,
                      stat2= input$stat2,
                      stat3= input$stat3)
       p
     })

    # output$save.plot <- downloadHandler(
    #   filename <- function() {FN},
    #   content = function(file) {
    #     file.copy(FN, file, overwrite=TRUE)
    #   }
    # )
  } # End Phyto Server function


