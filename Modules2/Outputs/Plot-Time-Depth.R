##############################################################################################################################
#     Title: Plot-Time-Depth.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Time Series plot (for non-depth dependent data)
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:
#   1. Make the Metero/Hydro Filters work
#   2. Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)

##############################################################################################################################
# User Interface
##############################################################################################################################

plot.time.depth.UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    
    plotlyOutput(ns("plot"), width = "100%", height = 600),
    # Plot Options
    fluidRow(br(), br()),
    fluidRow(
      column(11,
             tabsetPanel(
               tabPanel("Display Options", br(), br(),
                        column(2,
                               radioButtons(ns("plot.display.theme"), "Theme:", 
                                            choices= c("Gray", 
                                                       "Black and White",  
                                                       "Line Draw", 
                                                       "Light", 
                                                       "Dark", 
                                                       "Minimal", 
                                                       "Classic"))
                        ), # end column
                        column(2,
                               checkboxGroupInput(ns("plot.display.log"), "Misc. Display :", 
                                                  choices= c("Log-scale Y Axis"))
                        ), # end column
                        column(2,
                               sliderInput(ns("plot.display.psize"), "Point Size:",
                                           min = 0.5, max = 4, value = 1.5, step = 0.5)
                        ) # end column
               ), # end Tab Panel
               tabPanel("Trends and Lines", br(), br(),
                        column(2,
                               radioButtons(ns("plot.line.trend"), "Add Trendline:", 
                                            choices= c("None",
                                                       "Linear" = "lm",
                                                       "Curve" = "loess")),
                               checkboxInput(ns("plot.line.trend.ribbon"), "Show Conf.Ribbon"),
                               sliderInput(ns("plot.line.trend.size"), "Line Thickness:",
                                           min = 0.5, max = 3, value = 1, step = 0.5)
                        ), # end column
                        column(1),
                        column(2,
                               strong("Non-Detection Level:"),
                               checkboxInput(ns("plot.line.nd"),"Show Line"),
                               radioButtons(ns("plot.line.nd.type"), "Line Type",
                                            choices = c("solid", "dash", "dotted")),
                               sliderInput(ns("plot.line.nd.size"), "Line Thickness:",
                                           min = 0.5, max = 3, value = 1, step = 0.5)
                        ), # end column
                        column(2,
                               strong("Reporting Limit:"),
                               checkboxInput(ns("plot.line.rl"), "Show Line"),
                               radioButtons(ns("plot.line.rl.type"), "Line Type",
                                            choices = c("solid", "dash", "dotted")),
                               sliderInput(ns("plot.line.rl.size"), "Line Thickness:",
                                           min = 0.5, max = 3, value = 1, step = 0.5)
                        ), # end column
                        column(2,
                               strong("Performace Standard:"),
                               checkboxInput(ns("plot.line.ps"), "Show Line"),
                               radioButtons(ns("plot.line.ps.type"), "Line Type",
                                            choices = c("solid", "dash", "dotted")),
                               sliderInput(ns("plot.line.ps.size"), "Line Thickness:",
                                           min = 0.5, max = 3, value = 1, step = 0.5)
                        ) # end column
               ),
               tabPanel("Title and Axis Labels", br(), br(),
                        column(3,
                               radioButtons(ns("plot.title"), "Title Options:", 
                                            choices= c("None", "Auto", "Custom"),
                                            selected = "Auto"),
                               textInput(ns("plot.title.text"), "")
                        ), # end column
                        column(3,
                               radioButtons(ns("plot.xlab"), "X Label Options:", 
                                            choices= c("None", "Auto", "Custom"),
                                            selected = "Auto"),
                               textInput(ns("plot.xlab.text"), "")
                        ), # end column
                        column(3,
                               radioButtons(ns("plot.ylab"), "Y Label Options:", 
                                            choices= c("None", "Auto", "Custom"),
                                            selected = "Auto"),
                               textInput(ns("plot.ylab.text"), "")
                        ) # end column
               ), # end Tab Panel
               tabPanel("Grouping (Color/Shape)", br(), br(),
                        column(3,
                               radioButtons(ns("plot.color"), label = "Group with Colors:", 
                                            choices = c("None" = 1, 
                                                        "Station" = "Station",
                                                        "Sampling Level" = "Sampling_Level",
                                                        "met/hydro filter 1 (select group)" = "met1",
                                                        "met/hydro filter 2 (select group)" = "met2",
                                                        "Flagged data" = "FlagCode"),
                                            selected = "Station")
                        ), # end column
                        # new column
                        column(3,
                               radioButtons(ns("plot.shape"), label = "Group with Shapes:", 
                                            choices = c("None" = 1, 
                                                        "Station" = "Station",
                                                        "Sampling Level" = "Sampling_Level",
                                                        "met/hydro filter 1 (select group)" = "met1",
                                                        "met/hydro filter 2 (select group)" = "met2",
                                                        "Flagged data" = "FlagCode"),
                                            selected = 1)
                        ) # end column
               ), # tabPanel
               tabPanel("Save Plot", br(), br(),
                        column(2,
                               downloadButton(ns('save.plot'), "Save Plot")
                        ),
                        column(2,
                               radioButtons(ns("plot.save.size"), "Plot Size:", 
                                            choices= c("small",
                                                       "medium",
                                                       "large"))
                        ),
                        column(2,
                               radioButtons(ns("plot.save.type"), "File Type:", 
                                            choices= c("pdf",
                                                       "jpg",
                                                       "png"))
                        ),
                        column(2,
                               checkboxGroupInput(ns("plot.save.grid"), "Gridline Override:", 
                                                  choices= c("major gridlines",
                                                             "minor gridlines"))
                        ) # end column
               ) # tab panel
             ) # end tabSet Panel
      ), # end Column
      # extend the page with a right column of blank rows (hack at keeping the page the same height no matter the tab open)
      column(1, br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
             br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
      )
    ) # Fluid Row
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

plot.time.depth <- function(input, output, session, df) {
  
  
### Text For Plot
  
  # Station Text
  text.station <- reactive({
    df() %>% .$Station %>% factor() %>% levels() %>% paste()
  })
  
  # Depth Text
  text.depth <- reactive({
    df() %>% .$Depthm %>% factor() %>% levels() %>% paste()
  })
  
  # Param Text
  text.param <- reactive({
    df() %>% .$Parameter %>% factor() %>% levels() %>% paste()
  })
  
  # Units Text
  text.units <- reactive({
    df() %>% .$Units %>% factor() %>% levels() %>% paste()
  })
  
  # Date Text - Start
  text.date.start <- reactive({
    df() %>% .$Date %>% min(na.rm = TRUE) %>% paste()
  })
  
  # Date Text - End
  text.date.end <- reactive({
    df() %>% .$Date %>% max(na.rm = TRUE) %>% paste()
  })
  


### PLOT
  
  # Plot Creation
  
  p <- reactive({
    
    # Features in which all plot options have in common
    p <- ggplot(df(), aes(x = Date, y = Result))
      

# Display Tab
    
    # Theme based on selection
    if(input$plot.display.theme == "Gray"){
      p <- p + theme_gray()
    }
    if(input$plot.display.theme == "Black and White"){
      p <- p + theme_bw()
    }
    if(input$plot.display.theme == "Line Draw"){
      p <- p + theme_linedraw()
    }
    if(input$plot.display.theme == "Light"){
      p <- p + theme_light()
    }
    if(input$plot.display.theme == "Dark"){
      p <- p + theme_dark()
    }
    if(input$plot.display.theme == "Minimal"){
      p <- p + theme_minimal()
    }
    if(input$plot.display.theme == "Classic"){
      p <- p + theme_classic()
    }

    # Log Scale
    if("Log-scale Y Axis" %in% input$plot.display.log){
      p <- p + scale_y_log10()
    }
    
# Grouping and Trendline
    
    # Group by both Color and Shape when both selected
    if(input$plot.color != 1 & input$plot.shape != 1){
      p <- p + geom_point(aes_string(color = input$plot.color, shape = input$plot.shape), size = input$plot.display.psize)
      if(input$plot.line.trend != "None"){
        p <- p + geom_smooth(method = input$plot.line.trend, 
                             size = input$plot.line.trend.size,
                             se = input$plot.line.trend.ribbon,
                             aes_string(color = input$plot.color, linetype = input$plot.shape))
      }
    }
    # Group by only Color when only color grouping is selected
    else if (input$plot.color != 1){
      p <- p + geom_point(aes_string(color = input$plot.color), size = input$plot.display.psize)
      if(input$plot.line.trend != "None"){
        p <- p + geom_smooth(method = input$plot.line.trend, 
                             size = input$plot.line.trend.size,
                             se = input$plot.line.trend.ribbon,
                             aes_string(color = input$plot.color))
      }
    } 
    # Group by only Shape when only shape grouping is selected 
    else if (input$plot.shape != 1){
      p <- p + geom_point(aes_string(shape = input$plot.shape), size = input$plot.display.psize)
      if(input$plot.line.trend != "None"){
        p <- p + geom_smooth(method = input$plot.line.trend, 
                             size = input$plot.line.trend.size,
                             se = input$plot.line.trend.ribbon,
                             aes_string(linetype = input$plot.shape))
      }
    } 
    # No Grouping Selected
    else {
      p <- p + geom_point(size = input$plot.display.psize)
      if(input$plot.line.trend != "None"){
        p <- p + geom_smooth(method = input$plot.line.trend, 
                             size = input$plot.line.trend.size,
                             se = input$plot.line.trend.ribbon)
      }
    }
    
    # Facet for Sites if no grouping for site is selected and number of sites is greater than 1
    if(input$plot.color != "Station" & input$plot.shape != "Station" & length(c(input$station)) > 1){
      if(input$plot.color != "SamplingLevel" & input$plot.shape != "SamplingLevel" & length(c(input$level)) > 1){
        p <- p + facet_grid(Station~SamplingLevel)
      } else {
        p <- p + facet_grid(Station~.)
      }
    } else {
      if(input$plot.color != "SamplingLevel" & input$plot.shape != "SamplingLevel" & length(c(input$level)) > 1){
        p <- p + facet_grid(.~SamplingLevel)
      }
    }

# Add Lines
    
    # Show Non-Detect Level
    if(input$plot.line.nd == TRUE){
      p <- p + geom_hline(yintercept = 2, 
                          linetype = input$plot.line.nd.type,
                          size = input$plot.line.nd.size)
    }
    
    # Show Reprting Limit
    if(input$plot.line.rl == TRUE){
      p <- p + geom_hline(yintercept = 3, 
                          linetype = input$plot.line.rl.type,
                          size = input$plot.line.rl.size)
    }
    
    # Performance Standard
    if(input$plot.line.ps == TRUE){
      p <- p + geom_hline(yintercept = 4, 
                          linetype = input$plot.line.ps.type,
                          size = input$plot.line.ps.size)
    }


# Title and Axis Lables
    
    # Title
    if(input$plot.title == "None"){
      p <- p + ggtitle("")
    }
    if(input$plot.title == "Auto"){
      p <- p + ggtitle(paste(text.param(), "at", 
                             text.station(), text.depth(),
                             "from", text.date.start(), "to", text.date.end(), sep= " "))
    }
    if(input$plot.title == "Custom"){
      p <- p + ggtitle(input$plot.title.text)
    }
    
    # X Axis
    if(input$plot.xlab == "None"){
      p <- p + xlab("")
    }
    if(input$plot.xlab == "Auto"){
      p <- p + xlab("Date")
    }
    if(input$plot.xlab == "Custom"){
      p <- p + xlab(input$plot.xlab.text)
    }
    
    # Y Axis
    if(input$plot.ylab == "None"){
      p <- p + ylab("")
    }
    if(input$plot.ylab == "Auto"){
      p <- p + ylab(paste(text.param(), " (", text.units(),")", sep= ""))
    }
    if(input$plot.ylab == "Custom"){
      p <- p + ylab(input$plot.ylab.text)
    }
    
# Save Options
    
    # Size dependent? Change size for saving?
    p <- p + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))
    
    # Gridlines for saving options
    if("major gridlines" %in% input$plot.save.grid){
      p <- p + theme(panel.grid.major = element_line())
    }
    if("minor gridlines" %in% input$plot.save.grid){
      p <- p + theme(panel.grid.minor = element_line())
    }
    
    p
    
  })
  
  
# Plot Visualization - convert plot to interactive plot and create an plot output object
  
  output$plot <- renderPlotly({
    ggplotly(p())
  })
  
  
  # Plot Print
  
  output$save.plot <- downloadHandler(
    filename = function (){paste(text.param(),' Site ', text.station(), text.depth(), ' from ', text.date.start(),' to ', text.date.end(), '.png', sep='')},
    content = function(file) {ggsave(file, plot = p(), device = "png")},
    contentType = 'image/png'
  )

  
} # end Server Function

