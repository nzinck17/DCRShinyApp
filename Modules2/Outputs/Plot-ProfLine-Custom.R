##############################################################################################################################
#     Title: Plot-ProfLine-Custom.R
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
#   3. 1 to None (in shape and color )

##############################################################################################################################
# User Interface
##############################################################################################################################

plot.profline.custom.UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    
    uiOutput(ns("plot.ui")),
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
                               checkboxInput(ns("plot.display.int"), "Interactive Plot:")
                        ), # end column
                        column(2,
                               sliderInput(ns("plot.display.lsize"), "Line Thickness:",
                                           min = 0.5, max = 4, value = 1.5, step = 0.5)
                        ) # end column
               ), # end Tab Panel
               tabPanel("Trends and Lines", br(), br(),
                        column(2,
                               strong("Secchi Depth:"),
                               checkboxInput(ns("plot.line.sec"),"Show Line"),
                               radioButtons(ns("plot.line.sec.type"), "Line Type",
                                            choices = c("solid", "dash", "dotted")),
                               sliderInput(ns("plot.line.sec.size"), "Line Thickness:",
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
                        column(2,
                               radioButtons(ns("plot.color"), label = "Group with Colors:", 
                                            choices = c("Parameter", "Site"),
                                            selected = "Parameter")

                        ), # end column
                        # new column
                        column(2,
                               radioButtons(ns("plot.linetype"), label = "Group with Linetypes:", 
                                            choices = c("Parameter", "Site"),
                                            selected = "Site")
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

plot.profline.custom <- function(input, output, session, df) {
  
  
### Text For Plot
  
  # Site Text
  text.site <- reactive({
    df() %>% .$Site %>% factor() %>% levels() %>% paste()
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
    p <- ggplot(df(),aes(x=Result,y=Depthm)) +
      scale_y_reverse()
      

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
    
# Grouping and Faceting
    
    # No Parameter Coloring/linetype --> facet by param
    if(input$plot.color != "Parameter" & input$plot.linetype != "Parameter"){
      p <- p + geom_path(aes_string(color = input$plot.color, linetype = input$plot.linetype), size = input$plot.display.lsize) +
        facet_grid(Parameter~Date)
      # No Site Coloring/linetype --> facet by site
    } else if(input$plot.color != "Site" & input$plot.linetype != "Site"){
      p <- p + geom_path(aes_string(color = input$plot.color, linetype = input$plot.linetype), size = input$plot.display.lsize) +
        facet_grid(Site~Date)
      # Param and Site both colored or linetyped
    } else {
      p <- p + geom_path(aes_string(color = input$plot.color, linetype = input$plot.linetype), size = input$plot.display.lsize) +
        facet_grid(.~Date)
    }

# Add Lines
    
    # Show Secchi
    if(input$plot.line.sec == TRUE){
      p <- p + geom_hline(yintercept = 2, 
                          linetype = input$plot.line.sec.type,
                          size = input$plot.line.sec.size)
    }


# Title and Axis Lables
    
    # Title
    if(input$plot.title == "None"){
      p <- p + ggtitle("")
    }
    if(input$plot.title == "Auto"){
      p <- p + ggtitle(paste(text.param(), "at", 
                             text.site(), 
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
      p <- p + ylab("Depth (m)")
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
  
  
  # Plot Visualization - Non Interactive
  
  output$plot <- renderPlot(
    p()
  )
  
  
  # Plot Interactive
  
  output$plot.int <- renderPlotly({
    ggplotly(p() + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in")))
  })
  
  # Plot Output
  
  output$plot.ui <- renderUI({
    ns <- session$ns # see General Note 1
    
    if(input$plot.display.int == FALSE){
      plotOutput(ns("plot"), width = "100%", height = 500)
    } else {
      plotlyOutput(ns("plot.int"), width = "100%", height = 500)
    }
  })
  
  # Plot Print
  
  output$save.plot <- downloadHandler(
    filename = function (){paste(text.param(),' Site ', text.site(),' from ', text.date.start(),' to ', text.date.end(), '.png', sep='')},
    content = function(file) {ggsave(file, plot = p(), device = "png")},
    contentType = 'image/png'
  )

  
} # end Server Function

