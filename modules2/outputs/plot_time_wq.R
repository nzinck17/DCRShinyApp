##############################################################################################################################
#     Title: plot_time_wq.R
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

plot.time.UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    conditionalPanel(
      condition = "input.param2 == 'None'", ns = ns,
      plotlyOutput(ns("plot1"), width = "100%", height = 600)
      ),
    conditionalPanel(
      condition = "input.param2 != 'None'", ns = ns,
      plotOutput(ns("plot2"), width = "100%", height = 600),
      h4(textOutput(ns("plot2text")))
    ),
    # Plot Options
    fluidRow(br(), br()),
    fluidRow(
      column(11,
             tabsetPanel(
               tabPanel("Display Options", br(), br(),
                        column(2,
                               uiOutput(ns("param1.ui"))
                        ), # end column
                        column(2,
                               uiOutput(ns("param2.ui"))
                        ), # end column
                        column(2,
                               checkboxGroupInput(ns("plot.display.axis"), "Axis Options:",
                                                  choices= c("Log-scale Y-Axis",
                                                             "Y-axis start at zero"))
                        ), # end column
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
                               sliderInput(ns("plot.display.psize"), "Point Size:",
                                           min = 0.5, max = 4, value = 1.5, step = 0.5)
                        ) # end column
               ), # end Tab Panel
               tabPanel("Trends and Lines", br(), br(),
                        column(3,
                               radioButtons(ns("plot.line.trend"), "Add Simple Trendline:",
                                            choices= c("None",
                                                       "Linear" = "lm",
                                                       "Curve" = "loess")),
                               strong("Confidence Ribbon"),
                               checkboxInput(ns("plot.line.trend.ribbon"), "Show Conf. Ribbon"),
                               radioButtons(ns("plot.line.trend.conf"), NULL,
                                            choices= c(0.90,0.95,0.99), inline = TRUE),
                               sliderInput(ns("plot.line.trend.size"), "Line Thickness:",
                                           min = 0, max = 3, value = 1, step = 0.25)
                        ), # end column
                        column(3,
                               strong("Non-Detection Level:"),
                               checkboxInput(ns("plot.line.nd"),"Show Line"),
                               radioButtons(ns("plot.line.nd.type"), "Line Type",
                                            choices = c("solid", "dash", "dotted")),
                               sliderInput(ns("plot.line.nd.size"), "Line Thickness:",
                                           min = 0, max = 3, value = 1, step = 0.25)
                        ), # end column
                        column(3,
                               strong("Reporting Limit:"),
                               checkboxInput(ns("plot.line.rl"), "Show Line"),
                               radioButtons(ns("plot.line.rl.type"), "Line Type",
                                            choices = c("solid", "dash", "dotted")),
                               sliderInput(ns("plot.line.rl.size"), "Line Thickness:",
                                           min = 0, max = 3, value = 1, step = 0.25)
                        ), # end column
                        column(3,
                               strong("Performace Standard:"),
                               checkboxInput(ns("plot.line.ps"), "Show Line"),
                               radioButtons(ns("plot.line.ps.type"), "Line Type",
                                            choices = c("solid", "dash", "dotted")),
                               sliderInput(ns("plot.line.ps.size"), "Line Thickness:",
                                           min = 0, max = 3, value = 1, step = 0.25)
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
                                            choices = c("None" = "None",
                                                        "Site" = "Site",
                                                        "Flagged data" = "FlagCode"),
                                            selected = "Site")
                        ), # end column
                        # new column
                        column(3,
                               radioButtons(ns("plot.shape"), label = "Group with Shapes:",
                                            choices = c("None" = "None",
                                                        "Site" = "Site",
                                                        "Flagged data" = "FlagCode"),
                                            selected = "None")
                        ) # end column
               ), # tabPanel
               tabPanel("Save Plot", br(), br(),
                        column(2,
                               downloadButton(ns('save.plot'), "Save Plot")
                        ),
                        column(3,
                               numericInput(ns("plot.save.width"), "Plot Width (inches):", 7,
                                            min = 3, max = 17, step = 0.25),
                               
                               numericInput(ns("plot.save.height"), "Plot Height (inches):", 5,
                                            min = 3, max = 17, step = 0.25)

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

# Note that Argument "df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

plot.time <- function(input, output, session, df) {

  ns <- session$ns # see General Note 1
  
  
  ### Before Plot 
  
  # Find Number of Sites and Param (could make it so plot.time module server function accepts this instead (probably faster)
  
  site <- reactive({df() %>% .$Site %>% factor() %>% levels()})
  param <- reactive({df() %>% .$Parameter %>% factor() %>% levels()})

  

  ### Parameter Axis Choice
  
  output$param1.ui <- renderUI({
    radioButtons(ns("param1"), "Primary Y-Axis Parameter",
                 choices = param())
  })
  
  output$param2.ui <- renderUI({
    radioButtons(ns("param2"), "Secondary Y-Axis Parameter",
                 choices = c(param(), "None"),
                 selected = "None")
  })
  
  ### Two Dataframes (Primary and Secondary Axis)
  
  df1 <- reactive({
    req(input$param1, df())
    df() %>% filter(Parameter %in% input$param1)})
  
  df2 <- reactive({
    req(input$param2 != "None")
    df() %>% filter(Parameter %in% input$param2)
  })
  
  
  # Texts For Plot
  
  text.site <- reactive({site() %>% paste(collapse = ", ")})
  
  text.param1 <- reactive({df1()$Parameter %>% factor() %>% levels() %>% paste()})
  
  text.param2 <- reactive({df2()$Parameter %>% factor() %>% levels() %>% paste()})
  
  text.units1 <- reactive({df1()$Units %>% factor() %>% levels() %>% paste(collapse = ", ")})
  
  text.units2 <- reactive({df2()$Units %>% factor() %>% levels() %>% paste(collapse = ", ")})
  
  text.date.start1 <- reactive({df1()$Date %>% min(na.rm = TRUE) %>% paste()})
  
  text.date.end1 <- reactive({df1()$Date %>% max(na.rm = TRUE) %>% paste()})
  
  text.date.start2 <- reactive({c(df1()$Date, df2()$Date)  %>% min(na.rm = TRUE) %>% paste()})
  
  text.date.end2 <- reactive({c(df1()$Date, df2()$Date) %>% max(na.rm = TRUE) %>% paste()})
  
  plot.name <- reactive({
    if(input$param2 == "None"){
      paste0(text.param1(),' at Site(s)', text.site(),
             ' from ', text.date.start1(),' to ', text.date.end1())
    } else{
      paste0(text.param1(), ' and ', text.param2(), ' at Site(s)', text.site(),
             ' from ', text.date.start2(),' to ', text.date.end2())
    }
  })

  
  
  
  ### PLOT
  
  # Plot Creation
  
  p <- reactive({
    
    #
    
    # Features in which all plot options have in common
    p <- ggplot() +
      scale_x_datetime(breaks = pretty_breaks(n=12))
  
    
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
  
    
    # Grouping and Trendline
    
    # Group by both Color and Shape when both selected
    if(input$plot.color != "None" & input$plot.shape != "None"){
      if(input$param2 == "None"){
        p <- p + geom_point(data = df1(), 
                            aes_string(x = "as.POSIXct(Date)", y = "Result", color = input$plot.color, shape = input$plot.shape),
                            size = input$plot.display.psize)
      } else {
        p <- p + geom_point(data = df1(), 
                            aes_string(x = "as.POSIXct(Date)", y = "Result", color = input$plot.color),
                            shape = 16, size = input$plot.display.psize) +
          geom_point(data = df2(),
                     aes_string(x = "as.POSIXct(Date)", y = "Result*mult", color = input$plot.color), 
                     shape = 17, size = input$plot.display.psize)
      }
      if(input$plot.line.trend != "None"){
        p <- p + geom_smooth(data = df1(), aes_string(x = "as.POSIXct(Date)", y = "Result", 
                                                      color = input$plot.color, linetype = input$plot.shape),
                             method = input$plot.line.trend,
                             size = input$plot.line.trend.size,
                             se = input$plot.line.trend.ribbon,
                             level = as.numeric(input$plot.line.trend.conf))
      }
    }
    # Group by only Color when only color grouping is selected
    else if (input$plot.color != "None"){
      if(input$param2 == "None"){
        p <- p + geom_point(data = df1(), 
                            aes_string(x = "as.POSIXct(Date)", y = "Result", color = input$plot.color), 
                            size = input$plot.display.psize)
      }else{
        p <- p + geom_point(data = df1(), 
                            aes_string(x = "as.POSIXct(Date)", y = "Result", color = input$plot.color), 
                            shape = 16, size = input$plot.display.psize) +
          geom_point(data = df2(), 
                     aes_string(x = "as.POSIXct(Date)", y = "Result*mult", color = input$plot.color), 
                     shape = 17, size = input$plot.display.psize)
      }
      if(input$plot.line.trend != "None"){
        p <- p + geom_smooth(data = df1(), aes_string(x = "as.POSIXct(Date)", y = "Result", color = input$plot.color),
                             method = input$plot.line.trend,
                             size = input$plot.line.trend.size,
                             se = input$plot.line.trend.ribbon,
                             level = as.numeric(input$plot.line.trend.conf))
      }
    }
    # Group by only Shape when only shape grouping is selected
    else if (input$plot.shape != "None"){
      if(input$param2 == "None"){
      p <- p + geom_point(data = df1(), 
                          aes_string(x = "as.POSIXct(Date)", y = "Result", shape = input$plot.shape), 
                          size = input$plot.display.psize)
      }else{
        p <- p + geom_point(data = df1(), 
                            aes_string(x = "as.POSIXct(Date)", y = "Result"), 
                            shape = 16, size = input$plot.display.psize) +
        geom_point(data = df2(),
                   aes_string(x = "as.POSIXct(Date)", y = "Result*mult"), 
                   shape = 17, size = input$plot.display.psize)
      }
      if(input$plot.line.trend != "None"){
        p <- p + geom_smooth(data = df1(), aes_string(x = "as.POSIXct(Date)", y = "Result", linetype = input$plot.shape),
                             method = input$plot.line.trend,
                             size = input$plot.line.trend.size,
                             se = input$plot.line.trend.ribbon,
                             level = as.numeric(input$plot.line.trend.conf))
      }
    }
    # No Grouping Selected
    else {
      if(input$param2 == "None"){
        p <- p + geom_point(data = df1(), aes_string(x = "as.POSIXct(Date)", y = "Result"), size = input$plot.display.psize)
      }else{
        p <- p + geom_point(data = df1(), aes_string(x = "as.POSIXct(Date)", y = "Result"), 
                            shape = 16, size = input$plot.display.psize) +
          geom_point(data = df2(), aes_string(x = "as.POSIXct(Date)", y = "Result*mult"), 
                     shape = 17, size = input$plot.display.psize)        
      }
      if(input$plot.line.trend != "None"){
        p <- p + geom_smooth(data = df1(), aes_string(x = "as.POSIXct(Date)", y = "Result"),
                             method = input$plot.line.trend,
                             size = input$plot.line.trend.size,
                             se = input$plot.line.trend.ribbon,
                             level = as.numeric(input$plot.line.trend.conf))
      }
    }
    
    # Facet for Sites if no grouping for site is selected and number of sites is greater than 1
    if(input$plot.color != "Site" & (input$plot.shape != "Site" | input$param2 != "None") & length(c(site())) > 1){
      p <- p + facet_wrap(~Site, ncol = ceiling(length(c(site()))/4))
    }

    
    # Add Lines
    
    # Show Non-Detect Level
    if(input$plot.line.nd == TRUE){
      p <- p + geom_hline(data = df1(), aes_string(x = "as.POSIXct(Date)", y = "Result"),
                          yintercept = 2,
                          linetype = input$plot.line.nd.type,
                          size = input$plot.line.nd.size)
    }
    
    # Show Reprting Limit
    if(input$plot.line.rl == TRUE){
      p <- p + geom_hline(data = df1(), aes_string(x = "as.POSIXct(Date)", y = "Result"),
                          yintercept = 3,
                          linetype = input$plot.line.rl.type,
                          size = input$plot.line.rl.size)
    }
    
    # Performance Standard
    if(input$plot.line.ps == TRUE){
      p <- p + geom_hline(data = df1(), aes_string(x = "as.POSIXct(Date)", y = "Result"),
                          yintercept = 4,
                          linetype = input$plot.line.ps.type,
                          size = input$plot.line.ps.size)
    }
    
    
    # Title and Axis Lables
    
    # Title
    if(input$plot.title == "None"){
      p <- p + ggtitle("")
    }
    if(input$plot.title == "Auto"){
      p <- p + ggtitle(plot.name())
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
      p <- p + ylab(paste(text.param1(), " (", text.units1(),")", sep= ""))
    }
    if(input$plot.ylab == "Custom"){
      p <- p + ylab(input$plot.ylab.text)
    }
    
    
    # Log Scale
    if(input$param2 == "None"){
      if("Log-scale Y-Axis" %in% input$plot.display.axis){
        p <- p + scale_y_log10()
      } else {
        if("Y-axis start at zero" %in% input$plot.display.axis){
          ymax <- max(df1()$Result, na.rm = TRUE)
          p <- p + scale_y_continuous(limits = c(0,ymax))
        } else {
          p <- p + scale_y_continuous()
        }
      }
    }
    
    # If Secondary Axis parameter is chosen
    
    if(input$param2 != "None"){
      
      # Scalars for Secondary Y-axis
      y1min <- min(df1()$Result, na.rm = TRUE)
      y2min <- min(df2()$Result, na.rm = TRUE)
      y1max <- max(df1()$Result, na.rm = TRUE)
      y2max <- max(df2()$Result, na.rm = TRUE)
      
      
      # Two Y-axis Log Scale adn non-Lod Scale
      if("Log-scale Y-Axis" %in% input$plot.display.axis){

        mult <- exp(log(y1max  - y1min) / log(y2max - y2min))
        p <- p + scale_y_log10(sec.axis = sec_axis(~./mult, breaks = trans_breaks('log10', function(x) 10^x),
                                                   name = paste0(text.param2(), " (", text.units2(),")")))
                                 
      } else{
        
        if("Y-axis start at zero" %in% input$plot.display.axis){
          
          mult <- y1max / y2max
          p <- p + scale_y_continuous(breaks = pretty_breaks(),limits = c(0,y1max),
                                      sec.axis = sec_axis(~./mult, breaks = pretty_breaks(), 
                                                          name = paste0(text.param2(), " (", text.units2(),")")))
        } else {
          
          mult <- (y1max  - y1min) / (y2max - y2min)
          p <- p + scale_y_continuous(breaks = pretty_breaks(),
                                      sec.axis = sec_axis(~./mult, breaks = pretty_breaks(), 
                                                          name = paste0(text.param2(), " (", text.units2(),")")))
        }
        
      

      }
      
      p <- p + theme(text = element_text(size = 15))

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
    
  }) # end Plot
  
  
  
  
  
  # Text for legend of double Plot
  output$plot2text <- renderText({
    paste("Circle =", text.param1(), ";   Triangle =", text.param2())
    })
  

  # Plot Visualization - convert plot to interactive plot and create an plot output object

  output$plot1 <- renderPlotly({
    req(input$param2 == "None")
    
    ggplotly(p())
    
  })
  
  output$plot2 <- renderPlot({
    req(input$param2 != "None")

    p()

  })

  # filename
  
  filename <- reactive({
    paste0(plot.name(), ".", input$plot.save.type)
  })
  
  # Plot Print

  output$save.plot <- downloadHandler(
    filename = function(){filename()},
    content = function(file){ggsave(file, plot = p(), 
                                    width = input$plot.save.width,
                                    height = input$plot.save.height,
                                    device = input$plot.save.type)}#,
    #contentType = 'image/png'
  )


} # end Server Function



### Attempts at adding an actual shape legend for 2 parameters

### Method 1

# p2 <- data_frame(Col1 = c("Param1", "Param2"), Col2 = c(1, 2)) %>% ggplot() +
#   geom_point(aes(x = Col2, y = Col2,  shape = Col1))
# 
# p2.leg <- get_legend(p2)
# 
# 
# lay <- rbind(c(1,1,1,1,3),
#              c(1,1,1,1,2),
#              c(1,1,1,1,2),
#              c(1,1,1,1,2),
#              c(1,1,1,1,2),
#              c(1,1,1,1,2),
#              c(1,1,1,1,2))
# 
# p1.plot <- p() + guides(colour=FALSE)
# 
# p1.leg <- get_legend(p())
# 
# p1.plot
# 
# grid.arrange(grobs = list(p1.plot, p2.leg, p1.leg), layout_matrix = lay)

### Method 2

# annotate("text", xleg[2], 1.1*max(c(y1max, y2max)),
#          label = text.param1(), hjust = 0, nudge_x = 0.05) +
# annotate("text", xleg[2], 1.05*max(c(y1max, y2max)),
#          label = text.param2(), hjust = 0, nudge_x = 0.05) +
# annotate("point", xleg[1], 1.1*max(c(y1max, y2max)),
#          shape = 16) +
# annotate("point", xleg[1], 1.05*max(c(y1max, y2max)),
#          shape = 17)
# xleg <- quantile(
#   seq(min(c(as.POSIXct(df1()$Date), as.POSIXct(df2()$Date)), na.rm = TRUE),
#       max(c(as.POSIXct(df1()$Date), as.POSIXct(df2()$Date)), na.rm = TRUE), 
#       by = "day"), 
#   c(.92, .95))
