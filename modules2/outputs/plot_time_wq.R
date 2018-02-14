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

PLOT_TIME_WQ_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    fluidRow(
      uiOutput(ns("plot_ui"))
    ),
    # Plot Options
    fluidRow(br(), br()),
    fluidRow(
      column(11,
             tabsetPanel(
               ### Main Options
               tabPanel("Main Options", br(), br(),
                          column(4,
                                 wellPanel(
                                   fluidRow(
                                     column(6,
                                            uiOutput(ns("param1_ui"))
                                     ),
                                     column(6,
                                            uiOutput(ns("param2_ui"))
                                     )
                                   )
                                 ),
                                 wellPanel(
                                   checkboxGroupInput(ns("axis"), "Axis Options:",
                                                      choices= c("Log-scale Y-Axis",
                                                                 "Y-axis start at zero"))
                                 ),
                                 wellPanel(
                                   sliderInput(ns("point_size"), "Point Size:",
                                               min = 0.5, max = 4, value = 1.5, step = 0.5)
                                 )
                          ), # end column
                          column(4,
                                 wellPanel(
                                   fluidRow(
                                     column(6,
                                            uiOutput(ns("group_color_ui"))
                                     ), # end column
                                     # new column
                                     column(6,
                                            uiOutput(ns("group_shape_ui"))
                                     ) # end column
                                   )
                                 ),
                                 wellPanel(
                                   fluidRow(
                                     radioButtons(ns("point_shape"), "Point Shape (when not grouped by shape):",
                                                  choices = c("circle" = 16,
                                                              "square" = 15,
                                                              "triangle" = 17,
                                                              "diamond" = 18),
                                                  inline = TRUE),
                                     radioButtons(ns("point_color1"), "Point Color of Primary Axis (when not grouped by color):",
                                                  choices = c("black", "blue", "red", "green"),
                                                  inline = TRUE),
                                     radioButtons(ns("point_color2"), "Point Color of Secondary Axis (when not grouped by color):",
                                                  choices = c("black", "blue", "red", "green"),
                                                  selected = "green",
                                                  inline = TRUE)
                                   )
                                 )
                          ), # end column
                          column(4,
                                 wellPanel(
                                   fluidRow(
                                     strong("Trend Line(s)"),
                                     checkboxInput(ns("trend_show"), "Show Trendline")
                                   ),
                                   fluidRow(
                                     column(6,
                                            radioButtons(ns("trend_type"), "Trendline Type",
                                                         choices= c("Linear" = "lm",
                                                                    "Loess" = "loess",
                                                                    "Generalized Additive" = "gam"))
                                     ),
                                     column(6,
                                            strong("Confidence Ribbon"),
                                            checkboxInput(ns("trend_ribbon"), "Show Conf. Ribbon"),
                                            radioButtons(ns("trend_conf"), NULL,
                                                         choices= c(0.90,0.95,0.99))
                                     )
                                   ),
                                   radioButtons(ns("trend_line"), "Line Type (when not grouped by shape)",
                                                choices = c("solid", "dashed", "dotted"),
                                                inline = TRUE),
                                   sliderInput(ns("trend_size"), "Line Thickness:",
                                               min = 0, max = 3, value = 1, step = 0.25),
                                   sliderInput(ns("trend_alpha"), "Line Opacity:",
                                               min = 0, max = 1, value = .1, step = 0.1)

                                 )
                          ) # end column
               ), # end Tab Panel
               ### More Display Options
               tabPanel("More Display Options", br(), br(),
                        PLOT_DISPLAY_OPTIONS_UI(ns("more_display"))
               ),
               ### Titles and Axis Labels
               tabPanel("Title and Axis Labels", br(), br(),
                        PLOT_TITLE_AND_LABELS_UI(ns("title_label"))
               ), # end Tab Panel
               ### Save Plot
               tabPanel("Save Plot", br(), br(),
                        wellPanel(
                          fluidRow(
                            column(2,
                                   downloadButton(ns('save_plot'), "Save Plot")
                            ),
                            column(3,
                                   numericInput(ns("save_width"), "Plot Width (inches):", 7,
                                                min = 3, max = 17, step = 0.25),
                                   
                                   numericInput(ns("save_height"), "Plot Height (inches):", 5,
                                                min = 3, max = 17, step = 0.25)
                                   
                            ),
                            column(2,
                                   radioButtons(ns("save_type"), "File Type:",
                                                choices= c("pdf",
                                                           "jpg",
                                                           "png"))
                            ),
                            column(2,
                                   checkboxGroupInput(ns("save_grid"), "Gridline Override:",
                                                      choices= c("major gridlines",
                                                                 "minor gridlines"))
                            ) # end column
                          )
                        )
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

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

PLOT_TIME_WQ <- function(input, output, session, Df) {

  ns <- session$ns # see General Note 1
  
  
  # Find Choices for Sites and Param (could make it so plot.time module server function accepts this instead (probably faster)
  Param <- reactive({Df()$Parameter %>% factor() %>% levels()})

  # Parameter Axis Choice for Primary Axis
  output$param1_ui <- renderUI({
    if(Df() %>% summarise(n()) %>% unlist() != 0){
      radioButtons(ns("param1"), "Primary Y-Axis Parameter",
                   choices = Param())
    }
  })
  
  outputOptions(output, "param1_ui", suspendWhenHidden = FALSE) # see Dev. Manual
  
  # update when new flag code changes
  observe({
    
    # save previously selected value
    isolate({save_selected <- input$param1})
    
    updateRadioButtons(session, inputId = "param1", #label = "Primary Y-Axis Parameter", 
                      choices = Param(),
                      selected = save_selected)
    
  })
  
  # Parameter Axis Choice for Secondary Axis
  output$param2_ui <- renderUI({
    radioButtons(ns("param2"), "Secondary Y-Axis Parameter",
                 choices = c("None", Param()),
                 selected = "None")
  })
  
  outputOptions(output, "param2_ui", suspendWhenHidden = FALSE) # See Dev. Manual
  
  # update when new flag code changes
  observe({
    
    # save previously selected value
    isolate({save_selected <- input$param2})
    
    updateRadioButtons(session, inputId = "param2", #label = "Primary Y-Axis Parameter", 
                      choices = c("None", Param()),
                      selected = save_selected)
    
  })

  
  # Two Dataframes (Primary and Secondary Axis)
  
  Df1 <- reactive({
    req(input$param1)
    Df() %>% filter(Parameter %in% input$param1)})
  
  Df2 <- reactive({
    req(input$param1) # param1 not param2 in req() becuase param2 always has "None" option
    if(input$param2 != "None"){
      Df() %>% filter(Parameter %in% input$param2)
    }else{
      NULL
    }
  })
  
  # Site List  - Primary Axis Only Plots
  Site <- reactive({
    if(input$param2 == "None"){
      Df1()$Site %>% factor() %>% levels()
    } else{
      unique(Df1()$Site %>% factor() %>% levels(), Df2()$Site %>% factor() %>% levels())
    }
  })
  
  # Flag List
  Site <- reactive({
    if(input$param2 == "None"){
      Df1()$Site %>% factor() %>% levels()
    } else{
      unique(Df1()$Site %>% factor() %>% levels(), Df2()$Site %>% factor() %>% levels())
    }
  })
  
  # Color Grouping UI
  output$group_color_ui <- renderUI({
    req(input$param2)
    if(input$param2 == "None"){
      color_choices <- c("None", "Site", "Flags (needs update)")
      radioButtons(ns("group_color"), label = "Group with Colors:",
                   choices = color_choices,
                   selected = "Site")
    } else {
      tagList(
        strong("Group with Colors:"),
        p("Coloring is used for the two parameters, choose colors below.")
      )
    }
  })
  
  # # update when new flag code changes
  # observe({
  #   
  #   # save previously selected value
  #   isolate({save_selected <- input$group_color})
  #   
  #   #if(input$param2 == "None"){
  #     color_choices <- c("None", "Site", "Flags (needs update)")
  #     updateRadioButtons(session, inputId = "group_color", label = "Parameter:", 
  #                       choices=c(color_choices),
  #                       selected = save_selected)
  #   #}
  # })
  
  # Shape Grouping
  output$group_shape_ui <- renderUI({
    req(input$param2)
    
    # Make so this includes all types of flags. Probably make sticky memory
    shape_choices <- c("None/Parameter", "Site", "Flags (needs update)")
    
    radioButtons(ns("group_shape"), label = "Group with Shapes:",
                 choices = shape_choices,
                 selected = "None/Parameter")
  })
  
  # # update when new flag code changes
  # observeEvent(c(Df1(),Df2()), {
  #   
  #   # save previously selected value
  #   isolate({save_selected <- input$group_shape})
  #   
  #   shape_choices <- c("None/Parameter", "Site", "Flags (needs update)")
  #   updateRadioButtons(session, inputId = "group_shape", label = "Parameter:", 
  #                     choices=c(shape_choices),
  #                     selected = save_selected)
  #   
  # })

  
  ### Plot Outputs 
  
  # ONe Y-axis interactive Plotly plot
  output$plot1 <- renderPlotly({
    req(input$param2 == "None")
    ggplotly(P4())
  })
  
  # Two Y-axis ggplot static plot
  output$plot2 <- renderPlot({
    req(input$param2 != "None")
    P4()
  })
  
  output$plot_ui <- renderUI({
    req(input$param1, input$param2)
    if(input$param2 == "None"){
      plotlyOutput(ns("plot1"), width = "100%", height = 600)
    } else{
      tagList(
        plotOutput(ns("plot2"), width = "100%", height = 600),
        h4(textOutput(ns("plot2text")))
      )
    }
  })
  
  
  
  ### Plot Creation
  
  # Main Plot Scripts
  P1 <- reactive({
    
    # Features all plots have in common (This is unfortunetly a small list haha)
    
    ### One Parameter Plots - Primary Y axis only
    
    if(input$param2 == "None"){
      
      p <- ggplot(Df1())
      
      # Group by both Color and Shape when both selected
      if(input$group_color != "None" & input$group_shape != "None/Parameter"){
          p <- p + geom_point(aes_string(x = "as.POSIXct(Date)", 
                                         y = "Result", 
                                         color = input$group_color, 
                                         shape = input$group_shape),
                              size = input$point_size)
          if(input$trend_show == TRUE){
            p <- p + geom_smooth(aes_string(x = "as.POSIXct(Date)", 
                                            y = "Result", 
                                            color = input$group_color, 
                                            linetype = input$group_shape),
                                 method = input$trend_type,
                                 size = input$trend_size,
                                 alpha = input$trend_alpha,
                                 se = input$trend_ribbon,
                                 level = as.numeric(input$trend_conf))
          }
          # Group by only Color when only color grouping is selected
      } else if (input$group_color != "None"){
        p <- p + geom_point(aes_string(x = "as.POSIXct(Date)", 
                                       y = "Result", 
                                       color = input$group_color), 
                            shape = input$point_shape,
                            size = input$point_size)
        if(input$trend_show == TRUE){
          p <- p + geom_smooth(aes_string(x = "as.POSIXct(Date)", 
                                          y = "Result", 
                                          color = input$group_color),
                               linetype = input$trend_line,
                               method = input$trend_type,
                               size = input$trend_size,
                               alpha = input$trend_alpha,
                               se = input$trend_ribbon,
                               level = as.numeric(input$trend_conf))
        }
        # Group by only Shape when only shape grouping is selected
      } else if (input$group_shape != "None/Parameter"){
        p <- p + geom_point(aes_string(x = "as.POSIXct(Date)", 
                                       y = "Result", 
                                       shape = input$group_shape), 
                            color = input$point_color1,
                            size = input$point_size)
        
        if(input$trend_show == TRUE){
          p <- p + geom_smooth(aes_string(x = "as.POSIXct(Date)", 
                                          y = "Result", 
                                          linetype = input$group_shape),
                               method = input$trend_type,
                               size = input$trend_size,
                               alpha = input$trend_alpha,
                               se = input$trend_ribbon,
                               level = as.numeric(input$trend_conf))
        }
        # No Grouping Selected
      } else {
        p <- p + geom_point(aes_string(x = "as.POSIXct(Date)", 
                                       y = "Result"),
                            color = input$point_color1,
                            shape = input$point_shape,
                            size = input$point_size)
        if(input$trend_show == TRUE){
          p <- p + geom_smooth(aes_string(x = "as.POSIXct(Date)", 
                                          y = "Result"),
                               linetype = input$trend_line,
                               method = input$trend_type,
                               size = input$trend_size,
                               alpha = input$trend_alpha,
                               se = input$trend_ribbon,
                               level = as.numeric(input$trend_conf))
        }
      }
      
      
      # Facet for Sites if no grouping for site is selected and number of sites is greater than 1
      if(input$group_color != "Site" & input$group_shape != "Site" & length(c(Site())) > 1){
        p <- p + facet_wrap(~Site, ncol = ceiling(length(c(Site()))/4))
      }
      
      ### Two Parameter Plots - Primary and Secondary Y axes'
    } else{
      
      p <- ggplot()
      
      # Grouped by Shape
      if(input$group_shape != "None/Parameter"){
        p <- p + geom_point(data = Df1(), 
                            aes_string(x = "as.POSIXct(Date)", 
                                       y = "Result", 
                                       shape = input$group_shape),
                            color = input$point_color1,
                            size = input$point_size*2) +
          geom_point(data = Df2(),
                     aes_string(x = "as.POSIXct(Date)", 
                                y = "Result*Mult()", 
                                shape = input$group_shape), 
                     color = input$point_color2,
                     size = input$point_size*2)
        if(input$trend_show == TRUE){
          p <- p + geom_smooth(data = Df1(), aes_string(x = "as.POSIXct(Date)", 
                                                        y = "Result", 
                                                        linetype = input$group_shape),
                               color = input$point_color1,
                               method = input$trend_type,
                               size = input$trend_size,
                               alpha = input$trend_alpha,
                               se = input$trend_ribbon,
                               level = as.numeric(input$trend_conf)) + 
            geom_smooth(data = Df2(), aes_string(x = "as.POSIXct(Date)", 
                                                 y = "Result*Mult()",
                                                 linetype = input$group_shape),
                        color = input$point_color2,
                        method = input$trend_type,
                        size = input$trend_size,
                        alpha = input$trend_alpha,
                        se = input$trend_ribbon,
                        level = as.numeric(input$trend_conf))
        }
        # Not Grouped 
      } else{
        p <- p + geom_point(data = Df1(), 
                            aes_string(x = "as.POSIXct(Date)", 
                                       y = "Result"),
                            shape = 15,
                            color = input$point_color1,
                            size = input$point_size*2) +
          geom_point(data = Df2(),
                     aes_string(x = "as.POSIXct(Date)", 
                                y = "Result*Mult()"), 
                     shape = 17,
                     color = input$point_color2,
                     size = input$point_size*2)
      }
      if(input$trend_show == TRUE){
        p <- p + geom_smooth(data = Df1(), aes_string(x = "as.POSIXct(Date)", 
                                                      y = "Result"),
                             linetype = input$trend_line,
                             color = input$point_color1,
                             method = input$trend_type,
                             size = input$trend_size,
                             se = input$trend_ribbon,
                             level = as.numeric(input$trend_conf)) + 
          geom_smooth(data = Df2(), aes_string(x = "as.POSIXct(Date)", 
                                               y = "Result*Mult()"),
                      linetype = input$trend_line,
                      color = input$point_color2,
                      method = input$trend_type,
                      size = input$trend_size,
                      se = input$trend_ribbon,
                      level = as.numeric(input$trend_conf))
      }
      
      
      # Facet for Sites if no grouping for site is selected and number of sites is greater than 1
      if(input$group_shape != "Site" & length(c(Site())) > 1){
        p <- p + facet_wrap(~Site, ncol = ceiling(length(c(Site()))/4))
      }
      
    }
    
    p <- p + scale_x_datetime(breaks = pretty_breaks(n=12))
    
    p
    
  }) 
      
      

  
  # Display OPtions Tab
  P2 <- callModule(PLOT_DISPLAY_OPTIONS, "more_display", 
                   P = P1, 
                   Df1 = Df1, 
                   Df2 = Df2, 
                   x = "as.POSIXct(Date)", y = "Result")
  
  
  # Title and Labels - returns a list of two reactive expressions. One a plot object and one a text string Sec. Axis label
  P3 <- callModule(PLOT_TITLE_AND_LABELS, "title_label", 
                   P = P2, 
                   Title_Auto = Plot_Name, # error w/o reactive?
                   X_Lab_Auto = reactive({"Date"}), 
                   Y_Lab_Auto = reactive({paste(Text_Param1(), " (", Text_Units1(),")", sep= "")}), 
                   sec_y_axis = TRUE,
                   Y2_Lab_Auto = reactive({paste(Text_Param2(), " (", Text_Units2(),")", sep= "")}))
  
  
  # Log Axis Scripts
  P4 <- reactive({
    
    p <- P3$Plot()
    
    # Primary Axis Only
    if(input$param2 == "None"){
      if("Log-scale Y-Axis" %in% input$axis){
        p <- p + scale_y_log10()
      } else {
        if("Y-axis start at zero" %in% input$axis){
          ymax <- max(Df1()$Result, na.rm = TRUE)
          p <- p + scale_y_continuous(limits = c(0,ymax))
        } else {
          p <- p + scale_y_continuous()
        }
      }
      # Secondary and Primary Axis (2 Parameter Plots)
    } else{

      # Log Scale
      if("Log-scale Y-Axis" %in% input$display_axis){
        p <- p + scale_y_log10(sec.axis = sec_axis(~./Mult(), breaks = trans_breaks('log10', function(x) 10^x),
                                                   name = "hey")) #P3$Y2_Lab()
      } else{
        if("Y-axis start at zero" %in% input$display_axis){
          p <- p + scale_y_continuous(breaks = pretty_breaks(),limits = c(0,y1max),
                                      sec.axis = sec_axis(~./Mult(), breaks = pretty_breaks(), 
                                                          name = "hey"))
        } else {
          p <- p + scale_y_continuous(breaks = pretty_breaks(),
                                      sec.axis = sec_axis(~./Mult(), breaks = pretty_breaks(), 
                                                          name = "howdy"))
        }
      }
      p <- p + theme(text = element_text(size = 15))
    }

    # Save Options
    
    # Size dependent? Change size for saving?
    p <- p + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))
    
    # Gridlines for saving options - nonplotly inage
    if("major gridlines" %in% input$save_grid){
      p <- p + theme(panel.grid.major = element_line())
    }
    if("minor gridlines" %in% input$save_grid){
      p <- p + theme(panel.grid.minor = element_line())
    }
    
    p 
    
  }) # end Plot

  # Multiplier for Secondary Y axis
  Mult <- reactive({
    
    # Scalars for Secondary Y-axis
    y1min <- min(Df1()$Result, na.rm = TRUE)
    y2min <- min(Df2()$Result, na.rm = TRUE)
    y1max <- max(Df1()$Result, na.rm = TRUE)
    y2max <- max(Df2()$Result, na.rm = TRUE)
    
    # Two Y-axis Log Scale adn non-Lod Scale
    if("Log-scale Y-Axis" %in% input$axis){
      mult <- exp(log(y1max  - y1min) / log(y2max - y2min))
    } else{
      if("Y-axis start at zero" %in% input$display_axis){
        mult <- y1max / y2max 
      } else {
        mult <- (y1max  - y1min) / (y2max - y2min)
      }
    }
  
    mult
    
  })
  

  
  ### Texts For Plot
  
  Text_Site <- reactive({Site() %>% paste(collapse = ", ")})
  
  Text_Param1 <- reactive({Df1()$Parameter %>% factor() %>% levels() %>% paste()})
  
  Text_Param2 <- reactive({Df2()$Parameter %>% factor() %>% levels() %>% paste()})
  
  Text_Units1 <- reactive({Df1()$Units %>% factor() %>% levels() %>% paste(collapse = ", ")})
  
  Text_Units2 <- reactive({Df2()$Units %>% factor() %>% levels() %>% paste(collapse = ", ")})
  
  Text_Date_Start1 <- reactive({Df1()$Date %>% min(na.rm = TRUE) %>% paste()})
  
  Text_Date_End1 <- reactive({Df1()$Date %>% max(na.rm = TRUE) %>% paste()})
  
  Text_Date_Start2 <- reactive({c(Df1()$Date, Df2()$Date)  %>% min(na.rm = TRUE) %>% paste()})
  
  Text_Date_End2 <- reactive({c(Df1()$Date, Df2()$Date) %>% max(na.rm = TRUE) %>% paste()})
  
  Plot_Name <- reactive({
    if(input$param2 == "None"){
      paste0(Text_Param1(),' at Site(s) ', Text_Site(),
             ' from ', Text_Date_Start1(),' to ', Text_Date_End1())
    } else{
      paste0(Text_Param1(), ' and ', Text_Param2(), ' at Site(s) ', Text_Site(),
             ' from ', Text_Date_Start2(),' to ', Text_Date_End2())
    }
  })
  
  # legend for double y-axis plot
  output$plot2text <- renderText({
    paste(input$point_color1, "=", Text_Param1(), " , ", input$point_color2, "=", Text_Param2())
    })

  # Filename
  
  Filename <- reactive({
    paste0(Plot_Name(), ".", input$save_type)
  })
  
  # Plot Print

  output$save_plot <- downloadHandler(
    filename = function(){Filename()},
    content = function(file){ggsave(file, plot = P4(), 
                                    width = input$save_width,
                                    height = input$save_height,
                                    device = input$save_type)}#,
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
#   seq(min(c(as.POSIXct(Df1()$Date), as.POSIXct(Df2()$Date)), na.rm = TRUE),
#       max(c(as.POSIXct(Df1()$Date), as.POSIXct(Df2()$Date)), na.rm = TRUE), 
#       by = "day"), 
#   c(.92, .95))
