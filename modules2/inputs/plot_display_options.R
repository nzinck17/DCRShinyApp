##############################################################################################################################
#     Title: plot_display_options.R
#     Type: Module2
#     Description: Time Series plot (for non-depth dependent data)
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#   1. 

# To-Do List:
#   1. 

##############################################################################################################################
# User Interface
##############################################################################################################################

PLOT_DISPLAY_OPTIONS_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    
    fluidRow(
      column(3,
             wellPanel(
               radioButtons(ns("theme"), "Theme:",
                            choices= c("Gray",
                                       "Black and White",
                                       "Line Draw",
                                       "Light",
                                       "Dark",
                                       "Minimal",
                                       "Classic")),
               sliderInput(ns("psize"), "Point Size:",
                           min = 0.5, max = 4, value = 1.5, step = 0.5),
               sliderInput(ns("pshape"), "Point Shape:",
                           min = 0, max = 5, value = 1, step = 1)
             )
      ), # end column
      column(3,
             strong("Horizontal Line 1:"),
             textInput(ns("hline1_int"), "Y intercepts"),
             h5("multiple numbers seperated with comma"),
             radioButtons(ns("hline1_type"), "Line Type",
                          choices = c("solid", "dash", "dotted")),
             sliderInput(ns("hline1_size"), "Thickness:",
                         min = 0, max = 3, value = 1, step = 0.25),
             sliderInput(ns("hline1_alpha"), "Transparency:",
                         min = 0, max = 1, value = 1, step = 0.1)
      ), # end column
      column(3,
             strong("Horizontal Line 2:"),
             textInput(ns("hline2_int"), "Y intercepts"),
             h5("multiple numbers seperated with comma"),
             radioButtons(ns("hline2_type"), "Line Type",
                          choices = c("solid", "dash", "dotted")),
             sliderInput(ns("hline2_size"), "Thickness:",
                         min = 0, max = 3, value = 1, step = 0.25),
             sliderInput(ns("hline2_alpha"), "Transparency:",
                         min = 0, max = 1, value = 1, step = 0.1)
      ), # end column
      column(3,
             strong("Vertical Line:"),
             textInput(ns("vline_int"), "Y intercepts"),
             h5("multiple numbers seperated with comma"),
             radioButtons(ns("vline_type"), "Line Type",
                          choices = c("solid", "dash", "dotted")),
             sliderInput(ns("vline_size"), "Thickness:",
                         min = 0, max = 3, value = 1, step = 0.25),
             sliderInput(ns("vline_alpha"), "Transparency:",
                         min = 0, max = 1, value = 1, step = 0.1)
      ) # end column
    )
  )
}


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

PLOT_DISPLAY_OPTIONS <- function(input, output, session, P, Df1, Df2, x, y) {
  
  ns <- session$ns # see General Note 1
  
  
  ### Plot Additions 
  P1 <- reactive({
    
    p <- P()
    
    ### Theme based on selection
    
    if(input$theme == "Gray"){
      p <- p + theme_gray()
    }
    if(input$theme == "Black and White"){
      p <- p + theme_bw()
    }
    if(input$theme == "Line Draw"){
      p <- p + theme_linedraw()
    }
    if(input$theme == "Light"){
      p <- p + theme_light()
    }
    if(input$theme == "Dark"){
      p <- p + theme_dark()
    }
    if(input$theme == "Minimal"){
      p <- p + theme_minimal()
    }
    if(input$theme == "Classic"){
      p <- p + theme_classic()
    }
  

    ### Add Lines
    
    # Horizontal Line 1
    if(isTruthy(input$hline1_int)){
      p <- p + geom_hline(data = Df1(), aes_string(x = x, y = y),
                          yintercept = Hline1_Int(),
                          linetype = input$hline1_type,
                          size = input$hline1_size,
                          alpha = input$hline1_alpha)
    }
    
    # Horizontal Line 2
    if(isTruthy(input$hline2_int)){
      p <- p + geom_hline(data = Df1(), aes_string(x = x, y = y),
                          yintercept = Hline2_Int(),
                          linetype = input$hline2_type,
                          size = input$hline2_size,
                          alpha = input$hline2_alpha)
    }
    
    # Vertical Line 1
    if(isTruthy(input$vline_int)){
      p <- p + geom_vline(data = Df1(), aes_string(x = x, y = y),
                          xintercept = Vline_Int(),
                          linetype = input$vline_type,
                          size = input$vline_size,
                          alpha = input$vline_alpha)
    }
    
  p
  
  })
  
  
  # Text to number for Line intercept inputs
  Hline1_Int <- reactive({
    req(input$hline1_int)
    str_split(input$hline1_int, ",") %>% 
      unlist() %>%
      as.numeric()
  })
  
  Hline2_Int <- reactive({
    req(input$hline2_int)
    str_split(input$hline2_int, ",") %>% 
      unlist() %>%
      as.numeric()
  })
  
  Vline_Int <- reactive({
    req(input$vline_int)
    str_split(input$vline_int, ",") %>% 
      unlist() %>%
      as.numeric()
  })
  
  # include text input
  
return(P1)
  
} # end Server Function



#paste0(Text_Param2(), " (", Text_Units2(),")")