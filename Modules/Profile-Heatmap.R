##############################################################################################################################
#     Title: Profile-Heatmap.R
#     Type: Module for DCR Shiny App
#     Description: Heatmap and Heatmap 3D Plots for Profile Data
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:
#   1. Make Loading Bar for Plot
#   2. Make option for COloring Scale (whether based on Site, Year; Site; or None)
#   3. Change Decimal Date to DOY

##############################################################################################################################
# User Interface
##############################################################################################################################

prof.heatmap.UI <- function(id, df) {
  
ns <- NS(id)
  
tagList(
  wellPanel(
    fluidRow(
      # first column
      column(2,
             # Site Input
             selectInput(ns("site"), "Site: (Choose First)", 
                         choices = levels(factor(df$Site)), 
                         selected = factor(df$Site)[1])
      ),
      column(3,
             # Parameter Input
             uiOutput(ns("param.ui"))
      ),
      column(2,
             # Date Input
             uiOutput(ns("date.ui"))
      ),
      column(2,
             #Interpolation Type
             selectInput(ns("interp"), "Interpolation Method:", 
                         choices = c("none" = "none",
                                     "linear" = TRUE), 
                         selected = TRUE)
      ),
      column(2,
             #plot color style
             selectInput(ns("plot.color"), "Plot Color Style:", 
                         choices = c("blue scale",
                                     "rainbow",
                                     "blue-red"), 
                         selected = "rainbow")
      ),
      column(1,
             #download button
             downloadButton(ns('save.plot'), "Save Plot")
      )
    ) # end fluid row
  ), # end well panel
  tabsetPanel(
    # the "Plot" tab panel where everything realted to the plot goes
    tabPanel("Plot",
             plotlyOutput(ns("plot"), height = 500)
    ),
    tabPanel("Plot3D",
             plotlyOutput(ns("plot3D"), height = 700)
    ),
    tabPanel("Table",
             dataTableOutput(ns("table"))
    )
  )
) # end taglist
} # end UI


##############################################################################################################################
# Server Function
##############################################################################################################################

prof.heatmap <- function(input, output, session, df) {
  
  # Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6
  
  parameters.non.historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  
  # Parameter Selection UI
  
  output$param.ui <- renderUI({
    
    req(input$site) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    param.choices.new <- df %>%
      filter(Site %in% input$site,
             Parameter %in% parameters.non.historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    param.choices.old <- df %>%
      filter(Site %in% input$site,
             !(Parameter %in% parameters.non.historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    param.choices <- c(param.choices.new, param.choices.old)
    
    selectInput(ns("param"), "Parameter: ",
                choices=c(param.choices))
    
  })
  
  # Date Selection UI
  
  output$date.ui <- renderUI({
    
    req(input$site) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    Dates <- df %>% 
      filter(Site %in% input$site) %>%
      .$Date
    
    Date.min <- Dates %>% min(na.rm=TRUE)
    Date.max <- Dates %>% max(na.rm=TRUE)
    
    # Date Input
    selectInput(ns("date"), "Year:", 
                choices = year(seq(Date.min, Date.max, "years")), 
                selected = year(Date.max))
    
  })
  
  
# Reactive Data Frames:
  
  df.react <- reactive({
    df %>% 
      filter(Site %in% input$site,
             Parameter %in% input$param,
             year(Date) == input$date)
    #filter(!is.na(Date)) %>%
    #filter(!is.na(Depthm)) %>%
    #filter(!is.na(Result))
  })
  
# Reactive Data for color scale use
  
  df.limit <- reactive({df %>% filter(Parameter %in% input$param) %>% filter(!is.na(Result))})
  lowerlimit <- reactive({min(df.limit()$Result)})
  upperlimit <- reactive({max(df.limit()$Result)})
  midpoint <- reactive({(lowerlimit() + upperlimit()) / 2})
  
# Heat map Plot creation
  
  p <- reactive({
    
    # if interploation is selected
    if(input$interp != "none"){
      df.plot <- akima::interp(x = decimal_date(df.react()$Date), y = df.react()$Depthm, z = df.react()$Result, duplicate="strip", nx = 100, ny = 100)
      df.plot <- interp2xyz(df.plot, data.frame=TRUE)
      df.plot <- rename(df.plot, Date = x, Depthm = y, Result = z)
    } else {
      df.plot <- df.react()
    }
    
    p <-  ggplot(df.plot, aes(x=Date, y=Depthm, z=Result, fill=Result)) +
      geom_tile(height = 1) +
      scale_y_reverse()
    
    if(input$plot.color == "blue scale"){
      p <- p + scale_fill_gradient(limits = c(lowerlimit(),upperlimit()))
    } else if(input$plot.color == "rainbow"){
      p <- p + scale_fill_gradientn(colours=rainbow(30), limits = c(lowerlimit(),upperlimit()))  #
    } else if(input$plot.color == "blue-red"){
      p <- p + scale_fill_gradient2(low="blue",mid="white",high="red", midpoint=midpoint(), limits = c(lowerlimit(),upperlimit()))  #
    }
    
    p
    
  })
  
  
# Plot - Plotly Visualization
  
  output$plot <- renderPlotly({
    ggplotly(p()) %>% config(displayModeBar = F)  %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  


  # 3D Plot creation
  
  output$plot3D <- renderPlotly({
    
    # if interploation is selected
    df.interp <- akima::interp(x = decimal_date(df.react()$Date), y = df.react()$Depthm, z = df.react()$Result, duplicate="strip", nx = 100, ny = 100)
    
    if(input$interp != "none"){
      p3D <- plot_ly(x = df.interp[[2]], y = df.interp[[1]], z = df.interp[[3]]) %>% 
        add_surface() %>%
        layout(scene = list(
          xaxis = list(title = 'Depth (m)'),  
          yaxis = list(title = 'Year'),
          zaxis = list(title = input$param),
          camera = list(eye = list(x = 0.5, y = 0.2, z = 2.0))))
    } else {
      p3D <- plot_ly(df.react(), x = ~Result, y = ~Date, z = ~Depthm*-1, color = ~Result) %>%
        add_markers() %>%
        layout(scene = list(
          xaxis = list(title = input$param),  
          yaxis = list(range = 
                         c(as.numeric(as.POSIXct(min(df.react()$Date), format="%Y-%m-%d"))*1000,
                           as.numeric(as.POSIXct(max(df.react()$Date), format="%Y-%m-%d"))*1000),
                       type = "date"),
          zaxis = list(title = 'Depth (m)'))) #autorange = "reversed"  (no reverse capability yet of 3D plots)
          
    }
    
    p3D
    
  }) 
  
# Plot - print
  
  output$save.plot <- downloadHandler(
    filename = function (){paste(input$date,' ', input$param,' Site ',input$site,'.png', sep='')},
    content = function(file) {ggsave(file, plot = p(), device = "png")},   #function(file) {ggsave(file, plot = p(), device = "png")}
    contentType = 'image/png'
  )
  
# Table
  
  output$table <- renderDataTable(df.react())
  
} # end server
