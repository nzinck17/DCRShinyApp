##############################################################################################################################
#     Title: Profile-Heatmap.R
#     Type: Module for DCR Shiny App
#     Description: Heatmap and Heatmap 3D Plots for Profile Data
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#   2. Tried Progress Bar for Plot and did not work well. Used Custom Message instead

# To-Do List:
#   1. Make Loading Bar for Plot
#   2. Make option for COloring Scale (whether based on Site, Year; Site; or None)
#   3. Change Decimal Date to DOY

##############################################################################################################################
# User Interface
##############################################################################################################################

PROF_HEATMAP_UI <- function(id, df) {
  
ns <- NS(id)
  
tagList(
  wellPanel(
    fluidRow(
      column(2,
             # Site Input
             selectInput(ns("site"), "Site: (Choose First)", 
                         choices = levels(factor(df$Site)), 
                         selected = factor(df$Site)[1])
      ),
      column(3,
             # Parameter Input
             uiOutput(ns("param_ui"))
      ),
      column(2,
             # Date Input
             uiOutput(ns("date_ui"))
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
             selectInput(ns("plot_color"), "Plot Color Style:", 
                         choices = c("blue scale",
                                     "rainbow",
                                     "blue-red"), 
                         selected = "rainbow")
      ),
      column(1,
             #download button
             downloadButton(ns('save_plot'), "Save Plot")
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
    ),
    tabPanel("Standard Template Heatmap Plot",
             fluidRow(
               h2("Soon to Come", align = "center")
             )
    )
  )
) # end taglist
} # end UI


##############################################################################################################################
# Server Function
##############################################################################################################################

PROF_HEATMAP <- function(input, output, session, df) {
  
  # filter DF for blank data
  
  df <- df %>% filter(!is.na(Date), 
                      !is.na(Depthm),
                      !is.na(Result))
  
  # Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6
  
  parameters_non_historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  
  # Parameter Selection UI
  
  output$param_ui <- renderUI({
    
    req(input$site) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    param_choices_new <- df %>%
      filter(Site %in% input$site,
             Parameter %in% parameters_non_historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    param_choices_old <- df %>%
      filter(Site %in% input$site,
             !(Parameter %in% parameters_non_historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    param_choices <- c(param_choices_new, param_choices_old)
    
    selectInput(ns("param"), "Parameter: (Choose Second)",
                choices=c(param_choices))
    
  })
  
  # Date Selection UI
  
  output$date_ui <- renderUI({
    
    req(input$site, input$param) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    dates <- df %>% 
      filter(Site %in% input$site, Parameter %in% input$param) %>%
      .$Date
    
    date_min <- dates %>% min(na.rm=TRUE)
    date_max <- dates %>% max(na.rm=TRUE)
    
    # Date Input
    selectInput(ns("date"), "Year:", 
                choices = year(seq(date_min, date_max, "years")), 
                selected = year(date_max))
    
  })
  
  
# Reactive Data Frames:
  
  Df2 <- reactive({
    
    req(input$site, input$param, input$date) # See General Note 5
    
    df %>% filter(Site %in% input$site,
                  Parameter %in% input$param,
                  year(Date) == input$date)

  })
  
# Reactive Data for color scale use
  
  Df_Limit <- reactive({df %>% filter(Parameter %in% input$param) %>% filter(!is.na(Result))})
  Lower_Limit <- reactive({min(Df_Limit()$Result)})
  Upper_Limit <- reactive({max(Df_Limit()$Result)})
  Mid_Point <- reactive({(Lower_Limit() + Upper_Limit()) / 2})
  
# Heat map Plot creation
  
  P <- reactive({
    
    # if interploation is selected
    if(input$interp != "none"){
      df_plot <- akima::interp(x = decimal_date(Df2()$Date), y = Df2()$Depthm, z = Df2()$Result, duplicate="strip", nx = 100, ny = 100)
      df_plot <- interp2xyz(df_plot, data.frame=TRUE)
      df_plot <- rename(df_plot, Date = x, Depthm = y, Result = z)
      df_plot$Date <- as.Date(format(date_decimal(df_plot$Date), "%Y-%m-%d"))
    } else {
      df_plot <- Df2()
    }
    
    p <-  ggplot(df_plot, aes(x=Date, y=Depthm, z=Result, fill=Result)) +
      geom_tile(height = 1) +
      scale_y_reverse() + 
      scale_x_date(date_breaks = "months", date_labels = ("%b-%Y"))
    
    if(input$plot_color == "blue scale"){
      p <- p + scale_fill_gradient(limits = c(Lower_Limit(),Upper_Limit()))
    } else if(input$plot_color == "rainbow"){
      p <- p + scale_fill_gradientn(colours=rainbow(30), limits = c(Lower_Limit(),Upper_Limit()))  #
    } else if(input$plot_color == "blue-red"){
      p <- p + scale_fill_gradient2(low="blue", mid="white", high="red", 
                                    midpoint = Mid_Point(), limits = c(Lower_Limit(),Upper_Limit()))  #
    }
    
    p
    
  })

  
# Heat Map Plot - Plotly Visualization
  
  output$plot <- renderPlotly({
    ggplotly(P()) %>% config(displayModeBar = F)  %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  


  # 3D Plot creation
  
  output$plot3D <- renderPlotly({
    
    # if interploation is selected
    df_interp <- akima::interp(x = decimal_date(Df2()$Date), y = Df2()$Depthm, z = Df2()$Result, duplicate="strip", nx = 100, ny = 100)
    
    if(input$interp != "none"){
      p3D <- plot_ly(x = df_interp[[2]], y = df_interp[[1]], z = df_interp[[3]]) %>% 
        add_surface() %>%
        layout(scene = list(
          xaxis = list(title = 'Depth (m)'),  
          yaxis = list(title = 'Year'),
          zaxis = list(title = input$param),
          camera = list(eye = list(x = 0.5, y = 0.2, z = 2.0))))
    } else {
      p3D <- plot_ly(Df2(), x = ~Result, y = ~Date, z = ~Depthm*-1, color = ~Result) %>%
        add_markers() %>%
        layout(scene = list(
          xaxis = list(title = input$param),  
          yaxis = list(range = 
                         c(as.numeric(as.POSIXct(min(Df2()$Date), format="%Y-%m-%d"))*1000,
                           as.numeric(as.POSIXct(max(Df2()$Date), format="%Y-%m-%d"))*1000),
                       type = "date"),
          zaxis = list(title = 'Depth (m)'))) #autorange = "reversed"  (no reverse capability yet of 3D plots)
          
    }
    
    p3D
    
  }) 
  
# Plot - print
  
  output$save_plot <- downloadHandler(
    filename = function (){paste(input$date,' ', input$param,' Site ',input$site,'.png', sep='')},
    content = function(file) {ggsave(file, plot = p(), device = "png")},   #function(file) {ggsave(file, plot = p(), device = "png")}
    contentType = 'image/png'
  )
  
# Table
  
  output$table <- renderDataTable(Df2())
  
} # end server
