##############################################################################################################################
#     Title: ProfileLinePlot - Shiny Module
#     Description: This script will plot Profile Line Plots
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

# load libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)

#=========================================================================
# UI side

prof.line.UI <- function(id, df) {
  
ns <- NS(id)

tagList(
  
  wellPanel(
    
    fluidRow(
      
      # first column
      column(3,
             
             # Parameter Input
             checkboxGroupInput(ns("param"), "Parameter:",        
                         choices=levels(factor(df$Parameter)),
                         selected = factor(df$Parameter[1]))
      ),
      
      
      column(3,
             
             radioButtons(ns("date.option"), "Choose Date Method:",        
                                choices=c("Select Year",
                                          "Select Month",
                                          "Calendar Range",
                                          "Single Day"),
                                selected = "Select Year"),
             
             # Date Input
             uiOutput(ns("date.ui"))
             
      ),
      
      
      column(2,
             
             # Site Input
             checkboxGroupInput(ns("site"), "Site:", 
                         choices = levels(factor(df$Site)), 
                         selected = factor(df$Site)[1])
      ),
                        
      
      column(2,
             
             radioButtons(ns("plot.color"), label = "Group with Colors:", 
                          choices = c("Parameter",
                                      "Site"),
                          selected = "Parameter"),

             radioButtons(ns("plot.linetype"), label = "Group with Linetypes:", 
                           choices = c("Parameter",
                                       "Site"),
                           selected = "Site")
      ),
      
      
      column(1,
             
             downloadButton(ns('save.plot'), "Save Plot")
      )
      

      )
  ), # well panel
  
  
  tabsetPanel(
    
    # the "Plot" tab panel where everything realted to the plot goes
    tabPanel("Plot", 
             
             fluidRow(
               plotOutput(ns("plot"), height = 500)
             )
    ),
    
    tabPanel("Plot (interactive)", 
             
             fluidRow(
               plotlyOutput(ns("plotly"), height = 500),
               downloadButton(ns('save.plot2'), "Save Plot")
             )
             
    ),
    
    tabPanel("Table", 
             
             fluidRow(
               dataTableOutput(ns("table.dynamic"))
             )
             
    )
  ) # end tabsetpanel
    
  ) # end taglist 
}

#========================================================================
# server side

prof.line <- function(input, output, session, df) {
  
# Depending on input$date.option, we'll generate a different UI date component 
  
  output$date.ui <- renderUI({
    
    ns <- session$ns
    
    switch(input$date.option,
           
           "Select Year" = selectInput(ns("date"), "Year:", 
                         choices = year(seq(as.Date("1990/1/1"), Sys.Date(), "years")), 
                         selected = year(Sys.Date() - years(10))),
           
           "Select Month" = selectInput(ns("date"), "Month:", 
                       choices = c(January = 1,
                                   February = 2,
                                   March = 3,
                                   April = 4,
                                   May = 5,
                                   June = 6,
                                   July = 7,
                                   August = 8,
                                   September = 9,
                                   October = 10,
                                   November = 11,
                                   December = 12), 
                       selected = month(Sys.Date())),
           
           "Calendar Range" = dateRangeInput(ns("date"), "Date Range:", 
                                             start = Sys.Date() - years(1), 
                                             end = Sys.Date(), 
                                             min = as.Date("1990/1/1"),
                                             max = Sys.Date(),
                                             startview = "year"),
           
           "Single Day" = selectInput(ns("date"), "Year:", 
                                        choices = levels(factor(df$Date)), 
                                        selected = factor(df$Date)[1])
           
    )
  })
  
# Reactive Data Frames for different date selection methods:
  

  df.active <- reactive({
    
    if(input$date.option == "Select Year"){
      
        df %>% 
          filter(Parameter %in% input$param) %>% 
          filter(Site %in% c(input$site)) %>% 
          filter(year(Date) == input$date)
      
    } else if (input$date.option == "Select Month"){
      df %>% 
        filter(Parameter %in% input$param) %>% 
        filter(Site %in% c(input$site)) %>% 
        filter(month(Date) == input$date)
      
    } else if (input$date.option == "Calendar Range"){
      df %>% 
        filter(Parameter %in% input$param) %>% 
        filter(Site %in% c(input$site)) %>% 
        filter(Date > input$date[1], Date < input$date[2])
      
    } else if (input$date.option == "Single Day"){
  
        df %>% 
          filter(Parameter %in% input$param) %>% 
          filter(Site %in% c(input$site)) %>% 
          filter(Date == input$date)
    }
    
  })

# Plot Creation
  
  p <- reactive({
    
    p <- ggplot(df.active(),aes(x=Result,y=Depthm)) +
      scale_y_reverse() +
      theme_bw()
    
    # The coloring and facetting organization to not allow for a user to have hidden grouped Sites, params, or Dates
    
    # No Parameter Coloring/linetype --> facet by param
    if(input$plot.color != "Parameter" & input$plot.linetype != "Parameter"){
      p <- p + geom_path(aes_string(color = input$plot.color, linetype = input$plot.linetype)) +
        facet_grid(Parameter~Date)
      # No Site Coloring/linetype --> facet by site
    } else if(input$plot.color != "Site" & input$plot.linetype != "Site"){
      p <- p + geom_path(aes_string(color = input$plot.color, linetype = input$plot.linetype)) +
        facet_grid(Site~Date)
      # Param and Site both colored or linetyped
    } else {
      p <- p + geom_path(aes_string(color = input$plot.color, linetype = input$plot.linetype)) +
        facet_grid(.~Date)
    }
  
  p
  
  })

# Plot visual
  
  output$plot <- renderPlot({
    
    p()

  })
  
# Plot visual (interactive)
  
  output$plotly <- renderPlotly({
    
    ggplotly(p() + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in")))
    
  })
  
# Plot - print
  
  output$save.plot <- downloadHandler(
    filename = function (){paste(input$date,' ', input$param,' Site ',input$site,'.png', sep='')},
    content = function(file) {ggsave(file, plot = p(), device = "png")},   #function(file) {ggsave(file, plot = p(), device = "png")}
    contentType = 'image/png'
  )
  
  
# Table
  
  output$table.dynamic <- renderDataTable(df.active())
  
  
}