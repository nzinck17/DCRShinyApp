##############################################################################################################################
#     Title: ProfileHeatmapFilledContour - Shiny Module
#     Description: This script will plot an annual timeseries profile for one parameter for one calendar year
#                 using the filled contour method
#     Written by: Dan Crocker and Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

# load libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(akima)

#=========================================================================
# UI side


####IIIMMMMPOORTANT 
# change the df.profile to df


prof.heatmap.UI <- function(id, df) {
  
ns <- NS(id)
  
tagList(
    
  wellPanel(
    
    fluidRow(
      
      # first column
      column(3,
             
             # Parameter Input
             selectInput(ns("param"), "Parameter:",        
                         choices=levels(factor(df$Parameter)),
                         selected = factor(df$Parameter[1]))
      ),
      
      
      column(2,
             
             # Date Input
             selectInput(ns("date"), "Year:", 
                        choices = year(seq(as.Date("1990/1/1"), Sys.Date(), "years")), 
                        selected = year(Sys.Date() - years(5)))
      ),
      
      
      column(2,
             
             # Site Input
             selectInput(ns("site"), "Site:", 
                         choices = levels(factor(df$Site)), 
                         selected = factor(df$Site)[1])
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
      
      tabPanel("Table",
              dataTableOutput(ns("table"))
      )
    )
    
) # end taglist

} # end UI

#=============================================================================
# server side


prof.heatmap <- function(input, output, session, df) {
  
# Reactive Data Frames:
  
  df.active <- reactive({
    df %>% 
    filter(Parameter %in% input$param) %>% 
    filter(Site %in% c(input$site)) %>%
    filter(year(Date) == input$date) #%>%
    #filter(!is.na(Date)) %>%
    #filter(!is.na(Depthm)) %>%
    #filter(!is.na(Result))
  })
  
# Reactive Data for color scale use
  
  df.limit <- reactive({df %>% filter(Parameter %in% input$param) %>% filter(!is.na(Result))})
  lowerlimit <- reactive({min(df.limit()$Result)})
  upperlimit <- reactive({max(df.limit()$Result)})
  midpoint <- reactive({(lowerlimit() + upperlimit()) / 2})
  
# Plot creation
  
  p <- reactive({
    
    # if interploation is selected
    if(input$interp != "none"){
      df.plot <- akima::interp(x = decimal_date(df.active()$Date), y = df.active()$Depthm, z = df.active()$Result, duplicate="strip", nx = 100, ny = 100)
      df.plot <- interp2xyz(df.plot, data.frame=TRUE)
      df.plot <- rename(df.plot, Date = x, Depthm = y, Result = z)
    } else {
      df.plot <- df.active()
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
  
# Plot - print
  
  output$save.plot <- downloadHandler(
    filename = function (){paste(input$date,' ', input$param,' Site ',input$site,'.png', sep='')},
    content = function(file) {ggsave(file, plot = p(), device = "png")},   #function(file) {ggsave(file, plot = p(), device = "png")}
    contentType = 'image/png'
  )
  
# Table
  
  output$table <- renderDataTable(df.active())
  
} # end server
