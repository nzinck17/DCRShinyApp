##############################################################################################################################
#     Title: stats_time_depth_wq.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Summary Stats with Grouping
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

summary.depth.UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    fluidRow(
      column(3,
             checkboxInput(ns("summary.group.station"), label = "Group by Station", value = TRUE),
             checkboxInput(ns("summary.group.level"), label = "Group by Level", value = TRUE),
             radioButtons(ns("summary.group.time"), "Group by:",
                          choices = c("None" = 1,
                                      "Year" = 2,
                                      "Season (all years)" = 3,
                                      "Month (all years)" = 4,
                                      "Season (each year)" = 5,
                                      "month (each year)" = 6),
                          selected = 1)
      ),
      column(9,
             tableOutput(ns("summary"))
      ) # end column
    )
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

summary.depth <- function(input, output, session, df) {
  
  output$summary <- renderTable({
    
    sum.1 <- df() %>%
      mutate(Year = as.integer(lubridate::year(Date)), 
             Season = getSeason(Date),
             Month = month.abb[lubridate::month(Date)]
      )
    
    # group by time
    if (input$summary.group.time == 1){
      sum.dots = c()
    } else if (input$summary.group.time == 2) {
      sum.dots = c("Year")
    } else if (input$summary.group.time == 3) {
      sum.dots = c("Season")
    } else if (input$summary.group.time == 4) {
      sum.dots = c("Month")
    } else if (input$summary.group.time == 5) {
      sum.dots = c("Year", "Season")
    } else if (input$summary.group.time == 6) {
      sum.dots = c("Year", "Month")
    }
    
    # group by Location
    if(input$summary.group.station == TRUE){
      sum.dots <- c(sum.dots, "Station")
    }
    
    # group by Depth
    if(input$summary.group.level == TRUE){
      sum.dots <- c(sum.dots, "Sampling_Level")
    }
      
    # Group by Param
    sum.dots <- c("Parameter", sum.dots)
    
    # Applying Grouping (stats is always grouped by parameter)
    sum.2 <- sum.1 %>% group_by_(.dots = sum.dots)
    
    # Making the Sumamry Statistic Columns
    sum.2 %>% summarise(`number of samples` = n(),
                        average = mean(Result), 
                        min = min(Result, na.rm=TRUE), 
                        max = max(Result, na.rm=TRUE), 
                        `1st quartile` = quantile(Result, 0.25),
                        median = median(Result),
                        `3rd quartile` = quantile(Result, 0.75),
                        variance = var(Result, na.rm=TRUE), 
                        `stand.dev.` = sd(Result, na.rm=TRUE),
                        `geometric mean` = gm_mean(Result))
  })

} # end Server Function

