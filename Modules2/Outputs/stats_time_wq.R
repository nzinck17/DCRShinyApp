##############################################################################################################################
#     Title: stats_time_wq.R
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

summary.UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    fluidRow(
      column(3,
             checkboxInput(ns("summary.group.site"), label = "Group by Site", value = TRUE),
             radioButtons(ns("summary.group.time"), "Temporal Groupings:",
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

summary <- function(input, output, session, df) {
  
# Summary Statistics
  
  output$summary <- renderTable({
    
    # Add Year, season, and Month Columns
    sum.1 <- df() %>%
      mutate(Year = lubridate::year(Date), 
             Season = getSeason(Date),
             Month = lubridate::month(Date)
      )
    
    # Group by time (year, season, month)
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
    
    # Group by site
    if(input$summary.group.site == TRUE){
      sum.dots <- c(sum.dots, "Site")
    }  
    
    # Group by Param
    sum.dots <- c("Parameter", sum.dots)
    
    # Applying Grouping (if Grouping selected)
    if (input$summary.group.site == FALSE & input$summary.group.time == 1){
      sum.2 <- sum.1
    } else {
      sum.2 <- sum.1 %>%
        group_by_(.dots = sum.dots)
    }
    
    # Making the Sumamry Statistic Columns
    sum.2 %>% summarise(average = mean(Result), 
                        min = min(Result, na.rm=TRUE), 
                        max = max(Result, na.rm=TRUE), 
                        median = median(Result, na.rm=TRUE), 
                        variance = var(Result, na.rm=TRUE), 
                        `stand.dev.` = sd(Result, na.rm=TRUE),
                        `number of samples` = n())
  })
  
  

} # end Server Function

