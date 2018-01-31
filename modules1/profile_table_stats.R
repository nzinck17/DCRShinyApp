##############################################################################################################################
#     Title: Profile-Summary - Shiny Module
#     Description: This script will create Summary Statistics for Profile Data
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

#===========================================================================================
# UI side
prof.summary.UI <- function(id, df) {

ns <- NS(id)

tagList(
  
  wellPanel(br(),
    
    fluidRow(
      
      column(3,
             
        # Parameter Input
        selectInput(ns("param"), "Parameter:",        
                   choices=levels(factor(df$Parameter)),
                   selected = factor(df$Parameter[1]))
      ),
      
      column(3,
      
        dateRangeInput(ns("date"), "Date Range:", 
                       start = Sys.Date() - years(5), 
                       end = Sys.Date(),   #makes todays date
                       min =  min(df$Date, na.rm = TRUE),
                       max = Sys.Date(),
                       startview = "year")
      ),
      
      column(3,
             
        # Site Input
        checkboxGroupInput(ns("site"), "Site:", inline = TRUE,
                   choices = levels(factor(df$Site)), 
                   selected = factor(df$Site)[1])
      ),
      
      column(3,
             
             # Depth Input
             sliderInput(ns("depth"), "Depth:", 
                         min = 0, 
                         max = df %>% select(Depthm) %>% max() %>% ceiling(),
                         value = c(0,df %>% select(Depthm) %>% max() %>% ceiling())
             ) 
      )
      )
  
  ), # end wellPanel
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
           
           checkboxInput(ns("summary.group.site"), label = "Group by Site", value = TRUE),
           
           radioButtons(ns("summary.group.time"), "Group by Time:",
                        choices = c("None" = 1,
                                    "Year" = 2,
                                    "Season (all years)" = 3,
                                    "Month (all years)" = 4,
                                    "Season (each year)" = 5,
                                    "month (each year)" = 6),
                        selected = 1),
           
           radioButtons(ns("summary.group.depth"), "Group by Depth:",
                        choices = c("None" = 1,
                                    "1m interavals (rounded up)" = 2,
                                    "2m intervals" = 3,
                                    "5m intervals" = 4,
                                    "10m intervals" = 5),
                        selected = 1),
           h5('note: "[0m,5m)" actually means "=>0m & <5m"')
           
    ), # end sidebarPanel
  
    mainPanel(width = 9,
      
           tabsetPanel(
             
             tabPanel("Summary", 
                      tableOutput(ns("summary"))
             ),
             
             tabPanel("Table",
                        dataTableOutput(ns("table"))
             )
           ) # end tabset panel
           
    ) # end main panel
  ) # end sidebarlayout
) # end taglist
} # end UI

#=======================================================================================
# server side

prof.summary <- function(input, output, session, df) {
    
# Reactive Dataframe
  
  df.active <- reactive({
    df %>% 
      filter(Parameter %in% input$param) %>% 
      filter(Date > input$date[1], Date < input$date[2]) %>%
      filter(Site %in% c(input$site)) %>%
      filter(Depthm > input$depth[1], Depthm < input$depth[2])
  })
  
# Summary Stat
  
  output$summary <- renderTable({
    
    sum.1 <- df.active() %>%
      mutate(Year = lubridate::year(Date), 
             Season = getSeason(Date),
             Month = lubridate::month(Date),
             Depth1 = ceiling(Depthm),
             Depth2 = cut(Depthm, 
                                 breaks = seq(0,44,2), 
                                 labels = c("[0,2)", "[2,4)", "[4,6)", "[6,8)", "[8,10)", 
                                            "[10,12)", "[12,14)", "[14,16)", "[16,18)", "[18,20)",
                                            "[20,22)", "[22,24)", "[24,26)", "[26,28)", "[28,30)", 
                                            "[30,32)", "[32,34)", "[34,36)", "[36,38)", "[38,40)",
                                            "[40,42)", "[42,44)"),
                                 right = FALSE), 
             Depth5 = cut(Depthm, 
                                 breaks = seq(0,45,5), 
                                 labels = c("[0,5)", "[5,10)", "[10,15)", "[15,20)", 
                                            "[20,25)", "[25,30)", "[30,35)", "[35-40)", "[40-45)"), 
                                 right = FALSE), 
             Depth10 = cut(Depthm, 
                                  breaks = seq(0,50,10), 
                                  labels = c("[0,10)", "[10,20)", "[20,30)", "[30,40)", "[40,50)"), 
                                  right = FALSE))
    
    
    # group by time
  
    if (input$summary.group.time == 1){
      sum.dots = c()
    } else if(input$summary.group.time == 2) {
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
    
    # group by site
    if(input$summary.group.site == TRUE){
      sum.dots <- c(sum.dots, "Site")
    }     
    
    # group by depth
    if(input$summary.group.depth == 2){
      sum.dots <- c(sum.dots, "Depth1")
    } else if(input$summary.group.depth == 3){
      sum.dots <- c(sum.dots, "Depth2")
    } else if(input$summary.group.depth == 4){
      sum.dots <- c(sum.dots, "Depth5")
    } else if(input$summary.group.depth == 5){
      sum.dots <- c(sum.dots, "Depth10")
    }
    
    
    if (input$summary.group.site == FALSE & input$summary.group.time == 1 & input$summary.group.depth == 1){
      sum.2 <- sum.1
    } else {
      sum.2 <- sum.1 %>%
        group_by_(.dots = sum.dots)
    }
    
    
    sum.2 %>% summarise(average = mean(Result), 
                        min = min(Result), 
                        max = max(Result), 
                        median = median(Result), 
                        variance = var(Result), 
                        `stand.dev.` = sd(Result),
                        `number of samples` = n())
  }) # end summary
  

# Table
  
  output$table <- renderDataTable(df.active())
  
  
} # end server

