##############################################################################################################################
#     Title: Tributary-Regression.R
#     Type: Module for DCR Shiny App
#     Description: Regression plots and tables
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   

##############################################################################################################################
# User Interface
##############################################################################################################################

DISTRIBUTION_WQ_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    wellPanel(       
      fluidRow(
        column(3,
               radioButtons(ns("df_choice"), "Full or Filtered Data:", 
                            choices = c("full", "filtered"),
                            inline = TRUE),
               p(textOutput(ns("text_filtered"))),
               wellPanel(
                 h4(textOutput(ns("text_site_null")), align = "center"),
                 h4(textOutput(ns("text_param_null")), align = "center"),
                 h4(textOutput(ns("text_date_null")), align = "center"),
                 h5(textOutput(ns("text_num_text")), align = "center"),
                 strong(textOutput(ns("text_num")), align = "center")
               ), # end Well Panel
               wellPanel(
                 SITE_MAP_UI(ns("site_map"))
               ) # end Well Panel
        ), # end Column
        column(6,
               SITE_CHECKBOX_UI(ns("site"))
        ), # end Column
        column(3,
               PARAM_SELECT_UI(ns("param")),
               br(),
               DATE_SELECT_UI(ns("date"))
        ) # end column
      ) # end fluidrow     
    ), # end well panel
    
    sidebarLayout(
      sidebarPanel(
        radioButtons(ns("plot_type"), "Plot Type",
                     choices = c("Histogram", "Density Curve", "Box Plot")),
        uiOutput(ns("histo_ui")),
        checkboxInput(ns("rm_outliers"), "Remove Outliers"),
        radioButtons(ns("color_ui"), "group by color",
                     choices = c("None" = "None",
                    "Site" = "LocationLabel", 
                    "Year", 
                    "Month",
                    "Season")),
        radioButtons(ns("facet_ui"), "group by facet",
                     choices = c("None" = "None",
                                 "Site" = "LocationLabel", 
                                 "Year", 
                                 "Month",
                                 "Season"))
      ),
      mainPanel(
        h4(textOutput(ns("text_plot_no_data"))),
        h4(textOutput(ns("text_plot_zero_data"))),
        plotlyOutput(ns("plot"))
      )
    )
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

DISTRIBUTION_WQ <- function(input, output, session, df_full, Df_Filtered, df_site) {
  
  # move to functions
  remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
  }
  
  
  ns <- session$ns # see General Note 1
  
  # Dataframe filtered or full based on Selection
  Df1 <- reactive({
    if(input$df_choice == "filtered"){
      Df_Filtered()
    }else{
      df_full
    }
  })
  
  
  # Site Selection using Site Select Module
  Site <- callModule(SITE_CHECKBOX, "site", Df = Df1)
  
  
  # Reactive Dataframe - first filter of the dataframe for Site
  Df2 <- reactive({
    req(Site())
    
    Df1() %>% 
      filter(LocationLabel %in% Site(),
             !is.na(Result)) # can remove this once certain no NAs. Maybe remove for Rdata files
  })
  
  
  # Date Range Selection Using DateSelect Module
  Date_Range <- callModule(DATE_SELECT, "date", Df = Df2, Site = Site)
  
  
  
  # Reactive Dataframe - filter for param, value range, date, and remove rows with NA for Result
  Df3 <- reactive({
    req(Param$Type(), Param$Range_Min(), Param$Range_Min(), Date_Range$Lower(), Date_Range$Upper()) # See General Note _
    
    Df2() %>% 
      filter(Parameter %in% Param$Type()[1], 
             Result > Param$Range_Min(), Result < Param$Range_Max(),
             Date > Date_Range$Lower(), Date < Date_Range$Upper())
  })
  
  
  # Plot
  renderPlotly(
    plotly(P())
  )

  P <- reactive({
    
    
    df <- Df3() %>% mutate(Result = remove_outliers(df5$Result))
    
    # Histogram
    if(input$plot_type == "Histogram"){
      if(input$color == "None"){
        p <-   ggplot(df5, aes()) +
          geom_histogram(data = df5, aes(x = Result), alpha = 0.1, size =1.5)
      } else{
        p <-   ggplot(df5, aes()) +
          geom_histogram(data = df5, aes_string(x = "Result", fill = input$color, color = input$color), alpha = 0.1, size =1.5)
      }
      # Density Curve
    } else if(input$plot_type == "Density Curve"){
      if(input$color == "None"){
        p <-   ggplot(df5, aes()) +
          geom_density(data = df5, aes(x = Result), alpha = 0.1, size =1.5)
      } else{
        p <-   ggplot(df5, aes()) +
          geom_density(data = df5, aes_string(x = "Result", fill = input$color, color = input$color), alpha = 0.1, size =1.5)
      }
      # Box Plot
    } else if(input$plot_type == "Box Plot"){
      if(input$color == "None"){
        p <-   ggplot(df5, aes()) +
          geom_density(data = df5, aes(x = Result), alpha = 0.1, size =1.5)
      } else{
        p <-   ggplot(df5, aes()) +
          geom_density(data = df5, aes_string(x = input$color, y = "Result", fill = input$color, color = input$color), alpha = 0.1, size =1.5)
      }
    }
    
    if(input$facet != "None")
      p <- p + facet_wrap(~input$facet)
    
  })
  
  
  
  # Site Map
  callModule(SITE_MAP, "site_map", df_site = df_site, Site_List = Site)
  
  
 
  ################ Texts ###################################
  
  # Text - Filtered Data
  output$text_filtered <- renderText({
    req(input$df_choice == "filtered") # See General Note 1
    'This Dataset has been filtered and therefore some observations (data points) may be excluded.\n See "Filtered tab"'
  })
  
  # Text - Select Site
  output$text_site_null <- renderText({
    req(!isTruthy(Site())) # See General Note 1
    "Select Site(s)"
  })
  
  # Text - Select Param
  output$text_param_null <- renderText({
    req(!isTruthy(Param$Type())) # See General Note 1
    "Select Parameter"
  })
  
  # Text - Select Param
  output$text_date_null <- renderText({
    req(!isTruthy(Date_Range$Lower()) | !isTruthy(Date_Range$Upper())) # See General Note 1
    "Select Lower Date Range"
  })
  
  # Text - Number of Samples - Words
  output$text_num_text <- renderText({
    req(Df3()) # See General Note 1
    "Number of Samples in Selected Data:"
  })
  
  # Text - Number of Samples - Number
  output$text_num <- renderText({
    req(Df3()) # See General Note 1
    Df3() %>% summarise(n()) %>% paste()
  })
  
  # Text - Plot- No data Selected
  output$text_plot_no_data <- renderText({
    req(!isTruthy(Site()) | !isTruthy(Param$Type()) | !isTruthy(Date_Range$Lower()) | !isTruthy(Date_Range$Upper()))
    "Select Site, Param, and Date"
  })
  
  # Text - Plot - Zero data Selected for Plot
  output$text_plot_zero_data <- renderText({
    req(Df3()) # See General Note 1
    if(Df3() %>% summarise(n()) %>% unlist() == 0){
      "Selection Contains Zero Observations"
    }
  })
  
  
  
} # end Server Function



# Three Types of Plots - histo (count), density, box plot
# Histo Only - Number of bins adn position
# remove outliers
# parameter range
# Color by site, season, year, month
# Facet, Site, season, year, month - some caet wrap / nrow /4


