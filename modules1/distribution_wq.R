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
                     choices = c("Histogram", "Density Curve", "Box Plot"),
                     inline = TRUE),
        strong("Outliers"),
        checkboxInput(ns("rm_outliers"), "Remove Outliers"),
        fluidRow(
          column(6,
                 radioButtons(ns("color"), "group by color",
                              choices = c("None" = "None",
                                          "Site" = "LocationLabel", 
                                          "Year", 
                                          "Month",
                                          "Season"))
          ),
          column(6,
                 radioButtons(ns("facet"), "group by facet",
                              choices = c("None" = "None",
                                          "Site" = "LocationLabel", 
                                          "Year", 
                                          "Month",
                                          "Season"))
                 )
        ),
        uiOutput(ns("histo_ui")),
        strong(textOutput(ns("text_box_plot")))
      ),
      mainPanel(
        h4(textOutput(ns("text_plot_no_data"))),
        h4(textOutput(ns("text_plot_zero_data"))),
        plotOutput(ns("plot"))
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
  
  
  # Parameter Selection using ParameterSelect Module
  Param <- callModule(PARAM_SELECT, "param", Df = Df2, Site = Site, multiple = FALSE)
  
  
  # Date Range Selection Using DateSelect Module
  Date_Range <- callModule(DATE_SELECT, "date", Df = Df2, Site = Site)
  
  
  
  # Reactive Dataframe - filter for param, value range, date, and remove rows with NA for Result
  Df3 <- reactive({
    req(Param$Type(), Param$Range_Min(), Param$Range_Min(), Date_Range$Lower(), Date_Range$Upper()) # See General Note _
    
    df <- Df2() %>% 
      filter(Parameter %in% Param$Type()[1], 
             Result > Param$Range_Min(), Result < Param$Range_Max(),
             Date > Date_Range$Lower(), Date < Date_Range$Upper()) %>%
      mutate(Year = factor(lubridate::year(Date)), 
             Season = getSeason(Date),
             Month = month.abb[lubridate::month(Date)])
    
    if(input$rm_outliers == TRUE){
      df %>% mutate(Result = remove_outliers(Result))
    }else{
      df
    }
  })
  
  

  
  # render UI
  output$histo_ui <- renderUI({
    
    if(input$plot_type == "Histogram"){
    # min(Df3()$Result, na.rm = TRUE):max(Df3()$Result, na.rm = TRUE)
    slide_vec <- pretty(Df3()$Result, n = 20)
    interval <- (max(slide_vec) - min(slide_vec)) / (length(slide_vec)-1)
    digit <- round(log10(interval))*-1 + 2
    slide_val <- round(interval, digit)*2
    slide_min_step <- slide_val/4
    slide_max <- slide_val*2
    
    
    tagList(
      hr(),
      sliderInput(ns("bin_size"), "Bin Size (Hstogram)",
                  min = slide_min_step, max = slide_max,
                  value = slide_val, step = slide_min_step),
      radioButtons(ns("position"), "Position Type (Histogram)",
                   choices = c("stack", "identity", "dodge"),
                   inline = TRUE)
    )
    } else{
      
    }
    
  })
  
  
  # Plot Output
  output$plot <- renderPlot({
    P()
  })

  # Plot
  P <- reactive({
    
    p <- ggplot(Df3())
    
    # Histogram
    if(input$plot_type == "Histogram"){
      if(input$color == "None"){
        p <- p + geom_histogram(aes(x = Result), size = 1.0, binwidth = input$bin_size, position = input$position)
      } else{
        if(input$position == "identity"){
        p <-  p + geom_histogram(aes_string(x = "Result", color = input$color),
                                 size =1.0,  binwidth = input$bin_size, position = input$position)
        } else{
          p <-  p + geom_histogram(aes_string(x = "Result", fill = input$color, color = input$color),
                                   alpha = 0.1, size =1.0,  binwidth = input$bin_size, position = input$position)
        }
        
      }
    # Density Curve
    } else if(input$plot_type == "Density Curve"){
      if(input$color == "None"){
        p <-  p + geom_density(aes(x = Result), size = 1.0)
      } else{
        p <-  p + geom_density(aes_string(x = "Result", color = input$color), 
                               alpha = 0.1, size =1.0)
      }
    # Box Plot
    } else if(input$plot_type == "Box Plot"){
      if(input$color == "None"){
        p <-   p + geom_boxplot(aes(x = Parameter, y = Result), 
                                alpha = 0.5, size =1.0, width = 0.8, position = position_dodge(1)) +
          coord_flip()
      } else{
        p <- p + geom_boxplot(aes_string(x = "Parameter", y = "Result", fill = input$color, color = input$color), 
                              alpha = 0.5, size =1.0, width = 0.8, position = position_dodge(1)) + 
          coord_flip()
      }
    }
    
    # Facet Wrap
    if(input$facet != "None"){
      p <- p + facet_wrap(as.formula(paste("~", input$facet)))
    }
    
    p <- p + xlab(paste(Param$Type(), " (", Text_Units_X(),")", sep= ""))
    
    p
    
  })
  
  
  Text_Units_X <- reactive({Df3() %>% filter(Parameter == Param$Type()) %>% .$Units %>% unique() %>% paste(collapse = ", ")})
  
  
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
  
  # Text - Plot - Zero data Selected for Plot
  output$text_box_plot <- renderText({
    req(input$plot_type == "Box Plot") # See General Note 1
    "Caution should be taken when there is a small number of observations represented by a box plot"
  })
  
  
  
} # end Server Function


