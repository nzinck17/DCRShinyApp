##############################################################################################################################
#     Title: Tributary-Regression.R
#     Type: Module for DCR Shiny App
#     Description: Regression plots and tables
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

CORRELATION_WQ_UI <- function(id) {

ns <- NS(id)

tagList(
         wellPanel(
           fluidRow(
             column(3,
                    # SITE
                    wellPanel(
                      uiOutput(ns("site_primary_ui"))
                    ),
                    wellPanel(
                      uiOutput(ns("site_nonprim_cat_ui")),
                      uiOutput(ns("site_nonprim_ui"))
                    ) # end Well Panel
             ), # end column
             column(3,
                    # TEXT
                    wellPanel(
                      h2(textOutput(ns("text_num_null")), align = "center"),
                      h4(textOutput(ns("text_num_text")), align = "center"),
                      h3(textOutput(ns("text_num")), align = "center")
                    ), # end Well Panel
                    # DATE SELECTION
                    wellPanel(
                      uiOutput(ns("date_ui")) 
                    ), # end Well Panel
                    # MAP
                    wellPanel(
                      SITE_MAP_UI(ns("Site Map"))
                    ) # end Well Panel
             ),# end Column
             column(6,
                    fluidRow(
                      column(6,
                             # Y Parameter Selection
                             wellPanel(
                               uiOutput(ns("y_param_ui")),
                               uiOutput(ns("y_range_ui"))
                             ) # well
                      ), # end Column
                      column(6,
                             wellPanel(
                               # X Parameter Selection
                               strong("X axis Parameter:"),
                               radioButtons(ns("x_option"), label = NULL, choices = c("Water Quality", "Meteorology or Hydrology"), inline = TRUE),
                               #See General Note 2
                               conditionalPanel(condition = paste0("input['", ns("x_option"), "'] == 'Water Quality' "),
                                                uiOutput(ns("x_param_ui")),
                                                uiOutput(ns("x_range_ui"))
                               ),# end Conditional Panel
                               conditionalPanel(condition = paste0("input['", ns("x_option"), "'] == 'Meteorology or Hydrology' "),
                                                selectInput(ns("x_met_param"), label = NULL, choices = c("Wind Speed", 
                                                                                                         "Wind Direction", 
                                                                                                         "Precipitation - 24 hrs",
                                                                                                         "Precipitation - 48 hrs",
                                                                                                         "Temperature",
                                                                                                         "Cloud Cover",
                                                                                                         "Flow - Quabbin Aquaduct",
                                                                                                         "Flow - East Branch Swift",
                                                                                                         "Flow - West Branch Swift",
                                                                                                         "Flow - Quinapoxet",
                                                                                                         "Flow - Stillwater"),
                                                            selected = "Precipitation - 24 hrs"),
                                                sliderInput(ns("x_met_range"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                               )# end Conditional Panel
                             )# end Well Panel
                      )
                    )# end fluid row
             )# end Column
           )# end Fluid Row
         ), # end Well panel
         # Tabset Panel for plots and tables 
         tabsetPanel(
           
           # the "Plot" tab panel
           tabPanel("Plot", 
                    PLOT_CORR_WQ_UI(ns("Plot"))
           ), # end "plot" tabpanel
           
           # Table tabpanel
           tabPanel("Table",
                    # first row - print button, etc
                    fluidRow(
                      br(),
                      actionButton(ns("table_print"), "Print Table")
                    ),
                    # next row
                    fluidRow(
                      br(), br(),
                      dataTableOutput(ns("table"))
                    ) # end fluid row
           ) # end Tab panel - Table
         )  # end tabsetpanel - Plot and Table
) # end Taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

CORRELATION_WQ <- function(input, output, session, df, df_site) {

# Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6
  
  parameters_non_historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()
  
  
  # Site primary
  
  output$site_primary_ui <- renderUI({
    
    ns <- session$ns # see General Note 1
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    site_primary <- df %>%
      filter(LocationCategory == "Primary Active") %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
    
    # Check box input
    checkboxGroupInput(ns("site_primary"), "Primary Active Sites:",
                       choices = site_primary)
    
    
    
  })  
  
  # Site Categories UI (RendeUI becuase move Primary Active to front)
  
  output$site_nonprim_cat_ui <- renderUI({
    
    ns <- session$ns # see General Note 1
    
    # Change LocationCateogory NA to "NA" to show up in App
    df$LocationCategory <- as.character(df$LocationCategory)
    df$LocationCategory[is.na(df$LocationCategory)] <- "NA"
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    site_categories <- df %>%
      filter(LocationCategory != "Primary Active") %>%
      .$LocationCategory %>%
      factor() %>%
      levels()
    
    # Site Categories
    checkboxGroupInput(ns("site_nonprim_cat"), "Show Other Categories:",
                       choices = site_categories)
    
  })
  
  
  
  # Site Non Primary
  output$site_nonprim_ui <- renderUI({
    
    req(input$site_nonprim_cat) # See General Note 5
    
    ns <- session$ns # see General Note 1
    
    site_select <- df %>%
      filter(LocationCategory %in% input$site_nonprim_cat) %>%
      .$LocationLabel %>%
      factor() %>%
      levels()
    
    # Sites
    checkboxGroupInput(ns("site_nonprim"), "Sites:",
                       choices = site_select)
    
  })
  
  
  # Combine Site Input
  
  site_list <- reactive({
    c(input$site_primary, input$site_nonprim)
  })
  
  
# Y axis Parameter
  
  #Parameter Selection UI
  
  output$y_param_ui <- renderUI({
    
    req(site_list()) # See General Note _
    
    ns <- session$ns
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    y_param_choices_new <- df %>%
      filter(LocationLabel %in% c(site_list())) %>%
      filter(Parameter %in% parameters_non_historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    y_param_choices_old <- df %>%
      filter(LocationLabel %in% c(site_list())) %>%
      filter(!(Parameter %in% parameters_non_historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    y_param_choices <- c(y_param_choices_new, y_param_choices_old)
    
    selectInput(ns("y_param"), "Y-axis Parameter:",        
                choices=c(y_param_choices))

  })
  
  
  # Reactive Texts
  
  y_param_units <- reactive({ 
    
    req(site_list()) # See General Note _
    
    df %>%
      filter(Parameter %in% input$y_param) %>%
      .$Units %>%
      factor() %>%
      levels()
    
  })
  
  
  #Parameter Value Range UI
  
  output$y_range_ui <- renderUI({
    
    req(site_list()) # See General Note _
    
    ns <- session$ns
    
    y_result <- df %>%
      filter(LocationLabel %in% c(site_list())) %>%
      filter(Parameter %in% input$y_param) %>%
      .$Result
    
    y_param_min <- y_result %>% min(na.rm = TRUE)
    
    y_param_max <- y_result %>% max(na.rm = TRUE)
    
    sliderInput(ns("y_range"), paste("Range (", y_param_units() ,")"), 
                min = y_param_min, max = y_param_max,
                value = c(y_param_min, y_param_max))
    
  })
  

# X axis parameter 
  
  #Parameter Selection UI
  
  output$x_param_ui <- renderUI({
    
    req(site_list()) # See General Note _
    
    ns <- session$ns
    
    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    x_param_choices_new <- df %>%
      filter(LocationLabel %in% c(site_list())) %>%
      filter(Parameter %in% parameters_non_historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    x_param_choices_old <- df %>%
      filter(LocationLabel %in% c(site_list())) %>%
      filter(!(Parameter %in% parameters_non_historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()
    
    # Recent Parameters first and then old parameters
    x_param_choices <- c(x_param_choices_new, x_param_choices_old)
    
    selectInput(ns("x_param"), "X-axis Parameter:",        
                choices=c(x_param_choices))
    
  })
  
  
  # Reactive Texts
  
  x_param_units <- reactive({ 
    
    req(site_list()) # See General Note _
    
    df %>%
      filter(Parameter %in% input$x_param) %>%
      .$Units %>%
      factor() %>%
      levels()
    
  })
  
  
  #Parameter Value Range UI
  output$x_range_ui <- renderUI({
    
    req(site_list()) # See General Note _
    
    ns <- session$ns
    
    x_result <- df %>%
      filter(LocationLabel %in% c(site_list())) %>%
      filter(Parameter %in% input$x_param) %>%
      .$Result
    
    x_param_min <- x_result %>% min(na.rm = TRUE)
    
    x_param_max <- x_result %>% max(na.rm = TRUE)
    
    sliderInput(ns("x_range"), paste("Range (", x_param_units() ,")"), 
                min = x_param_min, max = x_param_max,
                value = c(x_param_min, x_param_max))
    
  })

  
# Date Selection UI
  
  output$date_ui <- renderUI({
    
    req(site_list()) # See General Note _
    
    ns <- session$ns
    
    Dates <- df %>% 
      filter(LocationLabel %in% c(site_list())) %>%
      .$Date
    
    Date_min <- Dates %>% min(na.rm = TRUE)
    Date_max <- Dates %>% max(na.rm = TRUE)
    
    dateRangeInput(ns("date"), "Date Range:", 
                   start = Date_min, 
                   end = Date_max,
                   min = Date_min,
                   max = Date_max,
                   startview = "year")
    
  })
  
  
# Reactive Dataframe
  
  Df2 <- reactive({
    
    req(site_list(), input$y_param, input$y_range, input$x_param, input$x_range, input$date) # See General Note _
    
    # filter by Site and Date adn save
    df_temp <- df %>% 
      filter(LocationLabel %in% c(site_list()),
             Date > input$date[1], Date < input$date[2])
    
    # X Parameter filter and make modifications
    df_temp_x <-  df_temp %>% 
      filter(Parameter %in% c(input$x_param),
             Result > input$x_range[1], Result < input$x_range[2]) %>%
      rename(x_Parameter = Parameter, x_Result = Result) %>%
      select(Site, Date, x_Parameter, x_Result)
    
    # Y Parameter filter and make modifications
    df_temp_y <-  df_temp %>% 
      filter(Parameter %in% c(input$y_param),
             Result > input$y_range[1], Result < input$y_range[2]) %>%
      rename(y_Parameter = Parameter, y_Result = Result) %>%
      select(Site, Date, y_Parameter, y_Result)
    
    # Join the two X and Y parameters dataframes
    inner_join(df_temp_x, df_temp_y, by = c("Site", "Date"))
    
  })
  
  
  # Text - Select Site - Red
  
  output$text_num_null <- renderText({
    req(is.null(site_list())) # See General Note 1
    "Select a Site"
  })
  
  # Text - Number of Samples
  
  output$text_num_text <- renderText({
    req(site_list()) # See General Note 1
    "Number of Samples in Selected Data"
  })
  
  # Text - Number of Samples
  
  output$text_num <- renderText({
    req(Df2()) # See General Note 1
    Df2() %>% summarise(n()) %>% paste()
  })
  
  # Plot
  
  callModule(PLOT_CORR_WQ, "Plot", Df = Df2)
  
  # Tables
  
  output$table <- renderDataTable(Df2())
  
  # Site Map
  
  callModule(SITE_MAP, "Site Map", df_site = df_site, Site_List = site_list)
  
} # end Server Function

