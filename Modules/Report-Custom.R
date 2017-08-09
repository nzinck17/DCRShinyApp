##############################################################################################################################
#     Title: Report-Custom.R
#     Type: Module for DCR Shiny App
#     Description: Report Generation via RMarkdown and knitR
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. 
#
# To-Do List:
#   1. Make Plot and Table Modules (second level)

##############################################################################################################################
# User Interface
##############################################################################################################################

report.custom.UI <- function(id, df) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    # INSERT
    wellPanel(
      strong("Insert Report Element"),
      fluidRow(
        actionButton(ns("insert.text.s"), "Insert Text (short)"),
        actionButton(ns("insert.text.l"), "Insert Text (long)"),
        actionButton(ns("insert.plot"), "Insert Plot"),
        actionButton(ns("insert.table"), "Insert Table"),
        actionButton(ns("insert.image"), "Insert Image")
      )
    ),
    # REMOVE
    wellPanel(
      uiOutput(ns("remove.choice.ui")),
      actionButton(ns("remove.action"), "Remove Selected Element")
    ),
    # DOWNLOAD
    wellPanel(    
      downloadButton(ns("report"), "Generate report")
    ),
    tags$div(id = 'placeholder')
  ) # end Taglist
} # end UI Function
  

##############################################################################################################################
# Server Function
##############################################################################################################################

report.custom <- function(input, output, session, df, df.site) { 
  
# Initialize a vector of inserted elements
  
  values <- reactiveValues(elements = NULL)

  
# Remove Element UI
  
  output$remove.choice.ui <- renderUI({
    
    if(length(values$elements != 0)){
      radioButtons(session$ns("remove.choice"), "Remove Report Elements", choices = values$elements)
    }
    
  })
  
  
# Text (short) Input
  observeEvent(input$insert.text.s, {

    num.text.s <- input$insert.text.s
    id <- paste("textsUI", num.text.s, sep = "")
    
    insertUI(
      selector = "#placeholder",
      where = "beforeEnd",
      ui = tags$div(
        h4(paste("Text (Short) Input", num.text.s)), 
        wellPanel(
          textInput(session$ns(paste("text.s", num.text.s)), "")
        ),
        id = id
      )
    )
    
    values$elements <- c(values$elements, id)
    
  })
  
  
# Text (Long) Insert
  
  observeEvent(input$insert.text.l, {
    
    num.text.l <- input$insert.text.l
    id <- paste("textlUI", num.text.l, sep = "")
    
    insertUI(
      selector = "#placeholder",
      where = "beforeEnd",
      ui = tags$div(
        h4(paste("Text (Long) Input", num.text.l)), 
        wellPanel(
          textAreaInput(session$ns(paste("text.l", num.text.l)), "", cols = 8)
        ),
        id = id
      )
    )
    
    values$elements <- c(values$elements, id)
    
  })
  
  
# Plot Insert
  
  observeEvent(input$insert.plot, {
    
    num.plot <- input$insert.plot
    id <- paste("plot", num.plot, sep = "")
    
    # Most likely shoulf make this a Module inside a module
    insertUI(
      selector = "#placeholder",
      where = "beforeEnd",
      ui = tags$div(
        h4(paste("Plot", num.plot)),
        wellPanel(
          fluidRow(
            column(3,
                   checkboxGroupInput(session$ns("site.plot"), "Sites: (Choose 1st)", 
                                      choices= levels(factor(df$Site)),
                                      selected = factor(df$Site[1]),
                                      inline=TRUE)
            ),
            #column(1),
            column(2,
                   selectInput(session$ns("param.plot"), "Water Quality Parameter:",        
                               choices=levels(factor(df$Parameter)),
                               selected = factor(df$Parameter[4]))
            ),
            #column(1),
            column(2,
                   dateRangeInput(session$ns("date.plot"), "Date Range:", 
                                  start = min(df$Date, na.rm = TRUE), 
                                  end = max(df$Date, na.rm = TRUE),
                                  min = min(df$Date, na.rm = TRUE),
                                  max = max(df$Date, na.rm = TRUE),
                                  startview = "year")
            )
          )
        ),
        id = id
        )
    )
    
    values$elements <- c(values$elements, id)
    
  })
 
  
# Table Insert
  
  observeEvent(input$insert.table, {

    num.table <- input$insert.table
    id <- paste("table", num.table)
    
    insertUI(
      selector = "#placeholder",
      where = "beforeEnd",
      ui = tags$div(
        h4(paste("Table", num.table)), 
        wellPanel(
          fluidRow(
            column(3,
                   checkboxGroupInput(session$ns("site.table"), "Sites: (Choose 1st)", 
                                      choices= levels(factor(df$Site)),
                                      selected = factor(df$Site[1]),
                                      inline=TRUE)
            ),
            #column(1),
            column(2,
                   checkboxGroupInput(session$ns("param.table"), "Water Quality Parameter:",        
                                      choices=levels(factor(df$Parameter)),
                                      selected = factor(df$Parameter[4]),
                                      inline=TRUE)
            ),
            #column(1),
            column(2,
                   dateRangeInput(session$ns("date.table"), "Date Range:", 
                                  start = min(df$Date, na.rm = TRUE), 
                                  end = max(df$Date, na.rm = TRUE),
                                  min = min(df$Date, na.rm = TRUE),
                                  max = max(df$Date, na.rm = TRUE),
                                  startview = "year")
            )
          )
        ),
        id = id
      )
    )
    
    values$elements <- c(values$elements, id)
    
  })
  
# Image Insert
  observeEvent(input$insert.image, {
    
    num.image <- input$insert.image
    id <- paste("image", num.image)
    
    insertUI(
      selector = "#placeholder",
      where = "beforeEnd",
      ui = tags$div(
        h4(paste("Image", num.image)), 
        id = id)
    )
    
    values$elements <- c(values$elements, id)
    
  })


  
# Remove Report Elements
  
  observeEvent(input$remove.action, {
    removeUI(
      selector = paste("#", input$remove.choice, sep = "")
    )
    
    values$elements <- values$elements[!values$elements %in% input$remove.choice]
    
  })

  
# Output and Save Report
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      
    } # end content funciton
  ) # end Download Handler
  
} # end Server Function