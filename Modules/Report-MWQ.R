##############################################################################################################################
#     Title: Report-MWQ.R
#     Type: Module for DCR Shiny App
#     Description: Report Generation via RMarkdown adn knitR
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. 
#
# To-Do List:
#   1. 

##############################################################################################################################
# User Interface
##############################################################################################################################

report.mwq.UI <- function(id, df) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    
    # Date Selection
    strong("Date Range:"), #mean bold text
    br(), br(),
    wellPanel(
      # Year
      selectInput(ns("year"), "Year:", 
                  choices = c(rev(year(seq(as.Date("1990-1-1"), Sys.Date(), "years")))), 
                  selected =  year(Sys.Date())),
      # Month
      selectInput(ns("month"), "Month:", 
                  choices = c("All Months",
                              January = 1,
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
                  selected = "All Months"),
      textInput(ns("mwq.text1"), "Section 1 text"),
      textInput(ns("mwq.text2"), "Section 2 text"),
      textInput(ns("mwq.text3"), "Section 3 text"),
      textInput(ns("mwq.text4"), "Conclusion text")
    ), # end Well Panel
    downloadButton(ns("report"), "Generate report")
  ) # end Taglist
} # end UI Function
  

##############################################################################################################################
# Server Function
##############################################################################################################################

report.mwq <- function(input, output, session, df.trib, df.res, df.profile, df.site) {  
  
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
