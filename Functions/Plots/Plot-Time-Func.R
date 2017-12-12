##############################################################################################################################
#     Title: Plot-Time-Func.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Time Series plot (for non-depth dependent data)
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:
#   1. Make the Metero/Hydro Filters work
#   2. Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)
#   3. 1 to None (in shape and color )

##############################################################################################################################
# User Interface
##############################################################################################################################


plot.time.func <- function(df, 
                           plot.display.theme = input$plot.display.theme, 
                           plot.display.log = input$plot.display.log, 
                           plot.color = input$plot.color,
                           plot.shape = input$plot.shape, 
                           plot.display.psize = input$plot.display.psize,
                           plot.line.trend = input$plot.line.trend,
                           plot.line.trend.size = input$plot.line.trend.size, 
                           plot.line.trend.ribbon = input$plot.line.trend.ribbon, 
                           plot.line.nd = input$plot.line.nd,
                           plot.line.nd.type = input$plot.line.nd.type,
                           plot.line.nd.size = input$plot.line.nd.size,
                           plot.line.rl = input$plot.line.rl,
                           plot.line.rl.type = input$plot.line.rl.type,
                           plot.line.rl.size = input$plot.line.rl.size,
                           plot.line.ps = input$plot.line.ps,
                           plot.line.ps.type = input$plot.line.ps.type,
                           plot.line.ps.size = input$plot.line.ps.size,
                           plot.title = input$plot.title, 
                           plot.xlab = input$plot.xlab, 
                           plot.ylab = input$plot.ylab,
                           plot.title.text = input$plot.title.text, 
                           plot.xlab.text = input$plot.xlab.text, 
                           plot.ylab.text = input$plot.ylab.text,
                           plot.save.grid = input$plot.save.grid){
                           # plot.display.theme, plot.display.log, 
                           # plot.color, plot.shape, plot.display.psize, 
                           # plot.line.trend, plot.line.trend.size, plot.line.trend.ribbon, 
                           # plot.line.nd, plot.line.nd.type, plot.line.nd.size,
                           # plot.line.rl, plot.line.rl.type, plot.line.rl.size,
                           # plot.line.ps, plot.line.ps.type, plot.line.ps.size,
                           # plot.title, plot.xlab, plot.ylab,
                           # plot.title.text, plot.xlab.text, plot.ylab.text,
                           # plot.save.grid){

### Text For Plot

site <- df %>% .$Site %>% factor() %>% levels()
text.site <- site %>% paste()
text.param <- df %>% .$Parameter %>% factor() %>% levels() %>% paste()
text.units <- df %>% .$Units %>% factor() %>% levels() %>% paste()
text.date.start <- df %>% .$Date %>% min(na.rm = TRUE) %>% paste()
text.date.end <- df %>% .$Date %>% max(na.rm = TRUE) %>% paste()


### PLOT

# Plot Creation

  
# Features in which all plot options have in common
p <- ggplot(df, aes(x = as.POSIXct(Date), y = Result)) +
  scale_x_datetime(breaks = pretty_breaks(n=12))
  
  # Display Tab
  
  # Theme based on selection
  if(plot.display.theme == "Gray"){
    p <- p + theme_gray()
  }
  if(plot.display.theme == "Black and White"){
    p <- p + theme_bw()
  }
  if(plot.display.theme == "Line Draw"){
    p <- p + theme_linedraw()
  }
  if(plot.display.theme == "Light"){
    p <- p + theme_light()
  }
  if(plot.display.theme == "Dark"){
    p <- p + theme_dark()
  }
  if(plot.display.theme == "Minimal"){
    p <- p + theme_minimal()
  }
  if(plot.display.theme == "Classic"){
    p <- p + theme_classic()
  }
  
  # Log Scale
  if("Log-scale Y Axis" %in% plot.display.log){
    p <- p + scale_y_log10()
  }
  
  # Grouping and Trendline
  
  # Group by both Color and Shape when both selected
  if(plot.color != 1 & plot.shape != 1){
    p <- p + geom_point(aes_string(color = plot.color, shape = plot.shape), size = plot.display.psize)
    if(plot.line.trend != "None"){
      p <- p + geom_smooth(method = plot.line.trend,
                           size = plot.line.trend.size,
                           se = plot.line.trend.ribbon,
                           aes_string(color = plot.color, linetype = plot.shape))
    }
  }
  # Group by only Color when only color grouping is selected
  else if (plot.color != 1){
    p <- p + geom_point(aes_string(color = plot.color), size = plot.display.psize)
    if(plot.line.trend != "None"){
      p <- p + geom_smooth(method = plot.line.trend,
                           size = plot.line.trend.size,
                           se = plot.line.trend.ribbon,
                           aes_string(color = plot.color))
    }
  }
  # Group by only Shape when only shape grouping is selected
  else if (plot.shape != 1){
    p <- p + geom_point(aes_string(shape = plot.shape), size = plot.display.psize)
    if(plot.line.trend != "None"){
      p <- p + geom_smooth(method = plot.line.trend,
                           size = plot.line.trend.size,
                           se = plot.line.trend.ribbon,
                           aes_string(linetype = plot.shape))
    }
  }
  # No Grouping Selected
  else {
    p <- p + geom_point(size = plot.display.psize)
    if(plot.line.trend != "None"){
      p <- p + geom_smooth(method = plot.line.trend,
                           size = plot.line.trend.size,
                           se = plot.line.trend.ribbon)
    }
  }
  
  # Facet for Sites if no grouping for site is selected and number of sites is greater than 1
  if(plot.color != "Site" & plot.shape != "Site" & length(c(site)) > 1){
    p <- p + facet_wrap(~Site, ncol = ceiling(length(c(site))/4))
  }
  
  # Add Lines
  
  # Show Non-Detect Level
  if(plot.line.nd == TRUE){
    p <- p + geom_hline(yintercept = 2,
                        linetype = plot.line.nd.type,
                        size = plot.line.nd.size)
  }
  
  # Show Reprting Limit
  if(plot.line.rl == TRUE){
    p <- p + geom_hline(yintercept = 3,
                        linetype = plot.line.rl.type,
                        size = plot.line.rl.size)
  }
  
  # Performance Standard
  if(plot.line.ps == TRUE){
    p <- p + geom_hline(yintercept = 4,
                        linetype = plot.line.ps.type,
                        size = plot.line.ps.size)
  }
  
  
  # Title and Axis Lables
  
  # Title
  if(plot.title == "None"){
    p <- p + ggtitle("")
  }
  if(plot.title == "Auto"){
    p <- p + ggtitle(paste(text.param, "at",
                           paste(text.site),
                           "from", text.date.start, "to", text.date.end, sep= " "))
  }
  if(plot.title == "Custom"){
    p <- p + ggtitle(plot.title.text)
  }
  
  # X Axis
  if(plot.xlab == "None"){
    p <- p + xlab("")
  }
  if(plot.xlab == "Auto"){
    p <- p + xlab("Date")
  }
  if(plot.xlab == "Custom"){
    p <- p + xlab(plot.xlab.text)
  }
  
  # Y Axis
  if(plot.ylab == "None"){
    p <- p + ylab("")
  }
  if(plot.ylab == "Auto"){
    p <- p + ylab(paste(text.param, " (", text.units,")", sep= ""))
  }
  if(plot.ylab == "Custom"){
    p <- p + ylab(plot.ylab.text)
  }
  
  # Save Options
  
  # Size dependent? Change size for saving?
  p <- p + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))
  
  # Gridlines for saving options
  if("major gridlines" %in% plot.save.grid){
    p <- p + theme(panel.grid.major = element_line())
  }
  if("minor gridlines" %in% plot.save.grid){
    p <- p + theme(panel.grid.minor = element_line())
  }
  
  
  # filename
  
  filename <- paste(text.param,' Site ', text.site,' from ', text.date.start,' to ', text.date.end, '.png', sep='')
  
  
  # Return the plot
  return(list(plot = p, filename = filename))
  
}