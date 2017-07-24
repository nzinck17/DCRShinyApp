# Function to create a circle size legend in Leaflet

# This takes as input a variable to determine the size range,
# a number of sizes to show, and a label and writes the HTML
# to create the legend.
#
# It requires some custom CSS that must be included somewhere:
#
# .legendCircle {
#   border-radius:50%; 
#   border: 2px solid black; 
#   display: inline-block;
#   position: relative;
# }
#
# There is no guarantee that the sizes will be comparable, so
# use with caution.  This assumes you are mapping circle radius
# to the square root of the value.  If some other function is 
# being used, if should be changed in the fuction.

circleSizeLegend <- function(x,n,label) {
  
  # Get the range
  
  max.x = 100 #max(x$Value)
  min.x = 0 #min(x$Value)
  
  # Set up the initial header for the code
  # The width is set to be reasonable and make the spacing work.
  # It really should be done more intelligently.
  
  myhtml <- paste0("<div id='customlegend' style='width:90px'><strong>",label,"</strong><br>")
  
  # Loop over the number of divisions  
  
  myvalue = round(max.x, 0)
  
  size = round(sqrt(myvalue) * 2, 1)
  margin = 16 # This is just a good starting value visually
  
  for(i in 1:n){
    
    myhtml <- paste0(myhtml,"<div class = 'legendCircle' style='width: ",size,"px;height: ",
                     size,"px; margin-left:",margin,"px'></div><span style='position:absolute;right:8px'>",
                     myvalue,"</span><br>")
    
    if (i == n){break}
    
    # Compute value for next iteration
    
    myvalue = round(max.x - (i * ((max.x - min.x) / (n - 1))),0)
    oldsize = size
    size = round(sqrt(myvalue) * 2, 1)
    margin = margin + (oldsize - size) / 2
    
  }
  
  # Close up the div
  
  myhtml <- paste0(myhtml,"</div>")
  
  return(myhtml)
}