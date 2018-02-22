# This script will run in a hidden rscript.exe session.

# 1.   nstall the packages that the Shiny App uses. This list will need to be appended as the app grows.
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.rstudio.com/")
  sapply(pkg, require, character.only = TRUE)
}
.libPaths(paste0("C:/R-",R.version$major,".",R.version$minor,"/library"))
# usage
packages <- "shiny"
ipak(packages)


# Launch App from GitHub
#runGitHub("DCRShinyApp", "dancrocker", launch.browser=TRUE)
runGitHub("DCRShinyApp", "nzinck17", launch.browser=TRUE)

# Launch locally as a separate process

#shiny::runApp('C:/R-Shiny/DCRShinyApp', launch.browser=TRUE)
