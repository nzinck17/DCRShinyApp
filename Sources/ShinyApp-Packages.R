
# install the packages that the Shiny App uses. This list will need to be appended as the app grows.


# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("rmarkdown", "knitr", "shiny", "tidyverse", "plotly", "leaflet", "RColorBrewer", "DT", "akima", "odbc", "DBI")
ipak(packages)


