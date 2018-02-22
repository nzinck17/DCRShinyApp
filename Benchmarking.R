

# devtools::install_github("hadley/lineprof")
# library(lineprof)
library(fasttime)
install.packages('fasttime','http://www.rforge.net/')
library(microbenchmark)

# This function replicates all code run by the Launch script and LoadMSAccessData.R up through line 123
LoadData <- function(){

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

packages <- c("rmarkdown", "knitr", "tidyverse", "lubridate", "plotly", "leaflet", "RColorBrewer",
              "DT", "akima", "odbc", "DBI", "scales", "stringr", "cowplot", "shinythemes", "fasttime")
ipak(packages)

# Then install and load this:


filename.quab <- "DBQ=C:/WQDatabase/QUABBIN_WQDB_fe.mdb"
filename.wach.wq <- "DBQ=C:/WQDatabase/WACHUSETT_WQDB_fe.mdb"
filename.wach.aquabio <- "DBQ=C:/WQDatabase/WACHUSETT_AQBIO_DB_fe.mdb"

### Quabbin Tribs and Res



# Connect to db
con <- dbConnect(odbc::odbc(),
                 .connection_string = paste("driver={Microsoft Access Driver (*.mdb, *.accdb)}", filename.quab, "Uid=Admin;Pwd=;", sep = ";"),
                 timezone = "America/New_York")

# Read Tables
df.trib.res.quab <- dbReadTable(con, "tblWQTribRes2")
df.prof.quab <- dbReadTable(con, "tblWQProfile")
df.quab.ware.site <- dbReadTable(con, "tblSiteLocation2")
df.quab.param <- dbReadTable(con, "tblParameters")

# Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)


### Wachusett Tribs and Reservoir Bact

# Connect to db
con <- dbConnect(odbc::odbc(),
                 .connection_string = paste("driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                                            filename.wach.wq, "Uid=Admin;Pwd=;", sep = ";"),
                 timezone = "America/New_York")

# Read Tables
df.trib.bact.wach <- dbReadTable(con, "tblWQALLDATA")
#df.trib.bact.wach <- dbGetQuery(con, paste("SELECT", wq.col.list, "FROM tblWQALLDATA"))
df.trib.bact.wach.site <- dbReadTable(con, "tblLocations")
df.trib.bact.wach.param <- dbReadTable(con, "tblParameters")

# Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)


### Wachusett Reservoir (Profile, Phytoplankton, Physiochemical)

# Connect to db
con<- dbConnect(odbc::odbc(),
                .connection_string = paste("driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                                           filename.wach.aquabio, "Uid=Admin;Pwd=;", sep = ";"),
                timezone = "America/New_York")

# Read Tables
df.chem.wach <- dbReadTable(con, "tbl_Nutrients")
df.prof.wach <- dbReadTable(con, "tbl_Profiles")
df.chem.prof.wach.site <- dbReadTable(con, "tblLocations")
df.secchi.wach <- dbReadTable(con, "tblSecchi") # Wachusett Secchi
df.phyto.wach <- dbReadTable(con, "tbl_Phyto") # Wachusett Phytoplankton
df.phyto_thresh.wach <- dbReadTable(con, "tbl_PhytoThresholds") # Wachusett Phytoplankton thresholds


# Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)


###########################################################################################################################
# RESTRUCTURING WATER QUALITY DATA
###########################################################################################################################

### QUABBIN

# delete columns
df.trib.res.quab <- df.trib.res.quab %>% select(-rownames)
df.prof.quab <- df.prof.quab %>% select(-rownames)

# result format - Numeric Class (Make Character first in the case that the results are imported as factors)
df.trib.res.quab$Result <- as.numeric(as.character(df.trib.res.quab$Result))
df.prof.quab$Result <- as.numeric(as.character(df.prof.quab$Result))

            # date Format - Date Class
            df.trib.res.quab$Date <- as.Date(as.character(df.trib.res.quab$Date),format ='%m/%d/%Y')
            df.prof.quab$Date <- as.Date(as.character(df.prof.quab$Date), format = '%d-%b-%y')

### WACHUSETT

# rename columns
df.trib.bact.wach <- rename(df.trib.bact.wach, Site = Location, `Result Temp` = ResultReported, Result = FinalResult)
df.chem.wach <- rename(df.chem.wach, Site = Location, `Result Temp` = ResultReported, Result = FinalResult)
df.prof.wach <- rename(df.prof.wach, Date = Pro_Date, Site = Pro_Station, Time = Pro_TimeFormatted, Depthm = Pro_Depth_m)

# reformat the Wachusett Profile data to "Tidy" data format ("Long" instead of "Wide")
df.prof.wach <- gather(df.prof.wach, Parameter, Result, c(Temp_C, SpCond, LDO_pct, LDO_mgL, pH, Chl_ugL, Chl_volts, Turbidity, TDS_mgL, BGA_PC_ugL))

# result format - Numeric Class (Make Character first in the case that the results are imported as factors (could maybe be set within DB))
df.trib.bact.wach$Result <- as.numeric(as.character(df.trib.bact.wach$Result))
df.chem.wach$Result <- as.numeric(as.character(df.chem.wach$Result))
df.prof.wach$Result <- as.numeric(as.character(df.prof.wach$Result))

# date format - Date Class. (Could maybe be set withing DB)
            # df.trib.bact.wach$SampleDateTime <- format(df.trib.bact.wach$SampleDateTime, tz ="America/New_York", usetz=TRUE) %>%
            #   as.POSIXct()
            df.trib.bact.wach$SampleDateTime <- format(df.trib.bact.wach$SampleDateTime, tz ="America/New_York", usetz=TRUE) %>%
            fastPOSIXct(tz = "America/New_York", required.components = 6L)



                    df.trib.bact.wach$Date <- as.Date(as.character(df.trib.bact.wach$SampleDateTime),format ='%Y-%m-%d %H:%M:%S')
                    df.phyto.wach$Phyt_Date <- as.Date(format(df.phyto.wach$Phyt_Date, tz ="America/New_York", usetz=TRUE))
                    df.secchi.wach$Date <- as.Date(format(df.secchi.wach$Date, tz ="America/New_York", usetz=TRUE))
                    df.chem.wach$Date <- as.Date(as.character(df.chem.wach$SampleDateTime),format ='%Y-%m-%d %H:%M:%S')
                    df.prof.wach$Date <- as.Date(df.prof.wach$Date)

# time format
          #df.chem.wach$Time <- format(df.chem.wach$Time,"%H:%M:%S") No longer needed because df now has combined date-time stamp
          df.prof.wach$Time <- format(df.prof.wach$Time,"%H:%M:%S")
###          df.prof.wach$DateTime <- as.POSIXct(paste(df.prof.wach$Date, df.prof.wach$Time), format="%Y-%m-%d %H:%M:%S")
          df.prof.wach$DateTime <- fastPOSIXct(paste(df.prof.wach$Date, df.prof.wach$Time), tz = "America/New_York", required.components = 6L)
}


microbenchmark(
  as.POSIXct(paste(df.prof.wach$Date, df.prof.wach$Time), format="%Y-%m-%d %H:%M:%S"),
  fastPOSIXct(paste(df.prof.wach$Date, df.prof.wach$Time), tz = "America/New_York", required.components = 6L),
  times = 1
)
paste(df.prof.wach$Date, df.prof.wach$Time)

fastPOSIXct(paste(df.prof.wach$Date, df.prof.wach$Time), tz = "America/New_York", required.components = 6L)

file <- "W:/WatershedJAH/EQStaff/WQDatabase/OtherData/FutureMultiParamFileDownload_DCRWebsite_text.txt"
data <- read.delim(file, header=T, sep=',', dec = '.', stringsAsFactors=T , quote = "\"" ,   fill = TRUE , skip = 0 )
write.csv(data, "W:/WatershedJAH/EQStaff/WQDatabase/OtherData/FutureMultiParamFileDownload_DCRWebsite_formatted.csv")
