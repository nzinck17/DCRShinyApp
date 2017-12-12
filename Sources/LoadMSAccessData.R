##############################################################################################################################
#     Title: LoadMSAccessData.R
#     Description: This script will load WQ data from both Quabbin and Wachusett Microsof Access Database
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

# File name path from the Shiny App Folder (***Update if name changed****)

filename.quab <- "DBQ=C:/WQDatabase/QUABBIN_WQDB_fe.mdb"
filename.wach.wq <- "DBQ=C:/WQDatabase/WACHUSETT_WQDB_fe.mdb"
filename.wach.aquabio <- "DBQ=C:/WQDatabase/WACHUSETT_AQBIO_DB_fe.mdb"

##############################################################################################################################
# GET DATA FROM DATABASE *(Connect to database, fetch tables, and close connection)
##############################################################################################################################

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
df.trib.bact.wach$SampleDateTime <- format(df.trib.bact.wach$SampleDateTime, tz ="America/New_York", usetz=TRUE) %>%
  as.POSIXct()
df.trib.bact.wach$Date <- as.Date(as.character(df.trib.bact.wach$SampleDateTime),format ='%Y-%m-%d %H:%M:%S')
df.phyto.wach$Phyt_Date <- as.Date(format(df.phyto.wach$Phyt_Date, tz ="America/New_York", usetz=TRUE))
df.secchi.wach$Date <- as.Date(format(df.secchi.wach$Date, tz ="America/New_York", usetz=TRUE))
df.chem.wach$Date <- as.Date(as.character(df.chem.wach$SampleDateTime),format ='%Y-%m-%d %H:%M:%S')
df.prof.wach$Date <- as.Date(df.prof.wach$Date)

# time format
#df.chem.wach$Time <- format(df.chem.wach$Time,"%H:%M:%S") No longer needed because df now has combined date-time stamp
df.prof.wach$Time <- format(df.prof.wach$Time,"%H:%M:%S")
df.prof.wach$DateTime <-as.POSIXct(paste(df.prof.wach$Date, df.prof.wach$Time), format="%Y-%m-%d %H:%M:%S")
#df.secchi.wach$SampleTime <- format(df.secchi.wach$SampleTime, "%H:%M")

# flag format - Change a "NA" or "NAN" value to "No Flag"
df.trib.bact.wach$FlagCode <- as.character(df.trib.bact.wach$FlagCode)
df.trib.bact.wach$FlagCode[is.na(df.trib.bact.wach$FlagCode)] <- "No Flag"
df.trib.bact.wach$FlagCode[is.nan(df.trib.bact.wach$FlagCode)] <- "No Flag"
df.trib.bact.wach$FlagCode <- factor(df.trib.bact.wach$FlagCode)

# Additional formatting for Phyto data
#Purge unwanted columns of data, transpose to long format, change data formats
df.phyto.wach <- select(df.phyto.wach,-Phyt_ID,-Microscope,-Magnification,-Method,-ImportDate,-DataSource, -Analyst, -UniqueID) %>%
 gather("taxa","count",5:82, na.rm =T) %>%
 dplyr::rename(Station = Phyt_Station, Year = Phyt_Year, Date = Phyt_Date, Depthm = Phyt_Depth_m, Taxa = taxa, Result = count) %>%
 select(Station, Year, Date, Depthm, Taxa, Result) %>%
 filter(Result >=0)

df.phyto.wach$Taxa <- as.factor(df.phyto.wach$Taxa)
df.phyto.wach$Station <- as.factor(df.phyto.wach$Station)

# Additional formatting for Phyto data
df.secchi.wach <- df.secchi.wach[,c(2:5)] # Remove unwated columns

###########################################################################################################################
# RESTRUCTURING SITE INFORMATION DATA
###########################################################################################################################

### QUABBIN

# delete columns
df.quab.ware.site <- df.quab.ware.site %>% select(-c(rownames, Description))

# rename columns
df.quab.ware.site <- df.quab.ware.site %>% rename(Site = SiteID, LocationType = Type, LocationDescription = SiteDescription)

# create columns
df.quab.ware.site <- df.quab.ware.site %>% mutate(LocationLabel = paste(LocationShortName, Site))
df.quab.ware.site$LocationElevFt <- NA

# change "Core" to "Primary Active"
df.quab.ware.site$LocationCategory <- as.character(df.quab.ware.site$LocationCategory)
df.quab.ware.site$LocationCategory[df.quab.ware.site$LocationCategory == "Core"] <- "Primary Active"


### WACHUSETT

# rename columns
df.trib.bact.wach.site <- df.trib.bact.wach.site %>% rename(Site = LocationMWRA)
df.chem.prof.wach.site <- df.chem.prof.wach.site %>% rename(Site = LocationMWRA, LocationDescription = StationDescription)

# create columns
df.chem.prof.wach.site$LocationElevFt <- NA
df.trib.bact.wach.site$Watershed <- "Wachusett"
df.chem.prof.wach.site$Watershed <- "Wachusett"


###########################################################################################################################
# Combine WQ with Site info (Gets the Station and Sampling Level for Chemical Sites)
###########################################################################################################################

# Quabbin Tributary and Chemical
df.trib.res.quab <- left_join(df.trib.res.quab, df.quab.ware.site, by = "Site")

# Quabbin Profile
df.prof.quab <- left_join(df.prof.quab, df.quab.ware.site, by = "Site")

# Wachusett Tributary and Bacteria
df.trib.bact.wach <- left_join(df.trib.bact.wach, df.trib.bact.wach.site, by = "Site")

# Wachusett Chemical
df.chem.wach <- left_join(df.chem.wach, df.chem.prof.wach.site, by = "Site")

# Wachusett Profile
df.prof.wach <- left_join(df.prof.wach, df.chem.prof.wach.site, by = "Site")

###########################################################################################################################
# List of Column names to be shown in Trib and Res Module tables and Default Selected in Export Module
###########################################################################################################################

# Trib and Res Columns to read into the App (right now for Wachusett but eventually for Quabbin too)
#wq.col.list <- "ID, UniqueID, Location, SampleDateTime, Parameter, Units, FinalResult, StormSample, StormSampleN, DetectionLimit, FlagCode"

# Quabbin, Ware, and All Tributary
col.trib.quab.ware <- c("LocationLabel", "Date", "Parameter", "Result", "Units", "Site", "LocationCategory")

# Wachusett Tributary
col.trib.wach <- c("LocationLabel", "SampleDateTime", "Date", "Parameter", "Result", "Units","FlagCode", "StormSample", "Site", "LocationCategory")

# Quabbin (Res) Bacteria
col.bact.quab <- c("LocationLabel", "Date", "Sampling_Level", "Parameter", "Result", "Units", "Site", "Station", "LocationCategory")

# Wachusett Res Bacteria
col.bact.wach <- c("LocationLabel", "Date", "Parameter", "Result", "Units", "FlagCode", "StormSample", "Site", "LocationCategory")

# Quabbin (Res) Chemical
col.chem.quab <- c("LocationLabel", "Date", "Sampling_Level", "Parameter", "Result", "Units", "Site", "Station", "LocationCategory")

# Wachusett Res Chemical
col.chem.wach <- c("LocationLabel", "Date", "LocationDepth", "Parameter", "Result", "Units", "FlagCode", "Site", "Station", "LocationCategory")

# Quabbin (Res) Profile
col.prof.quab <- c("LocationLabel", "Date", "Depthm", "Parameter", "Result", "Units", "Site", "Station", "LocationCategory")

# Wachusett Res Profile
col.prof.wach <- c("LocationLabel", "Date", "Depthm", "Parameter", "Result", "Site", "Station", "LocationCategory") # need units??

###########################################################################################################################
# Final Water Quality Dataframes
###########################################################################################################################

# Quabbin Tributary
df.trib.quab.exp <- df.trib.res.quab %>% filter(LocationType == "Tributary", Watershed == "Quabbin")
df.trib.quab <- df.trib.quab.exp %>% select(col.trib.quab.ware)

# Ware River Tributary
df.trib.ware.exp <- df.trib.res.quab %>% filter(LocationType == "Tributary", Watershed == "Ware River")
df.trib.ware <- df.trib.ware.exp %>% select(col.trib.quab.ware)

# Wachusett Tributary
df.trib.wach.exp <- df.trib.bact.wach %>% filter(LocationType == "Tributary")
df.trib.wach <- df.trib.wach.exp %>% select(col.trib.wach)

# All Tributaries
df.trib.all.exp <- bind_rows(df.trib.quab, df.trib.ware, df.trib.wach)
df.trib.all <- df.trib.all.exp %>%  select(col.trib.quab.ware)

# Quabbin Bacteria
df.bact.quab.exp <- df.trib.res.quab %>% filter(LocationType == "Nutrient")
df.bact.quab <- df.bact.quab.exp %>%  select(col.bact.quab)

# Wachusett Bacteria
df.bact.wach.exp <- df.trib.bact.wach %>% filter(LocationType == "Transect")
df.bact.wach <- df.bact.wach.exp %>% select(col.bact.wach)

# Quabbin Chemical
df.chem.quab.exp <- df.trib.res.quab %>% filter(LocationType == "Nutrient")
df.chem.quab <- df.chem.quab.exp %>% select(col.chem.quab)

# Wachusett Chemical (all set?)
df.chem.wach.exp <- df.chem.wach
df.chem.wach <- df.chem.wach.exp  %>% select(col.chem.wach)

# Quabbin Profile (all set?)
df.prof.quab.exp <- df.prof.quab
df.prof.quab <- df.prof.quab.exp %>% select(col.prof.quab)

# Wachusett Profile (all set?)
df.prof.wach.exp <- df.prof.wach
df.prof.wach <- df.prof.wach.exp  %>% select(col.prof.wach)

# Wachusett Phytoplankton
#df.phyto.wach <- df.phyto.wach

###########################################################################################################################
# Final Site/Location Information Dataframes
###########################################################################################################################

# Quabbin Tributary
df.trib.quab.site <- df.quab.ware.site %>% filter(Watershed == "Quabbin", LocationType == "Tributary")

# Ware River Tributary
df.trib.ware.site <- df.quab.ware.site %>% filter(Watershed == "Ware River")

# Wachusett Tributary
df.trib.wach.site <- df.trib.bact.wach.site %>% filter(LocationType == "Tributary")

# Quabbin Bacteria (Need to find sites)
df.bact.quab.site <- df.quab.ware.site %>% filter(LocationType == "Transect")

# Wachusett Bacteria
df.bact.wach.site <- df.trib.bact.wach.site %>% filter(LocationType == "Transect")

# Quabbin Chemical
df.chem.quab.site <- df.quab.ware.site %>% filter(LocationType == "Nutrient")

# Wachusett Chemical (Maybe in Site Table change location LocationType to Chemical or )
df.chem.wach.site <- df.chem.prof.wach.site %>% filter(!is.na(LocationDepth))

# Quabbin Profile (Need to find Sites)

# Wachusett Profile
df.prof.wach.site <- df.chem.prof.wach.site %>% filter(is.na(LocationDepth))

# Wachusett Phytoplankton

# Just use match function where needed - like in pick lists and chart labels/titles rather than append site data to each record

# All Sites - # Combine All Sites into 1 dataframe ( Need to update when Sites are squared away)
df.all.site.temp <- full_join(df.quab.ware.site,
                              df.trib.bact.wach.site,
                              by = c("Site",
                                     "Watershed",
                                     "LocationType",
                                     "LocationLong",
                                     "LocationLat",
                                     "LocationLabel",
                                     "LocationDescription",
                                     "LocationElevFt"))

df.all.site <- full_join(df.all.site.temp,
                         df.chem.prof.wach.site,
                         by = c("Site",
                                "Station",
                                "Watershed",
                                "LocationType",
                                "LocationLong",
                                "LocationLat",
                                "LocationLabel",
                                "LocationDescription",
                                "LocationElevFt"))

# All Tributaries
df.trib.all.site <- df.all.site %>% filter(LocationType == "Tributary")



