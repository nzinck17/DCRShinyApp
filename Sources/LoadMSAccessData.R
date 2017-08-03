##############################################################################################################################
#     Title: LoadMSAccessData
#     Description: This script will load WQ data from both Quabbin and Wachusett Microsof Access Database
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

# File name path from the Shiny App Folder (***Update if name changed****)
filename.quab <- "DBQ=C:/WQDatabase/QuabbinWQdataNZ.mdb" 
filename.wach.wq <- "DBQ=C:/WQDatabase/WaterQualityDB_be.mdb"
filename.wach.aquabio <- "DBQ=C:/WQDatabase/AqBioDBWachusett_be.mdb"

##############################################################################################################################
# Get data from Database *(Connect to database, fetch tables, and close connection)

# Quabbin Tribs and Res

connection.name <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                         filename.quab,
                         "Encrypt=yes",
                         "TrustServerCertificate=no",
                         "Connection Timeout=30", 
                         "ReadOnly=False",
                         sep = ";")


connection <- odbcConnectAccess(connection.name)
df.trib.res.quab <- sqlFetch(connection, "tblWQTribRes2")
df.profile.quab <- sqlFetch(connection, "tblWQProfile") 
df.quab.ware.site <- sqlFetch(connection, "tblSiteLocation2")
df.quab.param <- sqlFetch(connection, "tblParameters")
close(connection)
rm(connection)

# Wachusett Tribs

connection.name <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                         filename.wach.wq,
                         "Encrypt=yes",
                         "TrustServerCertificate=no",
                         "Connection Timeout=30", 
                         "ReadOnly=False",
                         sep = ";")

connection <- odbcConnectAccess(connection.name)
df.trib.res.wach <- sqlFetch(connection, "tblWQALLDATA")
df.wach.site <- sqlFetch(connection, "tblLocations")
df.trib.res.wach.param <- sqlFetch(connection, "tblParameters")
close(connection)
rm(connection)

# Wachusett Profile

connection.name <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                         filename.wach.aquabio,
                         "Encrypt=yes",
                         "TrustServerCertificate=no",
                         "Connection Timeout=30", 
                         "ReadOnly=False",
                         sep = ";")

connection <- odbcConnectAccess(connection.name)   
df.profile.wach <- sqlFetch(connection, "tbl_Profiles")
df.profile.wach.site <- sqlFetch(connection, "tblLocations")
close(connection)
rm(connection)

               


#############################################################################################################################
# RESTRUCTURING DATA

### WQ data

# rename the columns of the Wachusett Trib and Res to match Quabbin
df.trib.res.wach <- rename(df.trib.res.wach, Site = Location, `Result Temp` = Result, Result = FinalResult)
df.profile.wach <- rename(df.profile.wach, Date = Pro_Date, Site = Pro_Station, Time = Pro_TimeFormatted, Depthm = Pro_Depth_m)

# Reformat the Wachusett Profile data to "Tidy" data format
df.profile.wach <- gather(df.profile.wach, Parameter, Result, c(Temp_C, SpCond, LDO_pct, LDO_mgL, pH, Chl_ugL, Chl_volts, Turbidity, TDS_mgL, BGA_PC_ugL))

# make result column numeric class. Make Character first in the case that the results are imported as factors (could maybe be set within DB)
df.trib.res.quab$Result <- as.numeric(as.character(df.trib.res.quab$Result))
df.trib.res.wach$Result <- as.numeric(as.character(df.trib.res.wach$Result))
df.profile.quab$Result <- as.numeric(as.character(df.profile.quab$Result))
df.profile.wach$Result <- as.numeric(as.character(df.profile.wach$Result))

# Format Date (make date class). (Could maybe be set withing DB)
df.trib.res.wach$SampleDateTime <- format(df.trib.res.wach$SampleDateTime, tz ="America/New_York", usetz=TRUE)
df.trib.res.quab$Date <- as.Date(as.character(df.trib.res.quab$Date),format ='%m/%d/%Y')
df.trib.res.wach$Date <- as.Date(as.character(df.trib.res.wach$SampleDateTime),format ='%Y-%m-%d %H:%M:%S')
df.profile.quab$Date <- as.Date(as.character(df.profile.quab$Date), format = '%d-%b-%y')
df.profile.wach$Date <- as.Date(df.profile.wach$Date)

# add a time field
df.profile.wach$Time <- format(df.profile.wach$Time,"%H:%M:%S")
df.profile.wach$DateTime <-as.POSIXct(paste(df.profile.wach$Date, df.profile.wach$Time), format="%Y-%m-%d %H:%M:%S")

# Change a "NA" or "NAN" value to "No Flag"
df.trib.res.wach$FlagCode <- as.character(df.trib.res.wach$FlagCode)
df.trib.res.wach$FlagCode[is.na(df.trib.res.wach$FlagCode)] <- "No Flag"
df.trib.res.wach$FlagCode[is.nan(df.trib.res.wach$FlagCode)] <- "No Flag"
df.trib.res.wach$FlagCode <- factor(df.trib.res.wach$FlagCode)


#### Edit Site Tables

# Wachusett
df.wach.site <- df.wach.site %>% rename(Site = LocationMWRA, Type = LocationType)
df.wach.site$Watershed <- "Wachusett"
df.wach.site$Loc <- df.wach.site$Site
df.wach.site$Depth <- "Unknown"

# Quabbin
df.quab.ware.site <- df.quab.ware.site %>% rename(Site = SiteID)
df.quab.ware.site <- df.quab.ware.site %>% mutate(LocationLabel = paste(LocationShortName, Site))
df.quab.ware.site$LocationElevFt <- NA
df.quab.ware.site <- df.quab.ware.site %>% rename(LocationDescription = SiteDescription)
df.quab.ware.site <- df.quab.ware.site %>% select(-Description)


###
# make a dplyr combine with the Sites (choose by trib recieving body (quab, ware, wach) and by (Core or EQA)
df.trib.res.quab <- left_join(df.trib.res.quab, df.quab.ware.site, "Site")
df.trib.res.wach <- left_join(df.trib.res.wach, df.wach.site, "Site")

#############################################################################################################################
# Create final Dataframes for App

### Tributaries

# Quabbin Tributary
df.trib.quab <- filter(df.trib.res.quab, Type == "Tributary", Watershed == "Quabbin")

# Ware River Tributary
df.trib.ware <<- filter(df.trib.res.quab, Type == "Tributary", Watershed == "Ware River")

# Wachusett Tributary
df.trib.wach <- filter(df.trib.res.wach, Type == "Tributary")

# All Tributaries
df.trib.all <- bind_rows(df.trib.quab, df.trib.ware, df.trib.wach)

### Reservoirs

# Quabbin Reservoir
df.res.quab <- filter(df.trib.res.quab, Type == "Reservoir")

# Wachusett Tributary (need to find Reservoir Sites)
df.res.wach <- filter(df.trib.res.wach, Type == "Transect")

### Profiles (No work needed)


### Site Locations

# Split Sites into 5 dataframes

df.trib.quab.site <- df.quab.ware.site %>% filter(Watershed == "Quabbin",
                                                  Type == "Tributary")
df.trib.ware.site <- df.quab.ware.site %>% filter(Watershed == "Ware River")
df.trib.wach.site <- df.wach.site %>% filter(Type == "Tributary")
df.res.quab.site <- df.quab.ware.site %>% filter(Type == "Reservoir")
df.res.wach.site <- df.wach.site %>% filter(Type == "Transect")

# Combine All Sites into 1 dataframe

df.all.site <- full_join(df.quab.ware.site, df.wach.site, by = c("Site", 
                                                                 "Watershed", 
                                                                 "Type",
                                                                 "LocationLong", 
                                                                 "LocationLat", 
                                                                 "LocationLabel",
                                                                 "LocationDescription",
                                                                 "LocationElevFt"))

# All tribs
df.trib.all.site <- df.all.site %>% filter(Type == "Tributary")


