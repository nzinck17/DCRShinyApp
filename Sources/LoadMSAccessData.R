##############################################################################################################################
#     Title: LoadMSAccessData
#     Description: This script will load WQ data from both Quabbin and Wachusett Microsof Access Database
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

# File name path from the Shiny App Folder (***Update if name changed****)

filename.quab <- "DBQ=C:/WQDatabase/QuabbinWQdata_fe.mdb"
filename.wach.wq <- "DBQ=C:/WQDatabase/WaterQualityDB_fe.mdb"
filename.wach.aquabio <- "DBQ=C:/WQDatabase/AqBioDBWachusett_fe.mdb"

##############################################################################################################################
# Get data from Database *(Connect to database, fetch tables, and close connection)

# Quabbin Tribs and Res

con <- dbConnect(odbc::odbc(),
                 .connection_string = paste("driver={Microsoft Access Driver (*.mdb, *.accdb)}", filename.quab, "Uid=Admin;Pwd=;", sep = ";"),
                 timezone = "America/New_York")


# connection.name <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
#                          filename.quab,
#                          "Encrypt=yes",
#                          "TrustServerCertificate=no",
#                          "Connection Timeout=30",
#                          "ReadOnly=False",
#                          sep = ";")


df.trib.res.quab <- dbReadTable(con, "tblWQTribRes2")
df.prof.quab <- dbReadTable(con, "tblWQProfile")
df.quab.ware.site <- dbReadTable(con, "tblSiteLocation2")
df.quab.param <- dbReadTable(con, "tblParameters")
# Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)

## RODBC METHOD
# df.trib.res.quab <- sqlFetch(connection, "tblWQTribRes2")
# df.prof.quab <- sqlFetch(connection, "tblWQProfile")
# df.quab.ware.site <- sqlFetch(connection, "tblSiteLocation2")
# df.quab.param <- sqlFetch(connection, "tblParameters")
# close(connection)
# rm(connection)

# Wachusett Tribs
con <- dbConnect(odbc::odbc(),
                             .connection_string = paste("driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                              filename.wach.wq, "Uid=Admin;Pwd=;", sep = ";"),
                             timezone = "America/New_York")

# connection.name <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
#                          filename.wach.wq,
#                          "Encrypt=yes",
#                          "TrustServerCertificate=no",
#                          "Connection Timeout=30",
#                          "ReadOnly=False",
#                          sep = ";")


df.trib.transect.wach <- dbReadTable(con, "tblWQALLDATA")
df.trib.tran.wach.site <- dbReadTable(con, "tblLocations")
df.trib.transect.wach.param <- dbReadTable(con, "tblParameters")
# Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)

## RODBC METHOD
# df.trib.transect.wach <- sqlFetch(connection, "tblWQALLDATA")
# df.trib.tran.wach.site <- sqlFetch(connection, "tblLocations")
# df.trib.transect.wach.param <- sqlFetch(connection, "tblParameters")
# close(connection)
# rm(connection)

# Wachusett Profile
con<- dbConnect(odbc::odbc(),
                             .connection_string = paste("driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                              filename.wach.aquabio, "Uid=Admin;Pwd=;", sep = ";"),
                              timezone = "America/New_York")

# connection.name <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
#                          filename.wach.aquabio,
#                          "Encrypt=yes",
#                          "TrustServerCertificate=no",
#                          "Connection Timeout=30",
#                          "ReadOnly=False",
#                          sep = ";")

df.nut.wach <- dbReadTable(con, "tbl_Nutrients")
df.prof.wach <- dbReadTable(con, "tbl_Profiles")
df.nut.prof.wach.site <- dbReadTable(con, "tblLocations")
# Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)
# df.prof.wach <- sqlFetch(connection, "tbl_Profiles")
# df.nut.prof.wach.site <- sqlFetch(connection, "tblLocations")
# close(connection)
# rm(connection)

#############################################################################################################################
# RESTRUCTURING DATA

### WQ data

# rename the columns of the Wachusett Trib and Res to match Quabbin
df.trib.transect.wach <- rename(df.trib.transect.wach, Site = Location, `Result Temp` = Result, Result = FinalResult)
df.nut.wach <- rename(df.nut.wach, Site = Location, Result = Finalresult, Date = Date_Collected, Time = Collection_Time,
                      Parameter = Component, Units = Unit_of_Measure)
df.prof.wach <- rename(df.prof.wach, Date = Pro_Date, Site = Pro_Station, Time = Pro_TimeFormatted, Depthm = Pro_Depth_m)

# Reformat the Wachusett Profile data to "Tidy" data format
df.prof.wach <- gather(df.prof.wach, Parameter, Result, c(Temp_C, SpCond, LDO_pct, LDO_mgL, pH, Chl_ugL, Chl_volts, Turbidity, TDS_mgL, BGA_PC_ugL))

# Result Format - Numeric Class (Make Character first in the case that the results are imported as factors (could maybe be set within DB))
df.trib.res.quab$Result <- as.numeric(as.character(df.trib.res.quab$Result))
df.trib.transect.wach$Result <- as.numeric(as.character(df.trib.transect.wach$Result))
df.nut.wach$Result <- as.numeric(as.character(df.nut.wach$Result))
df.prof.quab$Result <- as.numeric(as.character(df.prof.quab$Result))
df.prof.wach$Result <- as.numeric(as.character(df.prof.wach$Result))

# Date Format - Date Class. (Could maybe be set withing DB)
df.trib.res.quab$Date <- as.Date(as.character(df.trib.res.quab$Date),format ='%m/%d/%Y')
df.trib.transect.wach$SampleDateTime <- format(df.trib.transect.wach$SampleDateTime, tz ="America/New_York", usetz=TRUE)
df.trib.transect.wach$Date <- as.Date(as.character(df.trib.transect.wach$SampleDateTime),format ='%Y-%m-%d %H:%M:%S')
df.nut.wach$Date <- as.Date(as.character(df.nut.wach$Date),format ='%Y-%m-%d %H:%M:%S')
df.prof.quab$Date <- as.Date(as.character(df.prof.quab$Date), format = '%d-%b-%y')
df.prof.wach$Date <- as.Date(df.prof.wach$Date)

# Time Format
df.nut.wach$Time <- format(df.nut.wach$Time,"%H:%M:%S")
df.prof.wach$Time <- format(df.prof.wach$Time,"%H:%M:%S")
df.prof.wach$DateTime <-as.POSIXct(paste(df.prof.wach$Date, df.prof.wach$Time), format="%Y-%m-%d %H:%M:%S")

# Flag Format - Change a "NA" or "NAN" value to "No Flag"
df.trib.transect.wach$FlagCode <- as.character(df.trib.transect.wach$FlagCode)
df.trib.transect.wach$FlagCode[is.na(df.trib.transect.wach$FlagCode)] <- "No Flag"
df.trib.transect.wach$FlagCode[is.nan(df.trib.transect.wach$FlagCode)] <- "No Flag"
df.trib.transect.wach$FlagCode <- factor(df.trib.transect.wach$FlagCode)


#### Edit Site Tables

# Quabbin
df.quab.ware.site <- df.quab.ware.site %>% rename(Site = SiteID, LocationType = Type)
df.quab.ware.site <- df.quab.ware.site %>% mutate(LocationLabel = paste(LocationShortName, Site))
df.quab.ware.site$LocationElevFt <- NA
df.quab.ware.site <- df.quab.ware.site %>% rename(LocationDescription = SiteDescription)
df.quab.ware.site <- df.quab.ware.site %>% select(-Description)

df.quab.ware.site$LocationCategory <- as.character(df.quab.ware.site$LocationCategory)
df.quab.ware.site$LocationCategory[df.quab.ware.site$LocationCategory == "Core"] <- "Primary Active"

# Wachusett
df.trib.tran.wach.site <- df.trib.tran.wach.site %>% rename(Site = LocationMWRA)
df.nut.prof.wach.site <- df.nut.prof.wach.site %>% rename(Site = LocationMWRA, LocationDescription = StationDescription)
df.nut.prof.wach.site$LocationElevFt <- NA
df.trib.tran.wach.site$Watershed <- "Wachusett"
df.nut.prof.wach.site$Watershed <- "Wachusett"

###
# Combine WQ with Site info (Gets the Station and Sampling Level for Nutrient Sites)
df.trib.res.quab <- left_join(df.trib.res.quab, df.quab.ware.site, "Site")  #
df.trib.transect.wach <- left_join(df.trib.transect.wach, df.trib.tran.wach.site, "Site")

#############################################################################################################################
# Create final Dataframes for App

### WQ Data

# Quabbin Tributary
df.trib.quab <- filter(df.trib.res.quab, LocationType == "Tributary", Watershed == "Quabbin")

# Ware River Tributary
df.trib.ware <- filter(df.trib.res.quab, LocationType == "Tributary", Watershed == "Ware River")

# Wachusett Tributary
df.trib.wach <- filter(df.trib.transect.wach, LocationType == "Tributary")

# All Tributaries
df.trib.all <- bind_rows(df.trib.quab, df.trib.ware, df.trib.wach)

# Quabbin Transect
df.tran.quab <- filter(df.trib.res.quab, LocationType == "Transect")

# Wachusett Transect
df.tran.wach <- filter(df.trib.transect.wach, LocationType == "Transect")

# Quabbin Nutrient
df.nut.quab <- filter(df.trib.res.quab, LocationType == "Nutrient")

# Wachusett Nutrient (all set?)

# Quabbin Profile (all set?)

# Wachusett Profile (all set?)


### Site Locations

# Split Sites into 5 dataframes

# Quabbin Tributary
df.trib.quab.site <- df.quab.ware.site %>% filter(Watershed == "Quabbin", LocationType == "Tributary")

# Ware River Tributary
df.trib.ware.site <- df.quab.ware.site %>% filter(Watershed == "Ware River")

# Wachusett Tributary
df.trib.wach.site <- df.trib.tran.wach.site %>% filter(LocationType == "Tributary")

# Quabbin Transect (Need to find sites)
df.tran.quab.site <- df.quab.ware.site %>% filter(LocationType == "Transect")

# Wachusett Transect
df.tran.wach.site <- df.trib.tran.wach.site %>% filter(LocationType == "Transect")

# Quabbin Nutrient
df.nut.quab.site <- df.quab.ware.site %>% filter(LocationType == "Nutrient")

# Wachusett Nutrient (Maybe in Site Table change location LocationType to Nutrient or )
df.nut.wach.site <- df.nut.prof.wach.site %>% filter(!is.null(LocationDepth))

# Quabbin Profile (Need to find Sites)

# Wachusett Profile
df.prof.wach.site <- df.nut.prof.wach.site %>% filter(is.null(LocationDepth))


# All Sites
# ! Need to update when Sites are squared away
# Combine All Sites into 1 dataframe


df.all.site.temp <- full_join(df.quab.ware.site,
                              df.trib.tran.wach.site,
                              by = c("Site",
                                     "Watershed",
                                     "LocationType",
                                     "LocationLong",
                                     "LocationLat",
                                     "LocationLabel",
                                     "LocationDescription",
                                     "LocationElevFt"))

df.all.site <- full_join(df.all.site.temp,
                         df.nut.prof.wach.site,
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



