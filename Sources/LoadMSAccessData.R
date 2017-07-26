##############################################################################################################################
#     Title: LoadMSAccessData
#     Description: This script will load WQ data from both Quabbin and Wachusett Microsof Access Database
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

# File name path from the Shiny App Folder (***Update if name changed****)
filename.quab <- "DBQ=F:/Nick Zinck/Shiny Water Quality/DCRShinyApp/Data/QuabbinWQdataNZ.mdb" 
filename.wach.trib.res <- "DBQ=F:/Nick Zinck/Shiny Water Quality/DCRShinyApp/Data/WaterQualityDB_be.mdb"
filename.wach.profile <- "DBQ=F:/Nick Zinck/Shiny Water Quality/DCRShinyApp/Data/AqBioDBWachusett_be.mdb"

##############################################################################################################################
# Get data from Database *(Connect to database, fetch tables, and close connection)

# Quabbin Tribs, Res, and Profile

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
df.quab.wach.site <- sqlFetch(connection, "tblSiteLocation2")
df.quab.param <- sqlFetch(connection, "tblParameters")
close(connection)                                  

# Wachusett Tribs and Res

connection.name <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                         filename.wach.trib.res,
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

# Wachusett Profile

connection.name <- paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                         filename.wach.profile,
                         "Encrypt=yes",
                         "TrustServerCertificate=no",
                         "Connection Timeout=30", 
                         "ReadOnly=False",
                         sep = ";")

connection <- odbcConnectAccess(connection.name)   
df.profile.wach <- sqlFetch(connection, "tbl_Profiles")
df.profile.wach.site <- sqlFetch(connection, "tblLocations")
close(connection)

#############################################################################################################################
# Restructuring data

# rename the columns of the Wachusett Trib and Res to match Quabbin
df.trib.res.wach <- rename(df.trib.res.wach, Site = Location, `Result Temp` = Result, Result = FinalResult)
df.profile.wach <- rename(df.profile.wach, Date = Pro_Date, Site = Pro_Station, Time = Pro_TimeFormatted, Depthm = Pro_Depth_m)

# Reformat the Wachusett Profile data to "Tidy" data format

df.profile.wach <- gather(df.profile.wach, Parameter, Result, c(Temp_C, SpCond, LDO_pct, LDO_mgL, pH, Chl_ugL, Chl_volts, Turbidity, TDS_mgL, BGA_PC_ugL))

# make result column numeric class
df.trib.res.quab$Result <- as.numeric(as.character(df.trib.res.quab$Result))
df.trib.res.wach$Result <- as.numeric(as.character(df.trib.res.wach$Result))
df.profile.quab$Result <- as.numeric(as.character(df.profile.quab$Result))
df.profile.wach$Result <- as.numeric(as.character(df.profile.wach$Result))

# make Date date Class
df.trib.res.quab$Date <- as.Date(as.character(df.trib.res.quab$Date),format ='%m/%d/%Y')
df.trib.res.wach$Date <- as.Date(as.character(df.trib.res.wach$SampleDateTime),format ='%Y-%m-%d %H:%M:%S')
df.profile.quab$Date <- as.Date(as.character(df.profile.quab$Date), format = '%d-%b-%y')
df.profile.wach$Date <- as.Date(df.profile.wach$Date)

# add a time field
df.profile.wach$Time <- format(df.profile.wach$Time,"%H:%M:%S")
df.profile.wach$DateTime <-as.POSIXct(paste(df.profile.wach$Date, df.profile.wach$Time), format="%Y-%m-%d %H:%M:%S")

#make flag data factor and have "No flag for blank"
df.trib.res.wach$FlagCode <- as.character(df.trib.res.wach$FlagCode)
df.trib.res.wach$FlagCode[is.na(df.trib.res.wach$FlagCode)] <- "No Flag"
df.trib.res.wach$FlagCode[is.nan(df.trib.res.wach$FlagCode)] <- "No Flag"
df.trib.res.wach$FlagCode <- factor(df.trib.res.wach$FlagCode)

#### Edit Site Tables
# rename column of Sites
df.quab.wach.site <- df.quab.wach.site %>% rename(Site = SiteID)
df.wach.site <- df.wach.site %>% rename(Site = LocationMWRA)

df.quab.wach.site <- df.quab.wach.site %>% mutate(LocationLabel = paste(LocationShortName, Site))
df.quab.wach.site$LocationElevFt <- NA
df.quab.wach.site <- df.quab.wach.site %>% rename(LocationDescription = SiteDescription)
df.quab.wach.site <- df.quab.wach.site %>% select(-Description)
###


# make a dplyr combine with the Sites (choose by trib recieving body (quab, ware, wach) and by (Core or EQA)
df.trib.res.quab <- left_join(df.trib.res.quab, df.quab.wach.site, "Site")
#df.trib.res.wach <- left_join(df.trib.res.wach, df.sites.wach, "Site")

#############################################################################################################################
# Create final Dataframes for App

### Tributaries

# Quabbin Tributary
df.trib.quab <- filter(df.trib.res.quab, Type == "Tributary", Watershed == "Quabbin")

# Ware River Tributary
df.trib.ware <<- filter(df.trib.res.quab, Type == "Tributary", Watershed == "Ware River")

# Wachusett Tributary
#df.trib.wach <- filter(df.trib.res.wach, Type == "Tributary")
df.trib.wach <- df.trib.res.wach

# All Tributaries
df.trib.all <- bind_rows(df.trib.quab, df.trib.ware, df.trib.wach)

### Reservoirs

# Quabbin Reservoir
df.res.quab <- readRDS("C:/Users/nick/Desktop/Shiny App/Data/df.res.quab.rds")
df.res.quab <- rename(df.res.quab, Result = Value, Site = SITE)
#df.res.quab <- filter(df.trib.res.quab, Type == "Reservoir")

# Wachusett Tributary
#df.res.wach <- filter(df.trib.res.wach, Type == "Reservoir")
df.res.wach <- df.trib.res.wach

### Profiles (No work needed)

# df.profile.quab
# df.profile.wach

### Site Locations

# Split Ware and Quabbin

df.quab.site <- df.quab.wach.site %>% filter(Watershed == "Quabbin")
df.ware.site <- df.quab.wach.site %>% filter(Watershed == "Ware River")





