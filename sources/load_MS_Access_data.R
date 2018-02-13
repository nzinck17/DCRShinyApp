##############################################################################################################################
#     Title: load_MS_Access_data_R
#     Description: This script will load WQ data from both Quabbin and Wachusett Microsof Access Database
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

# File name path from the Shiny App Folder (***Update if name changed****)

filename_quab <- "DBQ=C:/WQDatabase/QUABBIN_WQDB_fe.mdb"
filename_wach_wq <- "DBQ=C:/WQDatabase/WACHUSETT_WQDB_fe.mdb"
filename_wach_aquabio <- "DBQ=C:/WQDatabase/WACHUSETT_AQBIO_DB_fe.mdb"

##############################################################################################################################
# GET DATA FROM DATABASE *(Connect to database, fetch tables, and close connection)
##############################################################################################################################

### Quabbin Tribs and Res

# Connect to db
con <- dbConnect(odbc::odbc(),
                 .connection_string = paste("driver={Microsoft Access Driver (*.mdb, *.accdb)}", filename_quab, "Uid=Admin;Pwd=;", sep = ";"),
                 timezone = "America/New_York")

# Read Tables
df_trib_res_quab <- dbReadTable(con, "tblWQTribRes2")
df_prof_quab <- dbReadTable(con, "tblWQProfile")
df_quab_ware_site <- dbReadTable(con, "tblSiteLocation2")
df_quab_param <- dbReadTable(con, "tblParameters")

# Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)


### Wachusett Tribs and Reservoir Bact

# Connect to db
con <- dbConnect(odbc::odbc(),
                             .connection_string = paste("driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                              filename_wach_wq, "Uid=Admin;Pwd=;", sep = ";"),
                             timezone = "America/New_York")

# Read Tables
df_trib_bact_wach <- dbReadTable(con, "tblWQALLDATA")
#df_trib_bact_wach <- dbGetQuery(con, paste("SELECT", wq_col_list, "FROM tblWQALLDATA"))
df_wq_wach_site <- dbReadTable(con, "tblLocations")
df_wq_wach_param <- dbReadTable(con, "tblParameters")
df_wq_wach_flag <- dbReadTable(con, "tblFlags")
df_wq_wach_flag_sample <- dbReadTable(con, "tblSampleFlagIndex")

# Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)


### Wachusett Reservoir (Profile, Phytoplankton, Physiochemical)

# Connect to db
con<- dbConnect(odbc::odbc(),
                             .connection_string = paste("driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                              filename_wach_aquabio, "Uid=Admin;Pwd=;", sep = ";"),
                              timezone = "America/New_York")

# Read Tables
df_chem_wach <- dbReadTable(con, "tbl_Nutrients")
df_prof_wach <- dbReadTable(con, "tbl_Profiles")
df_chem_prof_wach_site <- dbReadTable(con, "tblLocations")
df_secchi_wach <- dbReadTable(con, "tblSecchi") # Wachusett Secchi
df_phyto_wach <- dbReadTable(con, "tbl_Phyto") # Wachusett Phytoplankton
df_phyto_thresh_wach <- dbReadTable(con, "tbl_PhytoThresholds") # Wachusett Phytoplankton thresholds
df_aq_wach_site <- dbReadTable(con, "tblLocations")
df_aq_wach_flag_sample <- dbReadTable(con, "tblSampleFlagIndex") # Wachusett Phytoplankton thresholds


# Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)


###########################################################################################################################
# RESTRUCTURING WATER QUALITY DATA
###########################################################################################################################

### QUABBIN

# delete columns
df_trib_res_quab <- df_trib_res_quab %>% select(-rownames)
df_prof_quab <- df_prof_quab %>% select(-rownames)

# result format - Numeric Class (Make Character first in the case that the results are imported as factors)
df_trib_res_quab$Result <- as.numeric(as.character(df_trib_res_quab$Result))
df_prof_quab$Result <- as.numeric(as.character(df_prof_quab$Result))

# date Format - Date Class
df_trib_res_quab$Date <- as.Date(as.character(df_trib_res_quab$Date),format ='%m/%d/%Y')
df_prof_quab$Date <- as.Date(as.character(df_prof_quab$Date), format = '%d-%b-%y')


### WACHUSETT

# rename columns
df_trib_bact_wach <- rename(df_trib_bact_wach, Site = Location, `Result Temp` = ResultReported, Result = FinalResult)
df_chem_wach <- rename(df_chem_wach, Site = Location, `Result Temp` = ResultReported, Result = FinalResult)
df_prof_wach <- rename(df_prof_wach, Date = Pro_Date, Site = Pro_Station, Time = Pro_TimeFormatted, Depthm = Pro_Depth_m)

# reformat the Wachusett Profile data to "Tidy" data format ("Long" instead of "Wide")
df_prof_wach <- gather(df_prof_wach, Parameter, Result, c(Temp_C, SpCond, LDO_pct, LDO_mgL, pH, Chl_ugL, Chl_volts, Turbidity, TDS_mgL, BGA_PC_ugL))

# result format - Numeric Class (Make Character first in the case that the results are imported as factors (could maybe be set within DB))
df_trib_bact_wach$Result <- as.numeric(as.character(df_trib_bact_wach$Result))
df_chem_wach$Result <- as.numeric(as.character(df_chem_wach$Result))
df_prof_wach$Result <- as.numeric(as.character(df_prof_wach$Result))

# date format - Date Class. (Could maybe be set withing DB)
df_trib_bact_wach$SampleDateTime <- format(df_trib_bact_wach$SampleDateTime, tz ="America/New_York", usetz=TRUE) %>%
  as.POSIXct()
df_trib_bact_wach$Date <- as.Date(as.character(df_trib_bact_wach$SampleDateTime),format ='%Y-%m-%d %H:%M:%S')
df_phyto_wach$Phyt_Date <- as.Date(format(df_phyto_wach$Phyt_Date, tz ="America/New_York", usetz=TRUE))
df_secchi_wach$Date <- as.Date(format(df_secchi_wach$Date, tz ="America/New_York", usetz=TRUE))
df_chem_wach$Date <- as.Date(as.character(df_chem_wach$SampleDateTime),format ='%Y-%m-%d %H:%M:%S')
df_prof_wach$Date <- as.Date(df_prof_wach$Date)

# time format
#df_chem_wach$Time <- format(df_chem_wach$Time,"%H:%M:%S") No longer needed because df now has combined date-time stamp
df_prof_wach$Time <- format(df_prof_wach$Time,"%H:%M:%S")
df_prof_wach$DateTime <-as.POSIXct(paste(df_prof_wach$Date, df_prof_wach$Time), format="%Y-%m-%d %H:%M:%S")
#df_secchi_wach$SampleTime <- format(df_secchi_wach$SampleTime, "%H:%M")

# flag format - Change a "NA" or "NAN" value to "No Flag"
df_trib_bact_wach$FlagCode <- as.character(df_trib_bact_wach$FlagCode)
df_trib_bact_wach$FlagCode[is.na(df_trib_bact_wach$FlagCode)] <- "No Flag"
df_trib_bact_wach$FlagCode[is.nan(df_trib_bact_wach$FlagCode)] <- "No Flag"
df_trib_bact_wach$FlagCode <- factor(df_trib_bact_wach$FlagCode)

# Additional formatting for Phyto data
#Purge unwanted columns of data, transpose to long format, change data formats
df_phyto_wach <- select(df_phyto_wach,-Phyt_ID,-Microscope,-Magnification,-Method,-ImportDate,-DataSource, -Analyst, -UniqueID) %>%
 gather("taxa","count",5:82, na.rm =T) %>%
 dplyr::rename(Station = Phyt_Station, Year = Phyt_Year, Date = Phyt_Date, Depthm = Phyt_Depth_m, Taxa = taxa, Result = count) %>%
 select(Station, Year, Date, Depthm, Taxa, Result) %>%
 filter(Result >=0)

df_phyto_wach$Taxa <- as.factor(df_phyto_wach$Taxa)
df_phyto_wach$Station <- as.factor(df_phyto_wach$Station)

# Additional formatting for Phyto data
df_secchi_wach <- df_secchi_wach[,c(2:5)] # Remove unwated columns

###########################################################################################################################
# RESTRUCTURING SITE INFORMATION DATA
###########################################################################################################################

### QUABBIN

# delete columns
df_quab_ware_site <- df_quab_ware_site %>% select(-c(rownames, Description))

# rename columns
df_quab_ware_site <- df_quab_ware_site %>% rename(Site = SiteID, LocationType = Type, LocationDescription = SiteDescription)

# create columns
df_quab_ware_site <- df_quab_ware_site %>% mutate(LocationLabel = paste(LocationShortName, Site))
df_quab_ware_site$LocationElevFt <- NA

# change "Core" to "Primary Active"
df_quab_ware_site$LocationCategory <- as.character(df_quab_ware_site$LocationCategory)
df_quab_ware_site$LocationCategory[df_quab_ware_site$LocationCategory == "Core"] <- "Primary Active"


### WACHUSETT

# rename columns
df_wq_wach_site <- df_wq_wach_site %>% rename(Site = LocationMWRA)
df_chem_prof_wach_site <- df_chem_prof_wach_site %>% rename(Site = LocationMWRA, LocationDescription = StationDescription)

# create columns
df_chem_prof_wach_site$LocationElevFt <- NA
df_wq_wach_site$Watershed <- "Wachusett"
df_chem_prof_wach_site$Watershed <- "Wachusett"


###########################################################################################################################
# Combine WQ with Site info (Gets the Station and Sampling Level for Chemical Sites)
###########################################################################################################################

# Quabbin Tributary and Chemical
df_trib_res_quab <- left_join(df_trib_res_quab, df_quab_ware_site, by = "Site")

# Quabbin Profile
df_prof_quab <- left_join(df_prof_quab, df_quab_ware_site, by = "Site")

# Wachusett Tributary and Bacteria
df_trib_bact_wach <- left_join(df_trib_bact_wach, df_wq_wach_site, by = "Site")

# Wachusett Chemical
df_chem_wach <- left_join(df_chem_wach, df_chem_prof_wach_site, by = "Site")

# Wachusett Profile
df_prof_wach <- left_join(df_prof_wach, df_chem_prof_wach_site, by = "Site")

###########################################################################################################################
# List of Column names to be shown in Trib and Res Module tables and Default Selected in Export Module
###########################################################################################################################

# Trib and Res Columns to read into the App (right now for Wachusett but eventually for Quabbin too)
#wq_col_list <- "ID, UniqueID, Location, SampleDateTime, Parameter, Units, FinalResult, StormSample, StormSampleN, DetectionLimit, FlagCode"

# Quabbin, Ware, and All Tributary
col_trib_quab_ware <- c("LocationLabel", "Date", "Parameter", "Result", "Units", "Site", "LocationCategory")

# Wachusett Tributary
col_trib_wach <- c("LocationLabel", "SampleDateTime", "Date", "Parameter", "Result", "Units","FlagCode", "StormSample", "Site", "LocationCategory")

# Quabbin (Res) Bacteria
col_bact_quab <- c("LocationLabel", "Date", "Sampling_Level", "Parameter", "Result", "Units", "Site", "Station", "LocationCategory")

# Wachusett Res Bacteria
col_bact_wach <- c("LocationLabel", "Date", "Parameter", "Result", "Units", "FlagCode", "StormSample", "Site", "LocationCategory")

# Quabbin (Res) Chemical
col_chem_quab <- c("LocationLabel", "Date", "Sampling_Level", "Parameter", "Result", "Units", "Site", "Station", "LocationCategory")

# Wachusett Res Chemical
col_chem_wach <- c("LocationLabel", "Date", "LocationDepth", "Parameter", "Result", "Units", "FlagCode", "Site", "Station", "LocationCategory")

# Quabbin (Res) Profile
col_prof_quab <- c("LocationLabel", "Date", "Depthm", "Parameter", "Result", "Units", "Site", "Station", "LocationCategory")

# Wachusett Res Profile
col_prof_wach <- c("LocationLabel", "Date", "Depthm", "Parameter", "Result", "Site", "Station", "LocationCategory") # need units??

###########################################################################################################################
# Final Water Quality Dataframes
###########################################################################################################################

# Quabbin Tributary
df_trib_quab_exp <- df_trib_res_quab %>% filter(LocationType == "Tributary", Watershed == "Quabbin")
df_trib_quab <- df_trib_quab_exp %>% select(col_trib_quab_ware)

# Ware River Tributary
df_trib_ware_exp <- df_trib_res_quab %>% filter(LocationType == "Tributary", Watershed == "Ware River")
df_trib_ware <- df_trib_ware_exp %>% select(col_trib_quab_ware)

# Wachusett Tributary
df_trib_wach_exp <- df_trib_bact_wach %>% filter(LocationType == "Tributary")
df_trib_wach <- df_trib_wach_exp %>% select(col_trib_wach)

# All Tributaries
df_trib_all_exp <- bind_rows(df_trib_quab, df_trib_ware, df_trib_wach)
df_trib_all <- df_trib_all_exp %>%  select(col_trib_quab_ware)

# Wachusett Bacteria
df_bact_wach_exp <- df_trib_bact_wach %>% filter(LocationType == "Transect")
df_bact_wach <- df_bact_wach_exp %>% select(col_bact_wach)

# Quabbin Chemical
df_chem_quab_exp <- df_trib_res_quab %>% filter(LocationType == "Nutrient")
df_chem_quab <- df_chem_quab_exp %>% select(col_chem_quab)

# Wachusett Chemical (all set?)
df_chem_wach_exp <- df_chem_wach
df_chem_wach <- df_chem_wach_exp  %>% select(col_chem_wach)

# Quabbin Profile (all set?)
df_prof_quab_exp <- df_prof_quab
df_prof_quab <- df_prof_quab_exp %>% select(col_prof_quab)

# Wachusett Profile (all set?)
df_prof_wach_exp <- df_prof_wach
df_prof_wach <- df_prof_wach_exp  %>% select(col_prof_wach)

# Wachusett Phytoplankton
#df_phyto_wach <- df_phyto_wach

###########################################################################################################################
# Final Site/Location Information Dataframes
###########################################################################################################################

# Quabbin Tributary
df_trib_quab_site <- df_quab_ware_site %>% filter(Watershed == "Quabbin", LocationType == "Tributary")

# Ware River Tributary
df_trib_ware_site <- df_quab_ware_site %>% filter(Watershed == "Ware River")

# Wachusett Tributary
df_trib_wach_site <- df_wq_wach_site %>% filter(LocationType == "Tributary")

# Wachusett Bacteria
df_bact_wach_site <- df_wq_wach_site %>% filter(LocationType == "Transect")

# Quabbin Chemical
df_chem_quab_site <- df_quab_ware_site %>% filter(LocationType == "Nutrient")

# Wachusett Chemical (Maybe in Site Table change location LocationType to Chemical or )
df_chem_wach_site <- df_chem_prof_wach_site %>% filter(!is.na(LocationDepth))

# Quabbin Profile (Need to find Sites)
df_prof_quab_site <- df_chem_quab_site

# Wachusett Profile
df_prof_wach_site <- df_chem_prof_wach_site %>% filter(is.na(LocationDepth))

# Wachusett Phytoplankton

# Just use match function where needed - like in pick lists and chart labels/titles rather than append site data to each record

# All Sites - # Combine All Sites into 1 dataframe ( Need to update when Sites are squared away)
df_all_site_temp <- full_join(df_quab_ware_site,
                              df_wq_wach_site,
                              by = c("Site",
                                     "Watershed",
                                     "LocationType",
                                     "LocationLong",
                                     "LocationLat",
                                     "LocationLabel",
                                     "LocationDescription",
                                     "LocationElevFt"))

df_all_site <- full_join(df_all_site_temp,
                         df_chem_prof_wach_site,
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
df_trib_all_site <- df_all_site %>% filter(LocationType == "Tributary")



