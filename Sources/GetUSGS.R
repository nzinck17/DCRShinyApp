library(dataRetrieval)


# Choptank River near Greensboro, MD
siteNumber <- "01174500"  
ChoptankInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"
#Raw daily data:

# Sample data Nitrate:
parameterCd <- "00618"
qwData <- readNWISqw(siteNumber,parameterCd,
                     "1980-01-01","2010-01-01")
pCode <- readNWISpCode(parameterCd)



#USGS Nearby Sites

#East Branch Swift River
site.east.swift <- "01174500"

#West Branch Swift River
site.west.swift <- "01174565"

# WARE RIVER AT INTAKE WORKS NEAR BARRE
site.ware.intake.works <- "01173000"

# WARE RIVER NEAR BARRE, MA
site.ware <- "01172500"

# STILLWATER RIVER NEAR STERLING, MA
site.stillwater <- "01095220"

# QUINAPOXET RIVER AT CANADA MILLS NEAR HOLDEN, MA
site.ware.river <- "01095375"

# GATES BROOK NEAR WEST BOYLSTON, MA
site.ware.river <- "01095434"
 
discharge <- "00060"

rawDailyData <- readNWISdv(site.east.swift,discharge,
                           "1980-01-01","2010-01-01")


#Code for Discharge
pCode <- "00060"

#Code for mean
sCode <- "00003"

start.date <- "2005-01-01"
end.date <- "2015-12-31"

FlowEastread <- readNWISdata(siteNumbers = siteNoEast, parameterCd = pCode,
                             startDate = start.date, endDate = end.date,
                             service = "dv")

FlowWestread <- readNWISdata(siteNumbers = siteNoWest, parameterCd = pCode,
                             startDate = start.date, endDate = end.date,
                             service = "dv")

dfFlowEast1 <- subset(FlowEastread, select =c(site_no,dateTime,X_00060_00003))

dfFlowWest1 <- subset(FlowWestread, select =c(site_no,dateTime,X_00060_00003))

dfFlowEast2 <- dfFlowEast1

dfFlowWest2 <- dfFlowWest1

#For Temporal Plots

colnames(dfFlowEast1)[1] <- "SITE"
colnames(dfFlowEast1)[2] <- "Date"
colnames(dfFlowEast1)[3] <- "Value"

colnames(dfFlowWest1)[1] <- "SITE"
colnames(dfFlowWest1)[2] <- "Date"
colnames(dfFlowWest1)[3] <- "Value"

dfFlowEast1$Parameter <- "Flow"
dfFlowWest1$Parameter <- "Flow"

dfFlowEast1$Year <-  format(dfFlowEast1$Date, format = "%Y")
dfFlowWest1$Year <-  format(dfFlowWest1$Date, format = "%Y")

dfFlow1 <- rbind(dfFlowEast1, dfFlowWest1)

#Binding WQ and Flow (USGS) Data Frames

dfWQsub <- subset(df, SITE == "211" | SITE == "216")

dfFlowSub <- dfFlow1

dfFlowSub$SITE[dfFlowSub$SITE == "01174565"] <- "211"

dfFlowSub$SITE[dfFlowSub$SITE == "01174500"] <- "216"

dfSwiftBind <- rbind(dfWQsub, dfFlowSub)

#Splitting into 3 different parameter groups

dfSwiftParamSet1 <- subset(dfSwiftBind, Parameter == "Flow" | Parameter == "TURB" | Parameter == "TEMPC" 
                           | Parameter == "SPCOND" | Parameter == "pH")

dfSwiftParamSet2 <- subset(dfSwiftBind, Parameter == "Flow" | Parameter == "DOPPM" | Parameter == "DOSAT" | Parameter == "FECCOLI" 
                           | Parameter == "TOTCOLI" | Parameter == "E. coli" | Parameter == "UV254 (ABU/cm)")

dfSwiftParamSet3 <- subset(dfSwiftBind, Parameter == "Flow" | Parameter == "TKN" | Parameter == "NO3." | Parameter == "NO2." 
                           | Parameter == "NH3." | Parameter == "TPH" | Parameter == "Ca..")

#Unused ..... For Other Plots. Alternative way  #dfEastMerge <- merge(dfFlowEast2, dfsub, by = "Date", All = TRUE)

colnames(dfFlowEast2)[1] <- "USGSSITE"
colnames(dfFlowEast2)[2] <- "Date"
colnames(dfFlowEast2)[3] <- "Flow"

colnames(dfFlowWest2)[1] <- "USGSSITE"
colnames(dfFlowWest2)[2] <- "Date"
colnames(dfFlowWest2)[3] <- "Flow"

