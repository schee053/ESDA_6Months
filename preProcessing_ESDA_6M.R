# Purpose        : Preprocess Charge point and Charge session data for use for ESDA tool;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : Finished
# Last update    : 12-01-2015
# Note           : Mannualy put charge data into workspace directory and save as CSV-file.


# Set directory
mainDir <- "M:/My Documents/ESDA_NL_ThesisTool/ESDA_6Months"
dataDir <- "Datasets"
outputDir <- "Output"
dir.create(file.path(mainDir,dataDir), showWarnings = FALSE)
dir.create(file.path(mainDir, outputDir), showWarnings = FALSE)
setwd(file.path(mainDir, dataDir))

# Set symstem language/time/location.
Sys.setlocale("LC_ALL","English")

# Download and open required packages
if (!require(plyr)) install.packages('plyr')
if (!require(RCurl)) install.packages('RCurl')
if (!require(chron)) install.packages('chron')
if (!require(lubridate)) install.packages('lubridate')

#-------------------------------------------------------------------------------------------  
# pre-process Charge Point Dataset (latitude/longitude)
#-------------------------------------------------------------------------------------------
get.stations <- function(webaddress, file.name){
  # Download Charge point dataset
  download.file(webaddress,destfile=file.name,method="libcurl")
  Stations <- read.csv(file.name, header = T, sep=";")
  # Remove white space from PostalCode
  Stations$PostalCode <- gsub(" ", "", Stations$PostalCode, fixed = T)
  # Create address for join opperation
  Stations$Address <- paste(Stations$Street, Stations$HouseNumber, Stations$PostalCode, sep=" ")
  # Remove double entries based on unique values in column "CPExternalID" 
  Stations <- Stations[ !duplicated(Stations["CPExternalID"]),]
  # Write to csv 
  write.csv(Stations, file= paste(file.name, "csv", sep = "."))
  return (Stations)
} 

Stations2015 <- get.stations("https://api.essent.nl/generic/downloadChargingStations?latitude_low=52.30567123031878&longtitude_low=4.756801078125022&latitude_high=52.43772606594848&longtitude_high=5.086390921875022&format=CSV", "ChargeStations")
#-------------------------------------------------------------------------------------------  
# pre-process Nuon charge session dataset
#-------------------------------------------------------------------------------------------

# Mannualy put charge data into workspace directory and save as CSV-file!!
list.files()

# Split (subset) Nuon files
NuonSplit <- read.csv("rapportage_verbruiksdata 201301 + 201306.csv", header = T, sep=",")
NuonSplit$Begin_CS <- as.POSIXct(paste(NuonSplit$Start), format="%d-%m-%Y %H:%M", tz = "GMT")
NuonSplit$End_CS <- as.POSIXct(paste(NuonSplit$Eind), format="%d-%m-%Y %H:%M",  tz = "GMT")
NuonSplitJan2013 <- subset(NuonSplit, Begin_CS < as.POSIXct("2013-06-01 00:00"))
NuonSplitJune2013 <- subset(NuonSplit, Begin_CS >= as.POSIXct("2013-06-01 00:00"))
write.csv(NuonSplitJan2013, file= paste("NuonSplitJan2013", "csv", sep = "."))
write.csv(NuonSplitJune2013, file= paste("NuonSplitJune2013", "csv", sep = "."))

prep_NUON <- function (csv.file, obj.name){
  # Read csv files and create R-objects
  NuonRaw <- read.csv(csv.file, header = T, sep=",")
  NuonRaw$Begin_CS <- as.POSIXct(paste(NuonRaw$Begin_CS), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
  NuonRaw$End_CS <- as.POSIXct(paste(NuonRaw$End_CS), format="%Y-%m-%d %H:%M:%S",  tz = "GMT")
  
  # Remove double sessions  
  NuonRaw <- NuonRaw[ !duplicated(NuonRaw["Sessie"]),] # Why are there double sessions in the first place?
  
  # Set date and time 
  # NuonRaw$Begin_CS <- as.POSIXct(paste(NuonRaw$Start), format="%d-%m-%Y %H:%M", tz = "GMT")
  # NuonRaw$End_CS <- as.POSIXct(paste(NuonRaw$Eind), format="%d-%m-%Y %H:%M",  tz = "GMT")
  
  # Add weekdays column
  NuonRaw$Weekday <- weekdays(as.Date(NuonRaw$Begin_CS, "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  NuonRaw$DayHour <- strftime(NuonRaw$Begin_CS, format = "%H")
  NuonRaw$Day <- strftime(NuonRaw$Begin_CS, format = "%d")
  NuonRaw$Week <- strftime(NuonRaw$Begin_CS, format = "%W")
  NuonRaw$Month <- strftime(NuonRaw$Begin_CS, format = "%m")
  NuonRaw$Year <- strftime(NuonRaw$Begin_CS, format = "%Y")
  NuonRaw$YearDay <- yday(NuonRaw$Begin_CS)
  
  # Rename columns: 
  names(NuonRaw)[names(NuonRaw)=="Straat"] <- "Street"
  names(NuonRaw)[names(NuonRaw)=="Huisnummer"] <- "HouseNumber"
  names(NuonRaw)[names(NuonRaw)=="Postcode"] <- "PostalCode"
  names(NuonRaw)[names(NuonRaw)=="Laadtijd"] <- "ConnectionTime"
  names(NuonRaw)[names(NuonRaw)=="Sessie"] <- "Session_ID"
  names(NuonRaw)[names(NuonRaw)=="kWh"] <- "kWh_total"
  
  # Remove white space from PostalCode
  NuonRaw$PostalCode <- gsub(" ", "", NuonRaw$PostalCode, fixed = T)
  
  # Remove string variables in connection time
  NuonRaw$ConnectionTime <- gsub("h", ":", NuonRaw$ConnectionTime, fixed = T)
  NuonRaw$ConnectionTime <- gsub("min", "", NuonRaw$ConnectionTime, fixed = T)
 
  # Add ConnectionTime in seconds
  NuonRaw$ConnectionTime <- paste(NuonRaw$ConnectionTime, "00", sep = ":")
  NuonRaw$ConnectionTime <- as.character.Date(NuonRaw$ConnectionTime, format= "%H:%M:%S", tz="GMT")
  
  toSeconds <- function(x){
    if (!is.character(x)) stop("x must be a character string of the form H:M:S")
    if (length(x)<=0)return(x)
    
    unlist(
      lapply(x,
             function(i){
               i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
               if (length(i) == 3) 
                 i[1]*3600 + i[2]*60 + i[3]
               else if (length(i) == 2) 
                 i[1]*60 + i[2]
               else if (length(i) == 1) 
                 i[1]
             }  
      )  
    )  
  } 
  
  NuonRaw$timeSec <- toSeconds(NuonRaw$ConnectionTime)
  NuonRaw$timeMin <- (NuonRaw$timeSec/60)
  
  # Remove sessions of 0 seconds (failed sessions)
  NuonRaw <- subset(NuonRaw, timeSec >= 60)

  # Calculate kWh per minute
  NuonRaw$kWh_per_min <- ((NuonRaw$kWh_total/NuonRaw$timeSec)*60) 
  NuonRaw$kWh_per_min <- round(NuonRaw$kWh_per_min,digits=3)
  NuonRaw$kWh_total <- round(NuonRaw$kWh_total,digits=2)
  
  # Join Charge data with xy-coordinates
  NuonRaw$Address <- paste(NuonRaw$Street, NuonRaw$HouseNumber, NuonRaw$PostalCode, sep=" ")
  NuonRaw.Stations <- join(NuonRaw, Stations, by="Address", type = "left")
  
  # Remove duplicates in joined file 
  NuonRaw.Sessions <- NuonRaw.Stations[ !duplicated(NuonRaw.Stations["Session_ID"]),]
  
  # Remove NA values in Latitude column 
  NuonRaw.Sessions <- NuonRaw.Sessions[!is.na(NuonRaw.Sessions$Latitude),] # Many failed matches (2778!) 
  #Maybe because of case sensitive join opperation?
  #View(NuonRaw)
  
  #Create pointID
  NuonRaw.Sessions$pointID <- paste(NuonRaw.Sessions$Longitude, NuonRaw.Sessions$Latitude, sep = "")
  NuonRaw.Sessions$pointID <- as.character(NuonRaw.Sessions$pointID)
  
  # Remove unnecessary columns
  keep <- c("Session_ID", "Begin_CS", "End_CS", "kWh_per_min", "ConnectionTime", "timeMin", "kWh_total", "Weekday", "DayHour", "Day", "Month", "Week", "Year", "Street", "HouseNumber", "PostalCode", "Address", "Latitude", "Longitude", "Provider", "YearDay", "pointID")
  NuonClean <- NuonRaw.Sessions[keep]
  
  # Write to csv and return object
  write.csv(NuonClean, file= paste(obj.name, "csv", sep = "."))
  return(NuonClean)
} 

# Run function
Nuon_January2013 <- prep_NUON("NuonSplitJan2013.csv", "Nuon_January2013")
Nuon_June2013 <- prep_NUON("NuonSplitJune2013.csv", "Nuon_June2013") 
list.files()
Nuon_January2015 <-prep_NUON("Nuon_01_2015.csv", "Nuon_January2015")

#-------------------------------------------------------------------------------------------  
# pre-process Essent charge session dataset
#-------------------------------------------------------------------------------------------
# Mannualy put charge data into workspace directory and save as CSV-file!!
list.files()

prep_ESSENT <- function(csv.file, obj.name){
  # Read CSV file
  EssentRaw <- read.csv(csv.file,  header = T, sep=",")
  
  # Extract the sum of sessions
  EssentRaw$IS_SUM_RECORD <- as.character(EssentRaw$IS_SUM_RECORD)
  EssentRaw <- subset(EssentRaw, IS_SUM_RECORD == "X")
  
  # Set date and time 
  EssentRaw$Begin_DA <- as.character(EssentRaw$BEGIN_LOAD_DATE)
  EssentRaw$Begin_TI <- as.character(EssentRaw$BEGIN_LOAD_TIME)
  EssentRaw$End_DA <- as.character(EssentRaw$END_LOAD_DATE)
  EssentRaw$End_TI <- as.character(EssentRaw$END_LOAD_TIME)
  EssentRaw$Begin_CS <- as.POSIXct(paste(EssentRaw$Begin_DA, EssentRaw$Begin_TI), format="%d.%m.%Y %H:%M:%S", tz = "GMT")
  EssentRaw$End_CS <- as.POSIXct(paste(EssentRaw$End_DA, EssentRaw$End_TI), format="%d.%m.%Y %H:%M:%S",  tz = "GMT")
  
  # Remove sessions from December 2012
  EssentRaw <- subset(EssentRaw, Begin_CS >= as.POSIXct("2013-01-01 00:00"))
  
  # Add weekdays column
  EssentRaw$Weekday <- weekdays(as.Date(EssentRaw$Begin_CS, "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  EssentRaw$DayHour <- strftime(EssentRaw$Begin_CS, format = "%H")
  EssentRaw$Day <- strftime(EssentRaw$Begin_CS, format = "%d")
  EssentRaw$Week <- strftime(EssentRaw$Begin_CS, format = "%W")
  EssentRaw$Month <- strftime(EssentRaw$Begin_CS, format = "%m")
  EssentRaw$Year <- strftime(EssentRaw$Begin_CS, format = "%Y")
  EssentRaw$YearDay <- yday(EssentRaw$Begin_CS)

  # Convert energy from factor to numeric
  EssentRaw$ENERGIE <- as.character(EssentRaw$ENERGIE)
  EssentRaw$ENERGIE <- gsub(",", "", EssentRaw$ENERGIE, fixed = TRUE)
  EssentRaw$ENERGIE <- as.numeric(EssentRaw$ENERGIE)
  EssentRaw$ENERGIE <- (EssentRaw$ENERGIE / 10000)
  
  # Rename columns: 
  names(EssentRaw)[names(EssentRaw)=="STREET"] <- "Street"
  names(EssentRaw)[names(EssentRaw)=="HOUSE_NUM1"] <- "HouseNumber"
  names(EssentRaw)[names(EssentRaw)=="POST_CODE1"] <- "PostalCode"
  names(EssentRaw)[names(EssentRaw)=="CHARGE_DURATION"] <- "ConnectionTime"
  names(EssentRaw)[names(EssentRaw)=="ENERGIE"] <- "kWh_total"
  names(EssentRaw)[names(EssentRaw)=="UNIQUE_ID"] <- "Session_ID"
  
  # Remove white space from PostalCode
  EssentRaw$PostalCode <- as.character(EssentRaw$PostalCode)
  EssentRaw$PostalCode <- gsub(" ", "", EssentRaw$PostalCode, fixed = T)
  
  # Add ConnectionTime in seconds
  EssentRaw$ConnectionTime <- as.character.Date(EssentRaw$ConnectionTime, format= "%H:%M:%S", tz="GMT")
  
  toSeconds <- function(x){
    if (!is.character(x)) stop("x must be a character string of the form H:M:S")
    if (length(x)<=0)return(x)
    
    unlist(
      lapply(x,
             function(i){
               i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
               if (length(i) == 3) 
                 i[1]*3600 + i[2]*60 + i[3]
               else if (length(i) == 2) 
                 i[1]*60 + i[2]
               else if (length(i) == 1) 
                 i[1]
             }  
      )  
    )  
  } 
  
  EssentRaw$timeSec <- toSeconds(EssentRaw$ConnectionTime)
  EssentRaw$timeMin <- (EssentRaw$timeSec/60)
  
  # Remove sessions of 0 seconds (failed sessions)
  EssentRaw <- subset(EssentRaw, timeSec >= 60)
  
  # Calculate kWh per minute
  EssentRaw$kWh_per_min <- ((EssentRaw$kWh_total/EssentRaw$timeSec)*60) 
  EssentRaw$kWh_per_min <- round(EssentRaw$kWh_per_min,digits=3)
  EssentRaw$kWh_total <- round(EssentRaw$kWh_total,digits=2)
  
  # Join Charge data with xy-coordinates
  EssentRaw$Address <- paste(EssentRaw$Street, EssentRaw$HouseNumber, EssentRaw$PostalCode, sep=" ")
  EssentRaw.Stations <- join(EssentRaw, Stations, by="Address", type = "left", match = "all")
  
  # Remove duplicates in joined file 
  EssentRaw.Stations$REMOVE_ID <- paste(EssentRaw.Stations$Session_ID, EssentRaw.Stations$METER_READ_BEGIN, EssentRaw.Stations$Address)
  EssentRaw.Sessions <- EssentRaw.Stations[ !duplicated(EssentRaw.Stations["REMOVE_ID"]),]
  # Not the right combination of joins! --> find out where the duplicates come from! 
  
  # Remove NA values in Latitude column 
  EssentRaw.Sessions <- EssentRaw.Sessions[!is.na(EssentRaw.Sessions$Latitude),] 
  
  #Create pointID
  EssentRaw.Sessions$pointID <- paste(EssentRaw.Sessions$Longitude, EssentRaw.Sessions$Latitude, sep = "")
  EssentRaw.Sessions$pointID <- as.character(EssentRaw.Sessions$pointID)
  
  # Remove unnecessary columns
  keep <- c("Session_ID", "Begin_CS", "End_CS", "kWh_per_min", "ConnectionTime", "timeMin", "kWh_total", "Weekday", "DayHour", "Day", "Month", "Week", "Year", "Street", "HouseNumber", "PostalCode", "Address", "Latitude", "Longitude", "Provider", "YearDay", "pointID")
  EssentClean <- EssentRaw.Sessions[keep]
  
  # Write to csv and return object
  write.csv(EssentClean, file = paste(obj.name, "csv", sep =".")) 
  return (EssentClean)
}

# Run function
Essent_January2013 <- prep_ESSENT("exp_201301-62014.csv", "Essent_January2013")
Essent_June2013 <- prep_ESSENT("exp_201306-62014.csv", "Essent_June2013")

#-------------------------------------------------------------------------------------------  
# Merge providers per month
#-------------------------------------------------------------------------------------------
AdamJanuary2013 <- rbind(Nuon_January2013, Essent_January2013)
write.csv(AdamJanuary2013, file = "AdamJanuary2013.csv")
AdamJune2013 <- rbind(Nuon_June2013, Essent_June2013)
write.csv(AdamJune2013, file = "AdamJune2013.csv")

#-------------------------------------------------------------------------------------------  
# Subset data per week
#-------------------------------------------------------------------------------------------
# Create week identifier based on week of the year (you want subset per week)
# Make a list of objects. Output is the list! 
# List will become input for plotKML function. For each item in the list, for length of the list, run the function.

splitWeek <- function (obj){
  obj$weekID <- paste(obj$Week, obj$Year, sep = ".")
  uniq <- unique(unlist(obj$weekID))
  x <- list()
  for (i in 1:length(uniq)) {
    name <- paste("Week",uniq[i],sep=".")
    y <- assign(name, subset(obj, weekID == uniq[i]))
    x[[name]] <- y
  }
  return (x)
}

JanuaryWeekList <- splitWeek(AdamJanuary2013)
JuneWeekList <- splitWeek(AdamJune2013)
length(JuneWeekList)

# #To create each as seperate objects (not as a list):
AdamJune2013$WeekNr <- strftime(AdamJune2013$Begin_CS, format = "%W")
AdamJune2013$Year <- strftime(AdamJune2013$Begin_CS, format = "%Y")
AdamJune2013$weekID <- paste(AdamJune2013$WeekNr, AdamJune2013$Year, sep = ".")
uniq <- unique(unlist(AdamJune2013$weekID))
for (i in 1:length(uniq)) {
  assign(paste("Week",uniq[i],sep="."), subset(AdamJune2013, weekID == uniq[i]))
}

AdamJanuary2013$WeekNr <- strftime(AdamJanuary2013$Begin_CS, format = "%W")
AdamJanuary2013$Year <- strftime(AdamJanuary2013$Begin_CS, format = "%Y")
AdamJanuary2013$weekID <- paste(AdamJanuary2013$WeekNr, AdamJanuary2013$Year, sep = ".")
uniq <- unique(unlist(AdamJanuary2013$weekID))
for (i in 1:length(uniq)) {
  assign(paste("Week",uniq[i],sep="."), subset(AdamJanuary2013, weekID == uniq[i]))
}
