###
### VIND NIEUWE API LINK MET LAADSTATIONS 2016!!!
###

# Mannualy put charge data into workspace directory and save as CSV-file!!
list.files()

prep_ESSENT3.0 <- function(csv.file, obj.name){
  # Read CSV file
  csv.file <- "Essent_01_2016.csv"
  obj.name <- "Essent_January2016"
  EssentRaw <- read.csv(csv.file,  header = T, sep=",")
  # Extract the sum of sessions
  EssentRaw$Merkmal.Summenzeile <- as.character(EssentRaw$Merkmal.Summenzeile)
  EssentRaw <- subset(EssentRaw, Merkmal.Summenzeile == "X")
  
  # Set date and time 
  EssentRaw$Begin_DA <- as.character(EssentRaw$Verbindung.Datum.Beginn)
  EssentRaw$Begin_TI <- as.character(EssentRaw$Verbindung.Zeit.Beginn)
  EssentRaw$End_DA <- as.character(EssentRaw$Verbindung.Datum.Ende)
  EssentRaw$End_TI <- as.character(EssentRaw$Verbindung.Zeit.Ende)
  EssentRaw$Begin_CS <- as.POSIXct(paste(EssentRaw$Begin_DA, EssentRaw$Begin_TI), format="%d.%m.%Y %H:%M:%S", tz = "GMT")
  EssentRaw$End_CS <- as.POSIXct(paste(EssentRaw$End_DA, EssentRaw$End_TI), format="%d.%m.%Y %H:%M:%S",  tz = "GMT")
  
  # Add weekdays column
  EssentRaw$Weekday <- weekdays(as.Date(EssentRaw$Begin_CS, "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  EssentRaw$DayHour <- strftime(EssentRaw$Begin_CS, format = "%H")
  EssentRaw$Day <- strftime(EssentRaw$Begin_CS, format = "%d")
  EssentRaw$Week <- strftime(EssentRaw$Begin_CS, format = "%W")
  EssentRaw$Month <- strftime(EssentRaw$Begin_CS, format = "%m")
  EssentRaw$Year <- strftime(EssentRaw$Begin_CS, format = "%Y")
  EssentRaw$YearDay <- yday(EssentRaw$Begin_CS)
  
  # Convert energy from factor to numeric
  EssentRaw$kWh_total <- as.character(EssentRaw$Menge)
  EssentRaw$kWh_total <- gsub(",", "", EssentRaw$kWh_total, fixed = TRUE)
  EssentRaw$kWh_total <- as.numeric(EssentRaw$kWh_total)
  EssentRaw$kWh_total <- (EssentRaw$kWh_total / 10000)
  
  # Rename columns: 
  names(EssentRaw)[names(EssentRaw)=="Stra."] <- "Street"
  names(EssentRaw)[names(EssentRaw)=="Hausnummer"] <- "HouseNumber"
  names(EssentRaw)[names(EssentRaw)=="Postleitzahl"] <- "PostalCode"
  names(EssentRaw)[names(EssentRaw)=="Verbindungsdauer...HH.MM.SS."] <- "ConnectionTime"
  names(EssentRaw)[names(EssentRaw)=="Unique.ID..z.B..RFID...Gutschein."] <- "Session_ID"
  
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
  EssentRaw.Stations <- join(EssentRaw, Stations2015, by="Address", type = "left", match = "all")
  
  # Remove duplicates in joined file 
  EssentRaw.Stations$REMOVE_ID <- paste(EssentRaw.Stations$Session_ID, EssentRaw.Stations$Anfang.ZST..kWh., EssentRaw.Stations$Address)
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
EssentAugust2015 <- prep_ESSENT3.0("Essent_08_2015.csv", "Essent_August2015")
EssentJanuary2016 <- prep_ESSENT3.0("Essent_01_2016.csv", "Essent_January2016")



prep_NUON2.0 <- function (csv.file, obj.name){
  # Read csv files and create R-objects
  NuonRaw <- read.csv(csv.file, header = T, sep=",")

  # Set date and time 
  NuonRaw$Start_datetime <- as.character(NuonRaw$Start_datetime)
  NuonRaw$End_datetime <- as.character(NuonRaw$End_datetime)
  NuonRaw$Start_datetime <- gsub("T", " ",   NuonRaw$Start_datetime, fixed = T)
  NuonRaw$End_datetime <- gsub("T", " ",   NuonRaw$End_datetime, fixed = T)
  NuonRaw$Start_datetime <- as.POSIXct(paste(NuonRaw$Start_datetime), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
  NuonRaw$End_datetime <- as.POSIXct(paste(NuonRaw$End_datetime), format="%Y-%m-%d %H:%M:%S",  tz = "GMT")
  
  # Remove double sessions  
  NuonRaw <- NuonRaw[ !duplicated(NuonRaw["CDR_ID"]),] # Why are there double sessions in the first place?
  
  # Add weekdays column
  NuonRaw$Weekday <- weekdays(as.Date(NuonRaw$Start_datetime, "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  NuonRaw$DayHour <- strftime(NuonRaw$Start_datetime, format = "%H")
  NuonRaw$Day <- strftime(NuonRaw$Start_datetime, format = "%d")
  NuonRaw$Week <- strftime(NuonRaw$Start_datetime, format = "%W")
  NuonRaw$Month <- strftime(NuonRaw$Start_datetime, format = "%m")
  NuonRaw$Year <- strftime(NuonRaw$Start_datetime, format = "%Y")
  NuonRaw$YearDay <- yday(NuonRaw$Start_datetime)
  
  # Rename columns: 
  names(NuonRaw)[names(NuonRaw)=="Charge_Point_Address"] <- "Street"
  names(NuonRaw)[names(NuonRaw)=="Charge_Point_ZIP"] <- "PostalCode"
  names(NuonRaw)[names(NuonRaw)=="Duration"] <- "ConnectionTime"
  names(NuonRaw)[names(NuonRaw)=="CDR_ID"] <- "Session_ID"
  names(NuonRaw)[names(NuonRaw)=="Volume"] <- "kWh_total"
  names(NuonRaw)[names(NuonRaw)=="Infra_Provider_ID"] <- "Provider"
  names(NuonRaw)[names(NuonRaw)=="Start_datetime"] <- "Begin_CS"
  names(NuonRaw)[names(NuonRaw)=="End_datetime"] <- "End_CS"
  
  # Add ConnectionTime in seconds
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
  
  # Remove sessions of 0 minutes (failed sessions)
  NuonRaw <- subset(NuonRaw, timeSec >= 60)
  
  # Calculate kWh per minute
  NuonRaw$kWh_per_min <- ((NuonRaw$kWh_total/NuonRaw$timeSec)*60) 
  NuonRaw$kWh_per_min <- round(NuonRaw$kWh_per_min,digits=3)
  NuonRaw$kWh_total <- round(NuonRaw$kWh_total,digits=2)
  
  # Join Charge data with xy-coordinates
  NuonRaw$Address <- paste(NuonRaw$Street, NuonRaw$PostalCode, sep=" ")
  NuonRaw.Stations2015 <- join(NuonRaw, Stations2015, by="Address", type = "left")
  
  # Remove duplicates in joined file 
  NuonRaw.Sessions <- NuonRaw.Stations2015[ !duplicated(NuonRaw.Stations2015["Session_ID"]),]

  # Remove NA values in Latitude column 
  NuonRaw.Sessions <- NuonRaw.Sessions[!is.na(NuonRaw.Sessions$Latitude),] # Many failed matches(!!! Station addresses are missing!) 
  
    # Remove unnecessary columns
  keep <- c("Session_ID", "Begin_CS", "End_CS", "kWh_per_min", "ConnectionTime", "timeMin", "kWh_total", "Weekday", "DayHour", "Day", "Month", "Week", "Year", "YearDay", "Address", "Latitude", "Longitude", "Provider")
  NuonClean <- NuonRaw.Sessions[keep]
  
  # Write to csv and return object
  write.csv(NuonClean, file= paste(obj.name, "csv", sep = "."))
  return(NuonClean)
} 

NuonJanuary2016 <- prep_NUON2.0("Nuon_01_2016.csv", "Nuon_January2016")
NuonJanuary2015 <- prep_NUON2.0("Nuon_01_2015.csv", "Nuon_January2015")
NuonAugust2015 <- prep_NUON2.0("Nuon_08_2015.csv", "Nuon_August2015")
NuonNovember2014 <- prep_NUON2.0("Nuon_11_2014.csv", "Nuon_November2015")

