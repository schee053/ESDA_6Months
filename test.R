
  csv.file <- "Nuon_01_2016.csv"
  Csv.file <- 
  obj.name <- "Nuon_January2016"
  # Read csv files and create R-objects
  NuonRaw <- read.csv(csv.file, header = T, sep=",")
  View(NuonRaw)
  NuonRaw$Start_datetime <- as.POSIXct(paste(NuonRaw$Start_datetime), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
  NuonRaw$End_datetime <- as.POSIXct(paste(NuonRaw$End_datetime), format="%Y-%m-%d %H:%M:%S",  tz = "GMT")
  
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