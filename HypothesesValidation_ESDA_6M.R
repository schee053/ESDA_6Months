# Purpose        : To analyse the dataset in order to test the hypotheses from the ESDA;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : Finished
# Last update    : 12-01-2015
# Note           : Manualy put charge data into workspace directory and save as CSV-file.

# # Find unique
# uniq <- unique(unlist(obj$variableName))
# # Subset
# subs <- subset(obj, Variable == value[i])
# # Merge 
# rbind(objA, objB)
# # Aggregate
# Agg <- aggregate(x = obj, by = list(obj$var1, obj$var2, etc), FUN = mean)
View(AdamJanuary2013)

# Create a list of all charge session datasets
AdamList <- list(AdamJanuary2013, AdamJune2013, AdamNovember2014, AdamJanuary2015, AdamAugust2015, AdamJanuary2016)

# Hyptheses: In the weekend there is less variation in charge locations than during the week.
WkEnd <- function(x){
    Adam <- x
    Adam.Weekends <- subset(Adam, (Adam$Weekday == "Saturday" | Adam$Weekday == "Sunday")) 
    Adam.Weekdays <- subset(Adam, (Adam$Weekday == "Monday" | Adam$Weekday == "Tuesday"| Adam$Weekday == "Wednesday" | Adam$Weekday == "Thursday"| Adam$Weekday == "Friday"))
    Weekends.uniq <- unique(unlist(Adam.Weekends$Address))
    Weekdays.uniq <- unique(unlist(Adam.Weekdays$Address))
    WndCount <- length(Weekends.uniq)
    WkCount <-length(Weekdays.uniq)
    Difference <- length(Weekends.uniq) < length(Weekdays.uniq)
    Answer <- c(WndCount,WkCount,Difference)
    return(Answer)
}

Hypothesis1 <- lapply(AdamList, WkEnd)
Hypothesis1

#------
# Explore the general values of each dataset.
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

DatExpl <- function(x){
  Adam <- x
  Sessions <- nrow(Adam)
  Stations <- length(unique(unlist(Adam$Address)))
  ConTime <- (mean(Adam$timeMin))/60
  kWh <- mean(Adam$kWh_total)
  Efficiency <- mean(Adam$kWh_per_min)
  Weekday <- Mode(AdamJanuary2013$Weekday)
  Address <- Mode(AdamJanuary2013$Address)
  Answers <- c(Sessions, Stations, ConTime, kWh, Efficiency, Weekday, Address)
  return(Answers)
}

Exploration <- lapply(AdamList, DatExpl)
Exploration

# Observation: At Julianaplein, many different vehicles charge on Tuesdays. (What is there on Tuesdays?)
JulianaPlein <- function(x){
  Plein <- subset(x, x$Address == "Julianaplein 1A 1097DN")
  Day <- Mode(Plein$Weekday)
  return(Day)
}

Plein <- lapply(AdamList, JulianaPlein)
Plein

View(AdamJanuary2016)
