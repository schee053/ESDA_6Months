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
#-------------------------------------------------------------------------------------------  
# Create a list of all charge session datasets
#-------------------------------------------------------------------------------------------  
AdamList <- list(AdamJanuary2013, AdamJune2013, AdamNovember2014, AdamJanuary2015, AdamAugust2015, AdamJanuary2016)

#-------------------------------------------------------------------------------------------  
# Explore the general values of each dataset.
#-------------------------------------------------------------------------------------------  
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
#-------------------------------------------------------------------------------------------  
# Hyptheses: In the weekend there is less variation in charge locations than during the week.
#-------------------------------------------------------------------------------------------  
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

#-------------------------------------------------------------------------------------------  
# Observation: At Julianaplein, many different vehicles charge on Tuesdays. (What is there on Tuesdays?)
#-------------------------------------------------------------------------------------------  
JulianaPlein <- function(x){
  Plein <- subset(x, x$Address == "Julianaplein 1A 1097DN")
  Day <- Mode(Plein$Weekday)
  return(Day)
}

Plein <- lapply(AdamList, JulianaPlein)
Plein

View(AdamJanuary2015)
View(Nuon_August2015)
#-------------------------------------------------------------------------------------------  
# Subset each weekday
#-------------------------------------------------------------------------------------------  

Weekday <- function(x){
  Adam <- x
  Adam.Monday <- subset(Adam, (Adam$Weekday == "Monday")) 
  Adam.Tuesday <- subset(Adam, (Adam$Weekday == "Tuesday"))
  Adam.Wednesday <- subset(Adam, (Adam$Weekday == "Wednesday"))
  Adam.Thursday <- subset(Adam, (Adam$Weekday == "Thursday"))
  Adam.Friday <- subset(Adam, (Adam$Weekday == "Friday"))
  Adam.Saturday <- subset(Adam, (Adam$Weekday == "Saturday"))
  Adam.Sunday <- subset(Adam, (Adam$Weekday == "Sunday"))
  Answer <- c(Adam.Monday, Adam.Tuesday, Adam.Wednesday, Adam.Thursday, Adam.Friday, Adam.Saturday, Adam.Sunday)
  return(Answer)
}

WeekdayList <- lapply(AdamList, Weekday)

#-------------------------------------------------------------------------------------------  
# Observation: The number of efficient sessions is above average on Saturdays. 
#------------------------------------------------------------------------------------------- 
# Conclusion: 
# In November 2014 indeed the number of sessions with an above average efficiency was largest on Saturdays (in absolute numbers).
# If you take the mean of the above average sessions on all other days (so all the above average sessions divided by 6), the number of abover average sessions on Saturday is in almost all cases higher.
# Except for the January 2013, in which the exploration was done...



Saturdays <- function(x){
  Adam <- x
  Adam.Saturdays <- subset(Adam, (Adam$Weekday == "Saturday")) 
  Adam.OtherDays <- subset(Adam, (Adam$Weekday == "Monday" | Adam$Weekday == "Tuesday"| Adam$Weekday == "Wednesday" | Adam$Weekday == "Thursday"| Adam$Weekday == "Friday"| Adam$Weekday == "Sunday"))
  # What is the average efficiency of the whole week?
  Efficiency <- mean(Adam$kWh_per_min)
  # How many sessions are above average on Saturday an the other days?
  Sessions.Saturdays <- subset(Adam.Saturdays, Adam.Saturdays$kWh_per_min > Efficiency)
  Sessions.OtherDays <- subset(Adam.OtherDays, Adam.OtherDays$kWh_per_min > Efficiency)
  Count.Saturdays <- nrow(Sessions.Saturdays)
  Count.OtherDays <- nrow(Sessions.OtherDays)/6
  Check.length <- Count.Saturdays > Count.OtherDays
  Answers <- c(Count.Saturdays, Count.OtherDays, Check.length)
  return(Answers)
}

Saturday.Efficient <- lapply(AdamList, Saturdays)
Saturday.Efficient

Most.Efficient <- function(x){
  Adam <- x
  # Subset days
  Adam.Monday <- subset(Adam, (Adam$Weekday == "Monday")) 
  Adam.Tuesday <- subset(Adam, (Adam$Weekday == "Tuesday"))
  Adam.Wednesday <- subset(Adam, (Adam$Weekday == "Wednesday"))
  Adam.Thursday <- subset(Adam, (Adam$Weekday == "Thursday"))
  Adam.Friday <- subset(Adam, (Adam$Weekday == "Friday"))
  Adam.Saturday <- subset(Adam, (Adam$Weekday == "Saturday"))
  Adam.Sunday <- subset(Adam, (Adam$Weekday == "Sunday"))
  # What is the average efficiency of the whole week?
  Efficiency <- mean(Adam$kWh_per_min)
  # How many sessions are above average on Saturday an the other days?
  Sessions.Monday <- subset(Adam.Monday, Adam.Monday$kWh_per_min >= Efficiency)
  Sessions.Tuesday <- subset(Adam.Tuesday, Adam.Tuesday$kWh_per_min >= Efficiency)
  Sessions.Wednesday <- subset(Adam.Wednesday, Adam.Wednesday$kWh_per_min >= Efficiency)
  Sessions.Thursday <- subset(Adam.Thursday, Adam.Thursday$kWh_per_min >= Efficiency)
  Sessions.Friday <- subset(Adam.Friday, Adam.Friday$kWh_per_min >= Efficiency)
  Sessions.Saturday <- subset(Adam.Saturday, Adam.Saturday$kWh_per_min >= Efficiency)
  Sessions.Sunday <- subset(Adam.Sunday, Adam.Sunday$kWh_per_min >= Efficiency)
  Count.Monday <- nrow(Sessions.Monday)
  Count.Tuesday <- nrow(Sessions.Tuesday)
  Count.Wednesday <- nrow(Sessions.Wednesday)
  Count.Thursday <- nrow(Sessions.Thursday)
  Count.Friday <- nrow(Sessions.Friday)
  Count.Saturday <- nrow(Sessions.Saturday)
  Count.Sunday <- nrow(Sessions.Sunday)
  Check.length <- Count.Saturday > (Count.Monday|Count.Tuesday|Count.Wednesday|Count.Thursday|Count.Friday|Count.Sunday)
  Answers <- c(Count.Monday, Count.Tuesday, Count.Wednesday, Count.Thursday, Count.Friday, Count.Saturday, Count.Sunday, Check.length)
  return(Answers)
}

Efficient.Saturday <- lapply(AdamList, Most.Efficient)
Efficient.Saturday  

#-------------------------------------------------------------------------------------------  
# Observation: Address Ouborg is mainly used on Saturdays. 
#------------------------------------------------------------------------------------------- 
Ouborg <- function(x){
  Adam <- x
  Adam <- subset(Adam, Adam$Address == "Ouborg 7 1083AE")
  # Subset days
  Adam.Monday <- subset(Adam, (Adam$Weekday == "Monday")) 
  Adam.Tuesday <- subset(Adam, (Adam$Weekday == "Tuesday"))
  Adam.Wednesday <- subset(Adam, (Adam$Weekday == "Wednesday"))
  Adam.Thursday <- subset(Adam, (Adam$Weekday == "Thursday"))
  Adam.Friday <- subset(Adam, (Adam$Weekday == "Friday"))
  Adam.Saturday <- subset(Adam, (Adam$Weekday == "Saturday"))
  Adam.Sunday <- subset(Adam, (Adam$Weekday == "Sunday"))
  # How many sessions are on Saturday an the other days?
  Count.Monday <- nrow(Adam.Monday)
  Count.Tuesday <- nrow(Adam.Tuesday)
  Count.Wednesday <- nrow(Adam.Wednesday)
  Count.Thursday <- nrow(Adam.Thursday)
  Count.Friday <- nrow(Adam.Friday)
  Count.Saturday <- nrow(Adam.Saturday)
  Count.Sunday <- nrow(Adam.Sunday)
  Check.length <- Count.Saturday > (Count.Monday|Count.Tuesday|Count.Wednesday|Count.Thursday|Count.Friday|Count.Sunday)
  Answers <- c(Count.Monday, Count.Tuesday, Count.Wednesday, Count.Thursday, Count.Friday, Count.Saturday, Count.Sunday, Check.length)
  return(Answers)
}

Ouborg.check <- lapply(AdamList, Ouborg)
Ouborg.check

#-------------------------------------------------------------------------------------------  
# Observation: IJburg-West has very little sessions on Sundays. 
#------------------------------------------------------------------------------------------- 
IJburg <- function(x){
  Adam <- x
  Adam <- subset(Adam, Adam$Address == "Johan van der Keukenstraat 2 1087BN")
  # Subset days
  Adam.Monday <- subset(Adam, (Adam$Weekday == "Monday")) 
  Adam.Tuesday <- subset(Adam, (Adam$Weekday == "Tuesday"))
  Adam.Wednesday <- subset(Adam, (Adam$Weekday == "Wednesday"))
  Adam.Thursday <- subset(Adam, (Adam$Weekday == "Thursday"))
  Adam.Friday <- subset(Adam, (Adam$Weekday == "Friday"))
  Adam.Saturday <- subset(Adam, (Adam$Weekday == "Saturday"))
  Adam.Sunday <- subset(Adam, (Adam$Weekday == "Sunday"))
  Count.Monday <- nrow(Adam.Monday)
  Count.Tuesday <- nrow(Adam.Tuesday)
  Count.Wednesday <- nrow(Adam.Wednesday)
  Count.Thursday <- nrow(Adam.Thursday)
  Count.Friday <- nrow(Adam.Friday)
  Count.Saturday <- nrow(Adam.Saturday)
  Count.Sunday <- nrow(Adam.Sunday)
  Answers <- c(Count.Monday, Count.Tuesday, Count.Wednesday, Count.Thursday, Count.Friday, Count.Saturday, Count.Sunday)
  return(Answers)
}

IJburg.check <- lapply(AdamList, IJburg)
IJburg.check

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
str(JanuaryWeekList)

IJburgWeekcheck <- lapply(JanuaryWeekList, IJburg)
IJburgWeekcheck

#-------------------------------------------------------------------------------------------  
# Observation: In Buikslotermeer large amounts of kWh are charged on Thursdays and Fridays. 
#------------------------------------------------------------------------------------------- 

Buikslotermeer <- function(x){
  Adam <- x
  Adam <- subset(Adam, Adam$Address == "Buikslotermeerplein 2000 1025XL")
  # Subset days
  Adam.Monday <- subset(Adam, (Adam$Weekday == "Monday")) 
  Adam.Tuesday <- subset(Adam, (Adam$Weekday == "Tuesday"))
  Adam.Wednesday <- subset(Adam, (Adam$Weekday == "Wednesday"))
  Adam.Thursday <- subset(Adam, (Adam$Weekday == "Thursday"))
  Adam.Friday <- subset(Adam, (Adam$Weekday == "Friday"))
  Adam.Saturday <- subset(Adam, (Adam$Weekday == "Saturday"))
  Adam.Sunday <- subset(Adam, (Adam$Weekday == "Sunday"))
  # What is the average efficiency of the whole week?
  kWh_avg <- mean(Adam$kWh_total)
  # How many sessions are above average on Saturday an the other days?
  Sessions.Monday <- subset(Adam.Monday, Adam.Monday$kWh_total >= kWh_avg)
  Sessions.Tuesday <- subset(Adam.Tuesday, Adam.Tuesday$kWh_total >= kWh_avg)
  Sessions.Wednesday <- subset(Adam.Wednesday, Adam.Wednesday$kWh_total >= kWh_avg)
  Sessions.Thursday <- subset(Adam.Thursday, Adam.Thursday$kWh_total >= kWh_avg)
  Sessions.Friday <- subset(Adam.Friday, Adam.Friday$kWh_total >= kWh_avg)
  Sessions.Saturday <- subset(Adam.Saturday, Adam.Saturday$kWh_total >= kWh_avg)
  Sessions.Sunday <- subset(Adam.Sunday, Adam.Sunday$kWh_total >= kWh_avg)
  Count.Monday <- nrow(Sessions.Monday)
  Count.Tuesday <- nrow(Sessions.Tuesday)
  Count.Wednesday <- nrow(Sessions.Wednesday)
  Count.Thursday <- nrow(Sessions.Thursday)
  Count.Friday <- nrow(Sessions.Friday)
  Count.Saturday <- nrow(Sessions.Saturday)
  Count.Sunday <- nrow(Sessions.Sunday)
  Answers <- c(Count.Monday, Count.Tuesday, Count.Wednesday, Count.Thursday, Count.Friday, Count.Saturday, Count.Sunday)
  return(Answers)
}

Buikslotermeer.check <- lapply(AdamList, Buikslotermeer)
Buikslotermeer.check 

#-------------------------------------------------------------------------------------------  
# Observation: On weekdays, relatively more sessions occur during the night, than in the weekend. 
#------------------------------------------------------------------------------------------- 
# Night = session starts between 22:00 and 05:00 (based on work rithm)

Night01 <- function(x){
  Adam <- x
  # Subset the night
  Adam <- subset(Adam, Adam$DayHour >= 23)
  Adam <- subset(Adam, Adam$DayHour <= 06)
  # Subset nights weekday/weekends
  Adam.Weekends <- subset(Adam, (Adam$Weekday == "Saturday" | Adam$Weekday == "Sunday")) 
  Adam.Weekdays <- subset(Adam, (Adam$Weekday == "Monday" | Adam$Weekday == "Tuesday"| Adam$Weekday == "Wednesday" | Adam$Weekday == "Thursday"| Adam$Weekday == "Friday"))
  # Count the average number of sessions per night
  Count.Weekends <- nrow(Adam.Weekends)/2
  Count.Weekdays <- nrow(Adam.Weekdays)/6
  # # Subset nights per day
  # Adam.Monday <- subset(Adam, (Adam$Weekday == "Monday")) 
  # Adam.Tuesday <- subset(Adam, (Adam$Weekday == "Tuesday"))
  # Adam.Wednesday <- subset(Adam, (Adam$Weekday == "Wednesday"))
  # Adam.Thursday <- subset(Adam, (Adam$Weekday == "Thursday"))
  # Adam.Friday <- subset(Adam, (Adam$Weekday == "Friday"))
  # Adam.Saturday <- subset(Adam, (Adam$Weekday == "Saturday"))
  # Adam.Sunday <- subset(Adam, (Adam$Weekday == "Sunday"))
  # # Count the night sessions per day
  # Count.Monday <- nrow(Adam.Monday)
  # Count.Tuesday <- nrow(Adam.Tuesday)
  # Count.Wednesday <- nrow(Adam.Wednesday)
  # Count.Thursday <- nrow(Adam.Thursday)
  # Count.Friday <- nrow(Adam.Friday)
  # Count.Saturday <- nrow(Adam.Saturday)
  # Count.Sunday <- nrow(Adam.Sunday)
  Check.length <- Count.Weekdays > Count.Weekends
  Answers <- c(Count.Weekdays, Count.Weekends, Check.length)
  Check.length <- Count.Weekdays > Count.Weekends
  return(Answers)
}

Night01.check <- lapply(AdamList, Night01)
Night01.check 
