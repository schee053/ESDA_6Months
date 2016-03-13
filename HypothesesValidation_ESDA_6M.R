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

#-------------------------------------------------------------------------------------------  
# Read datafiles with new locations and put them in a list.
#------------------------------------------------------------------------------------------- 
# Set directory
mainDir <- "E:/Raw_ChargeSession_Datasets"
dataDir <- "Datasets"
outputDir <- "Output"
dir.create(file.path(mainDir,dataDir), showWarnings = FALSE)
dir.create(file.path(mainDir, outputDir), showWarnings = FALSE)
setwd(file.path(mainDir, outputDir))

read.location <- function(x){
  csv <- read.csv(x, header = TRUE, sep = ",")
  return(csv)
}

January2013_location <- read.location("January2013_complete.csv")
June2013_location <- read.location("June2013_complete.csv")
November2014_location <- read.location("November2014_complete.csv")
January2015_location <- read.location("January2015_complete.csv")
August2015_location <- read.location("August2015_complete.csv")
January2016_location <- read.location("January2016_complete.csv")

AdamList2 <- list(January2013_location, June2013_location, November2014_location, January2015_location, August2015_location, January2016_location)

#-------------------------------------------------------------------------------------------  
# Observation: In Buikslotermeer large amounts of kWh are charged on Thursdays and Fridays. 
#------------------------------------------------------------------------------------------- 

Buikslotermeer <- function(x){
  Adam <- x
  Adam <- subset(Adam, Adam$buurtcom00 == "Buikslotermeer")
  # Subset days
  Adam.Monday <- subset(Adam, (Adam$Weekday == "Monday")) 
  Adam.Tuesday <- subset(Adam, (Adam$Weekday == "Tuesday"))
  Adam.Wednesday <- subset(Adam, (Adam$Weekday == "Wednesday"))
  Adam.Thursday <- subset(Adam, (Adam$Weekday == "Thursday"))
  Adam.Friday <- subset(Adam, (Adam$Weekday == "Friday"))
  Adam.Saturday <- subset(Adam, (Adam$Weekday == "Saturday"))
  Adam.Sunday <- subset(Adam, (Adam$Weekday == "Sunday"))
  # What is the average kWh of the dataset?
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

Buikslotermeer.check <- lapply(AdamList2, Buikslotermeer)
Buikslotermeer.check 

#-------------------------------------------------------------------------------------------  
# Observation: IJburg-West has very little sessions on Sundays. 
#------------------------------------------------------------------------------------------- 
IJburg <- function(x){
  Adam <- x
  Adam <- subset(Adam, Adam$buurtcom00 == "IJburg West")
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

IJburg.check <- lapply(AdamList2, IJburg)
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

JanuaryWeekList <- splitWeek(January2013_location)
str(JanuaryWeekList)

IJburgWeekcheck <- lapply(JanuaryWeekList, IJburg)
IJburgWeekcheck

#-------------------------------------------------------------------------------------------  
# Observation: The most efficient sessions are in Nieuw-West.
#------------------------------------------------------------------------------------------- 

NieuwWest <- function(x){
  Adam <- x
  # Subset districts
  Adam.Centrum <- subset(Adam, (Adam$stadsdee00 == "Centrum")) 
  Adam.Noord <- subset(Adam, (Adam$stadsdee00 == "Noord"))
  Adam.Oost <- subset(Adam, (Adam$stadsdee00 == "Oost"))
  Adam.Zuid <- subset(Adam, (Adam$stadsdee00 == "Zuid"))
  Adam.Zuidoost <- subset(Adam, (Adam$stadsdee00 == "Zuidoost"))
  Adam.West <- subset(Adam, (Adam$stadsdee00 == "West"))
  Adam.Westpoort <- subset(Adam, (Adam$stadsdee00 == "Westpoort"))
  Adam.Nieuw_West <- subset(Adam, (Adam$stadsdee00 == "Nieuw-West"))
  # What is the average efficiency of the whole week?
  Efficiency <- mean(Adam$kWh_per_mi)
  # How many sessions are above average on Saturday an the other days?
  Sessions.Centrum <- subset(Adam.Centrum, Adam.Centrum$kWh_per_mi > Efficiency)
  Sessions.Noord <- subset(Adam.Noord, Adam.Noord$kWh_per_mi > Efficiency)
  Sessions.Oost <- subset(Adam.Oost, Adam.Oost$kWh_per_mi > Efficiency)
  Sessions.Zuid <- subset(Adam.Zuid, Adam.Zuid$kWh_per_mi > Efficiency)
  Sessions.Zuidoost <- subset(Adam.Zuidoost, Adam.Zuidoost$kWh_per_mi > Efficiency)
  Sessions.West <- subset(Adam.West, Adam.West$kWh_per_mi > Efficiency)
  Sessions.Westpoort <- subset(Adam.Westpoort, Adam.Westpoort$kWh_per_mi > Efficiency)
  Sessions.Nieuw_West <- subset(Adam.Nieuw_West, Adam.Nieuw_West$kWh_per_mi > Efficiency)
  Count.Centrum <- nrow(Sessions.Centrum)
  Count.Noord <- nrow(Sessions.Noord)
  Count.Oost <- nrow(Sessions.Oost)
  Count.Zuid <- nrow(Sessions.Zuid)
  Count.Zuidoost <- nrow(Sessions.Zuidoost)
  Count.West <- nrow(Sessions.West)
  Count.Westpoort <- nrow(Sessions.Westpoort)
  Count.Nieuw_West <- nrow(Sessions.Nieuw_West)
  Answers <- c(Count.Centrum, Count.Noord, Count.Oost, Count.Zuid, Count.Zuidoost, Count.West, Count.Westpoort, Count.Nieuw_West)
  return(Answers)
}

NieuwWest.check <- lapply(AdamList2, NieuwWest)
NieuwWest.check 

#-------------------------------------------------------------------------------------------  
# Observation: The most efficient sessions are in Nieuw-West.
#------------------------------------------------------------------------------------------- 

Efficiency <- function(x){
  Adam <- x
  # Subset districts
  Adam.Centrum <- subset(Adam, (Adam$stadsdee00 == "Centrum")) 
  Adam.Noord <- subset(Adam, (Adam$stadsdee00 == "Noord"))
  Adam.Oost <- subset(Adam, (Adam$stadsdee00 == "Oost"))
  Adam.Zuid <- subset(Adam, (Adam$stadsdee00 == "Zuid"))
  Adam.Zuidoost <- subset(Adam, (Adam$stadsdee00 == "Zuidoost"))
  Adam.West <- subset(Adam, (Adam$stadsdee00 == "West"))
  Adam.Westpoort <- subset(Adam, (Adam$stadsdee00 == "Westpoort"))
  Adam.Nieuw_West <- subset(Adam, (Adam$stadsdee00 == "Nieuw-West"))
  # What is the average efficiency per city district?
  Efficiency <- mean(Adam$kWh_per_mi)
  Efficiency.Centrum <- mean(Adam.Centrum$kWh_per_mi)
  Efficiency.Noord <- mean(Adam.Noord$kWh_per_mi)
  Efficiency.Oost <- mean(Adam.Oost$kWh_per_mi)
  Efficiency.Zuid <- mean(Adam.Zuid$kWh_per_mi)
  Efficiency.Zuidoost <- mean(Adam.Zuidoost$kWh_per_mi)
  Efficiency.West <- mean(Adam.West$kWh_per_mi)
  Efficiency.Westpoort <- mean(Adam.Westpoort$kWh_per_mi)
  Efficiency.Nieuw_West <- mean(Adam.Nieuw_West$kWh_per_mi)
  # How many sessions are above average on Saturday an the other days?
  Diff.Centrum <- round((Efficiency.Centrum - Efficiency), digits = 4)
  Diff.Noord <- round((Efficiency.Noord - Efficiency), digits = 4)
  Diff.Oost <-  round((Efficiency.Oost - Efficiency), digits = 4)
  Diff.Zuid <- round((Efficiency.Zuid - Efficiency), digits = 4)
  Diff.Zuidoost <- round((Efficiency.Zuidoost - Efficiency), digits = 4)
  Diff.West <-  round((Efficiency.West - Efficiency), digits = 4)
  Diff.Westpoort <-  round((Efficiency.Westpoort - Efficiency), digits = 4)
  Diff.Nieuw_West <-  round((Efficiency.Nieuw_West - Efficiency), digits = 4)
  Answers <- c(Diff.Centrum, Diff.Noord, Diff.Oost, Diff.Zuid, Diff.Zuidoost, Diff.West, Diff.Westpoort, Diff.Nieuw_West)
  return(Answers)
}

Efficiency.check <- lapply(AdamList2, Efficiency)
Efficiency.check 

  Adam <- January2013_location
  # Subset districts
  Adam.Centrum <- subset(Adam, (Adam$stadsdee00 == "Centrum")) 
  Adam.Noord <- subset(Adam, (Adam$stadsdee00 == "Noord"))
  Adam.Oost <- subset(Adam, (Adam$stadsdee00 == "Oost"))
  Adam.Zuid <- subset(Adam, (Adam$stadsdee00 == "Zuid"))
  Adam.Zuidoost <- subset(Adam, (Adam$stadsdee00 == "Zuidoost"))
  Adam.West <- subset(Adam, (Adam$stadsdee00 == "West"))
  Adam.Westpoort <- subset(Adam, (Adam$stadsdee00 == "Westpoort"))
  Adam.Nieuw_West <- subset(Adam, (Adam$stadsdee00 == "Nieuw-West"))
  # What is the average efficiency per city district?
  Efficiency <- mean(Adam$kWh_per_mi)
  Efficiency.Centrum <- mean(Adam.Centrum$kWh_per_mi)
  Efficiency.Noord <- mean(Adam.Noord$kWh_per_mi)
  Efficiency.Oost <- mean(Adam.Oost$kWh_per_mi)
  Efficiency.Zuid <- mean(Adam.Zuid$kWh_per_mi)
  Efficiency.Zuidoost <- mean(Adam.Zuidoost$kWh_per_mi)
  Efficiency.West <- mean(Adam.West$kWh_per_mi)
  Efficiency.Westpoort <- mean(Adam.Westpoort$kWh_per_mi)
  Efficiency.Nieuw_West <- mean(Adam.Nieuw_West$kWh_per_mi)
  Sessions.Zuid <- subset(Adam.Zuid, Adam.Zuid$kWh_per_mi > Efficiency)
  # How many sessions are above average on Saturday an the other days?
  Diff.Centrum <- round((Efficiency.Centrum - Efficiency), digits = 4)
  Diff.Noord <- round((Efficiency.Noord - Efficiency), digits = 4)
  Diff.Oost <-  round((Efficiency.Oost - Efficiency), digits = 4)
  Diff.Zuid <- round((Efficiency.Zuid - Efficiency), digits = 4)
  Diff.Zuidoost <- round((Efficiency.Zuidoost - Efficiency), digits = 4)
  Diff.West <-  round((Efficiency.West - Efficiency), digits = 4)
  Diff.Westpoort <-  round((Efficiency.Westpoort - Efficiency), digits = 4)
  Diff.Nieuw_West <-  round((Efficiency.Nieuw_West - Efficiency), digits = 4)

Efficiency
Efficiency.Zuid
Efficiency.Oost  
View(Sessions.Zuid)
#-------------------------------------------------------------------------------------------  
# Percentage of more efficient sessions 
#------------------------------------------------------------------------------------------- 

Perc_efficient <- function(x){
  Adam <- x
  # Subset districts
  Adam.Centrum <- subset(Adam, (Adam$stadsdee00 == "Centrum")) 
  Adam.Noord <- subset(Adam, (Adam$stadsdee00 == "Noord"))
  Adam.Oost <- subset(Adam, (Adam$stadsdee00 == "Oost"))
  Adam.Zuid <- subset(Adam, (Adam$stadsdee00 == "Zuid"))
  Adam.Zuidoost <- subset(Adam, (Adam$stadsdee00 == "Zuidoost"))
  Adam.West <- subset(Adam, (Adam$stadsdee00 == "West"))
  Adam.Westpoort <- subset(Adam, (Adam$stadsdee00 == "Westpoort"))
  Adam.Nieuw_West <- subset(Adam, (Adam$stadsdee00 == "Nieuw-West"))
  # Number of sesions per district
  Count.Centrum <- nrow(Adam.Centrum)
  Count.Noord <- nrow(Adam.Noord)
  Count.Oost <- nrow(Adam.Oost)
  Count.Zuid <- nrow(Adam.Zuid)
  Count.Zuidoost <- nrow(Adam.Zuidoost)
  Count.West <- nrow(Adam.West)
  Count.Westpoort <- nrow(Adam.Westpoort)
  Count.Nieuw_West <- nrow(Adam.Nieuw_West)
  # What is the average efficiency of the whole week?
  Efficiency <- mean(Adam$kWh_per_mi)
  # How many sessions are above average on Saturday an the other days?
  Sessions.Centrum <- subset(Adam.Centrum, Adam.Centrum$kWh_per_mi > Efficiency)
  Sessions.Noord <- subset(Adam.Noord, Adam.Noord$kWh_per_mi > Efficiency)
  Sessions.Oost <- subset(Adam.Oost, Adam.Oost$kWh_per_mi > Efficiency)
  Sessions.Zuid <- subset(Adam.Zuid, Adam.Zuid$kWh_per_mi > Efficiency)
  Sessions.Zuidoost <- subset(Adam.Zuidoost, Adam.Zuidoost$kWh_per_mi > Efficiency)
  Sessions.West <- subset(Adam.West, Adam.West$kWh_per_mi > Efficiency)
  Sessions.Westpoort <- subset(Adam.Westpoort, Adam.Westpoort$kWh_per_mi > Efficiency)
  Sessions.Nieuw_West <- subset(Adam.Nieuw_West, Adam.Nieuw_West$kWh_per_mi > Efficiency)
  Count2.Centrum <- nrow(Sessions.Centrum)
  Count2.Noord <- nrow(Sessions.Noord)
  Count2.Oost <- nrow(Sessions.Oost)
  Count2.Zuid <- nrow(Sessions.Zuid)
  Count2.Zuidoost <- nrow(Sessions.Zuidoost)
  Count2.West <- nrow(Sessions.West)
  Count2.Westpoort <- nrow(Sessions.Westpoort)
  Count2.Nieuw_West <- nrow(Sessions.Nieuw_West)
  # What percentage of sessions is more efficient?
  perc.Centrum <- round(((100*Count2.Centrum)/Count.Centrum), digits = 1)
  perc.Noord <-  round(((100*Count2.Noord)/Count.Noord), digits = 1)
  perc.Oost <-  round(((100*Count2.Oost)/Count.Oost), digits = 1)
  perc.Zuid <-  round(((100*Count2.Zuid)/Count.Zuid), digits = 1)
  perc.Zuidoost <-  round(((100*Count2.Zuidoost)/Count.Zuidoost), digits = 1)
  perc.West <-  round(((100*Count2.West)/Count.West), digits = 1)
  perc.Westpoort <-  round(((100*Count2.Westpoort)/Count.Westpoort), digits = 1)
  perc.Nieuw_West <-  round(((100*Count2.Nieuw_West)/Count.Nieuw_West), digits = 1)
  Answers <- c(perc.Centrum,  perc.Noord, perc.Oost, perc.Zuid, perc.Zuidoost, perc.West, perc.Westpoort, perc.Nieuw_West)
  return(Answers)
}

Perc.eff.check <- lapply(AdamList2, Perc_efficient)
Perc.eff.check

#-------------------------------------------------------------------------------------------  
# Number of sessions per district
#------------------------------------------------------------------------------------------- 
N_cityCenter <- function(x){ 
  Adam <- x
  # Subset districts
  Adam.Centrum <- subset(Adam, (Adam$stadsdee00 == "Centrum")) 
  Adam.Noord <- subset(Adam, (Adam$stadsdee00 == "Noord"))
  Adam.Oost <- subset(Adam, (Adam$stadsdee00 == "Oost"))
  Adam.Zuid <- subset(Adam, (Adam$stadsdee00 == "Zuid"))
  Adam.Zuidoost <- subset(Adam, (Adam$stadsdee00 == "Zuidoost"))
  Adam.West <- subset(Adam, (Adam$stadsdee00 == "West"))
  Adam.Westpoort <- subset(Adam, (Adam$stadsdee00 == "Westpoort"))
  Adam.Nieuw_West <- subset(Adam, (Adam$stadsdee00 == "Nieuw-West"))
  # Number of sesions per district
  Count.Centrum <- nrow(Adam.Centrum)
  Count.Noord <- nrow(Adam.Noord)
  Count.Oost <- nrow(Adam.Oost)
  Count.Zuid <- nrow(Adam.Zuid)
  Count.Zuidoost <- nrow(Adam.Zuidoost)
  Count.West <- nrow(Adam.West)
  Count.Westpoort <- nrow(Adam.Westpoort)
  Count.Nieuw_West <- nrow(Adam.Nieuw_West)
  # # What is the average efficiency of the whole week?
  # Efficiency <- mean(Adam$kWh_per_mi)
  # # How many sessions are above average on Saturday an the other days?
  # Sessions.Centrum <- subset(Adam.Centrum, Adam.Centrum$kWh_per_mi >= Efficiency)
  # Sessions.Noord <- subset(Adam.Noord, Adam.Noord$kWh_per_mi >= Efficiency)
  # Sessions.Oost <- subset(Adam.Oost, Adam.Oost$kWh_per_mi >= Efficiency)
  # Sessions.Zuid <- subset(Adam.Zuid, Adam.Zuid$kWh_per_mi >= Efficiency)
  # Sessions.Zuidoost <- subset(Adam.Zuidoost, Adam.Zuidoost$kWh_per_mi >= Efficiency)
  # Sessions.West <- subset(Adam.West, Adam.West$kWh_per_mi >= Efficiency)
  # Sessions.Westpoort <- subset(Adam.Westpoort, Adam.Westpoort$kWh_per_mi >= Efficiency)
  # Sessions.Nieuw_West <- subset(Adam.Nieuw_West, Adam.Nieuw_West$kWh_per_mi >= Efficiency)
  Count.Centrum <- nrow(Adam.Centrum)
  Count.Noord <- nrow(Adam.Noord)
  Count.Oost <- nrow(Adam.Oost)
  Count.Zuid <- nrow(Adam.Zuid)
  Count.Zuidoost <- nrow(Adam.Zuidoost)
  Count.West <- nrow(Adam.West)
  Count.Westpoort <- nrow(Adam.Westpoort)
  Count.Nieuw_West <- nrow(Adam.Nieuw_West)
  Answers <- c(Count.Centrum, Count.Noord, Count.Oost, Count.Zuid, Count.Zuidoost, Count.West, Count.Westpoort, Count.Nieuw_West)
  return(Answers)
}

N_cityCenter.check <- lapply(AdamList2, N_cityCenter)
N_cityCenter.check 

