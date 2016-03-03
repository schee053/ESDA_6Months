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
CopyAdamJan <- AdamJanuary2013

# Hyptheses: In the weekend there is less variation in charge locations than during the week.
AdamJan.Weekends <- subset(CopyAdamJan, (CopyAdamJan$Weekday == "Saturday" | CopyAdamJan$Weekday == "Sunday")) 
AdamJan.Weekdays <- subset(CopyAdamJan, (CopyAdamJan$Weekday == "Monday" | CopyAdamJan$Weekday == "Tuesday"| CopyAdamJan$Weekday == "Wednesday" | CopyAdamJan$Weekday == "Thursday"| CopyAdamJan$Weekday == "Friday"))
Weekends.uniq <- unique(unlist(AdamJan.Weekends$Address))
Weekdays.uniq <- unique(unlist(AdamJan.Weekdays$Address))
length(Weekends.uniq) < length(Weekdays.uniq)


