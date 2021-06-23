library(data.table)
library(gtfsFunctions) #devtools::install_github("b-tomhave/gtfsFunctions")
library(tidytransit)
library(dplyr)
{x <- gtfsFunctions::formatGTFSObject("/Users/bentomhave/Documents/Data_GTFS/PreCovid_Fall2019/MetroTransit_MSP_Oct2019.zip")
#MetroTransit_MSP_5_2021
#MetroTransit_MSP

# Filter and Set Column Order for Each Table to only include necessary columns
# Filter Routes & Set Column Order
routes_Keep = c("formattedRouteName", "route_id", "route_short_name", "route_long_name",
                "route_desc", "route_type", "route_url", "agency_id",
                "route_color") # Need color still for mapping
x$routes[ , setdiff(names(x$routes), routes_Keep) := NULL]

# Remove columns in desired column that don't exist and column order based on those remaining
routes_orderedPossibleColumns <- routes_Keep[routes_Keep %in% names(x$routes)]
setcolorder(x$routes, routes_orderedPossibleColumns)

# Filter Routes to only include those that are present in trips file
x$routes <- x$routes[as.character(x$routes$route_id) %in% as.character(unique(x$trips$route_id)),]

# Filter Agency & Set Column Order
agency_Keep = c("agency_id", "agency_name", "agency_url")
x$agency[ , setdiff(names(x$agency), agency_Keep) := NULL]
# Remove columns in desired column that don't exist and column order based on those remaining
agency_orderedPossibleColumns <- agency_Keep[agency_Keep %in% names(x$agency)]
setcolorder(x$agency, agency_orderedPossibleColumns)


# Filter Stops & Set Column Order
stops_Keep = c("stop_id", "stop_name", "stop_desc",
               "stop_lat", "stop_lon", "stop_url")
x$stops[ , setdiff(names(x$stops), stops_Keep) := NULL]
x$stops <- x$stops[gtools::mixedorder(as.character(stop_id))] # Set row order based on stop_id
# Remove columns in desired column that don't exist and column order based on those remaining
stops_orderedPossibleColumns <- stops_Keep[stops_Keep %in% names(x$stops)]
setcolorder(x$stops, stops_orderedPossibleColumns)

# -------------------------------------------------------------------------------------------
# Some transit feeds (i.e. pre-2020 Metro Transit Twin Cities) have an incorrect dash and number
# in route_id resulting in a double count of all route records. This section fixes that error.

# Identify values after initial dash in route_id (if any)
afterDash <- as.character(sapply(strsplit(as.character(x$routes$route_id),"-"),'[',2))

# Get rows where issue (i.e. where there is a value after the dash)
potentialIssueRows <- !(is.na(as.character(sapply(strsplit(as.character(x$routes$route_id),"-"),'[',2))))

# Convert vector to a table counting frequency of each after-dash alue
afterDashTable <- table(afterDash)

# Create named list converting errored dashed route_id (if present) to non-dashed route-id if same value after dash occers 50+ times
if (nrow(table(afterDash)) != 0){
  potentialIssueCorrectedIds <- as.character(ifelse(na.omit(as.numeric(afterDashTable[afterDash]), 0) >= 50,
                                                    as.character(sapply(strsplit(as.character(x$routes$route_id[potentialIssueRows]),"-"),'[',1)),
                                                    as.character(x$routes$route_id[potentialIssueRows])))
  # Create Named list with the potential issue dashed routes AND the fine routes
  old2NonDashedRouteId <- c(potentialIssueCorrectedIds,
                            as.character(x$routes$route_id[!potentialIssueRows]))
  
  names(old2NonDashedRouteId) <- c(as.character(x$routes$route_id[potentialIssueRows]),
                                   as.character(x$routes$route_id[!potentialIssueRows]))
}else{
  old2NonDashedRouteId <- as.character(x$routes$route_id)
  names(old2NonDashedRouteId) <- as.character(x$routes$route_id)
}

# Update old route-id (with dash error if present) with new non-dashed route_id whereever they appear (i.e. routes.txt and trips.txt)
x$routes$route_id <- as.character(old2NonDashedRouteId[x$routes$route_id])
x$trips$route_id <- as.character(old2NonDashedRouteId[x$trips$route_id])

# Get Only Unique Records
x$routes <- x$routes%>%unique()
x$trips  <- x$trips%>%unique()
}

data <- x
test <- na.omit(gtfsFunctions::calculateFrequenciesByRoute(data))


midday <- test[test$period == c("AM Peak", "Midday", "PM Peak"), ]
 
midday2 <- midday[midday$avgHeadway_Mins < 30, ]

# Order 

midday2 <- midday[midday$avgHeadway_Mins < 9000 & midday$direction_id == 0 & midday$period == "AM Peak", ]
midday2$route_id <- factor(midday2$route_id , levels = unique(midday2$route_id)[order(as.numeric(midday2$avgHeadway_Mins))])

# Color code by route type?
fig2 <- plot_ly(midday2,
  x = ~route_id,
  y = ~avgHeadway_Mins,
  type = "bar"
)


fig2

fig <- plot_ly(midday2, x = ~period, y = ~avgHeadway_Mins,
        type = 'scatter', mode = 'lines+markers', linetype = ~route_id) %>%
  layout(title = "Test",
         xaxis = list(title = 'Time of Day'),
         yaxis = list (title = 'Avg. Headway (Minutes)'))

fig
gtfs <- x

departure_time = trip_id = stop_sequence = headway_secs = headway_prev = end_time = start_time = end_time = exact_times = NULL # due to NSE notes in R CMD check
#`.` = function(...) NULL

## Load scheduled times
trips <- gtfs$trips
stop_times <- gtfs$stop_times
routes <- gtfs$routes

# Convert to datatable
setDT(trips)
setDT(stop_times)

# Join trips and stop times
sch <- trips[stop_times, on = 'trip_id']


## Calculate headways by route and direction
setkeyv(sch, c('service_id', 'departure_time'))

# Make Sure departure time is in ITime transit format from which it can be converted to seconds
sch[, departure_time := as.TransitTime(departure_time)]

test1 <- sch[stop_sequence == 1,]
diff(c(1,534,21,5))

unclass(test1$departure_time)

time1 = as.numeric(as.TransitTime("23:40:00"))
time2 = as.numeric(as.TransitTime("04:29:00"))

time2-time1+as.TransitTime("24:00:00")
# If value is negative ad 24hrs because it indicates that value starts before midnight and finishes after midnight
as.integer((time2+as.TransitTime("24:00:00"))-time1)

as.numeric(as.TransitTime("23:40:00"))
as.numeric(as.TransitTime("04:29:00"))

data.table::tstrsplit("23:40:00", ':', type.convert = TRUE, fill = 0L)
## create frequencies.txt table where freq calculated per service_id, route_id and direction_id

ifelse(diff(unclass(sch$departure_time)) < 0, 
       diff(unclass(sch$departure_time)) + as.TransitTime("24:00:00"),
       diff(unclass(sch$departure_time)))
# Timing issue occurs when departure_times run over midnight
freqs <- sch[stop_sequence == 1, .(trip_id,
                                   start_time = departure_time,
                                   end_time = shift(departure_time, 1, type = 'lead'),
                                   secs = departure_time,
                                   headway_secs = c(diff(unclass(departure_time)), NA), # Subtracts current value from last
                                   headway_prev = c(NA, diff(unclass(departure_time))), exact_times = 0L
),
keyby = c('service_id', 'route_id', 'direction_id')]

# fix last trip of each service/route/direction
freqs[is.na(headway_secs), headway_secs := headway_prev]
# fix for only trip of service/route/direction
freqs[is.na(headway_secs), headway_secs := 3600]
freqs[is.na(end_time), end_time := start_time + headway_secs - 1L]
freqs[, headway_prev := NULL]

freqs[, .(trip_id, service_id, direction_id, start_time = start_time, end_time = end_time, headway_secs, exact_times)]


freqs$headway_secs <- ifelse(freqs$headway_secs < 0, 
                             freqs$headway_secs+ as.TransitTime("24:00:00"),
                             freqs$headway_secs)
transitTime2HHMMSS(26940)


tripHeadways <- gtfsFunctions::calculateFrequenciesByTripAndService(data)

tripHeadways$headway_mins <- tripHeadways$headway_secs/60
tripHeadways <- tripHeadways[dplyr::select(data$trips, route_id, trip_id) , on = "trip_id"] # Join with route_ids
tripHeadways <- tripHeadways[data$calendar, on = "service_id"] # Join with calendar to get days of week
# Reference table of seconds after midnight time od day (TOD) table for reference
TOD <- data.table::data.table(BeginTime = c(0,14400,21600,32400,54000,66600,75600,86400),
                              EndTime = c(14400,21600,32400,54000,66600,75600,86400,100800),
                              Period = factor(c("Owl","Early","AM Peak","Midday",
                                                "PM Peak","Evening", "Night","Owl"),
                                              levels = c("Early","AM Peak","Midday", "PM Peak",
                                                         "Evening", "Night", "Owl"),
                                              ordered = T))
# Join time of day categorization to table
tripHeadways <- tripHeadways[TOD, period := Period, on = .(start_time > BeginTime, start_time<= EndTime)]
names(tripHeadways)
simpleTable <- select(tripHeadways, route_id, service_id, direction_id, start_time, headway_mins) 

working <- simpleTable[simpleTable$route_id == "23",]

transitTime2HHMMSS(32400)

testALine <- test[test$route_id == "23",]
testALine$direction_id <- as.factor(testALine$direction_id )
fig <- plot_ly(testALine, x = ~period, type = 'scatter', color = ~direction_id)%>%
  add_trace(y = ~avgHeadway_Mins, name = 'Avg. Headway (Mins)', mode = 'lines+markers')
fig

plot_ly(setorder(test[test$route_id == "94",], direction_id, period),
        x = ~period, y = ~avgHeadway_Mins, type = 'scatter', mode = 'lines+markers', linetype = ~direction_id) %>%
  layout(title = 'Route __ Avg. Headway By Time of Day & Route Direction',
         xaxis = list(title = 'Time of Day'),
         yaxis = list (title = 'Avg. Headway (Minutes)'))



validServiceIds <- unique(tripHeadways[ , .N, by=.(service_id, route_id, direction_id)][
  order(-N), 
  .SD[ N == max(N) ] # subset data table to only show largest occurence
  ,by= .(route_id, direction_id)])                     # includes ties

print(validServiceIds)

# Calculate average headway for route-direction-time of day group. Round down to nearest minute
tripHeadways2 <- tripHeadways[, .(avgHeadway_Mins = floor(mean(headway_mins))), by = c("route_id", "period", "service_id","direction_id")]

tripHeadways2 <- 
#print(unique( tripHeadways2[tripHeadways2$route_id == "921",]$service_id))
#tripHeadways2 <- tripHeadways2[, route_id := gtools::mixedsort(route_id)]

tripHeadways2 <- dplyr::select(tripHeadways2, route_id, direction_id, period, avgHeadway_Mins)%>%dplyr::distinct()
#unique(testing$service_id)
testing <- tripHeadways2[tripHeadways2$route_id == "921",]
testingCorrect <- testing[testing$service_id %in% validServiceIds,]

testing4 <- testing[testing$service_id == "MAR21-MVS-BUS-Weekday-05",]
testing5 <- testing4[testing4$direction_id == 0,]
transitTime2HHMMSS(21600)


TOD <- data.table::data.table(BeginTime = gtfsFunctions::transitTime2HHMMSS(c(0,14400,21600,32400,54000,66600,75600,86400)),
                              EndTime = gtfsFunctions::transitTime2HHMMSS(c(14400,21600,32400,54000,66600,75600,86400,100800)),
                              Period = factor(c("Owl","Early","AM Peak","Midday",
                                                "PM Peak","Evening", "Night","Owl"),
                                              levels = c("Early","AM Peak","Midday", "PM Peak",
                                                         "Evening", "Night", "Owl"),
                                              ordered = T))

TOD$BeginTime <- gtfsFunctions::transitTime2HHMMSS(TOD$BeginTime)





