# R Shiny App For Health Tracking (Blood pressure and Heartbeat)
##############################################################################
# Libraries
library(shiny)
library(shinyjs) #For javascript collapse box button
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(plotly)
library(tidytransit) # For read_gtfs
library(shinyWidgets) # For pickerInput
library(data.table)
library(gtfsFunctions) #devtools::install_github("b-tomhave/gtfsFunctions")
library(tidytransit)
library(leaflet)
library(sf)
#library(leaflet.extras)
library(gtools) # For mixed sort
library(DT)

#setwd("~/Documents/R Projects/RoutesAndStopsViewer")

# Allow input zip file to be up to 100mb in size
options(shiny.maxRequestSize = 100*1024^2)
##############################################################################
# Functions/ Basic Inputs
##############################################################################

##############################################################################
# Directories
##############################################################################

##############################################################################
# Load Data
##############################################################################


##############################################################################
# UI Side of App
##############################################################################
ui <-navbarPage("Routes & Stops Viewer", id="nav",
           # Map Page
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include custom CSS
                          includeCSS("styles.css")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("routemap", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Routes & Stops Viewer"),
                                      helpText("Initial load time of 5-10 seconds."),
                                      fileInput("selectFile", h4("Select GTFS Zip File:"),
                                                multiple = FALSE,
                                                accept = ".zip"),
                                      uiOutput('routeOptions'),
                                      uiOutput('zoom2Stop')
                        )
                    )
           ),
           # Header Preview Pge
           tabPanel("GTFS Overview",
                    fluidRow(
                        valueBoxOutput("staticBox_modes", width = 2),
                        valueBoxOutput("staticBox_stops", width = 2),
                        valueBoxOutput("staticBox_routes", width = 2),
                        valueBoxOutput("staticBox_trips", width = 2)
                    ),
                    DT::dataTableOutput("routesTable")
                    ),
           tabPanel("Frequency Analysis",
                    DT::dataTableOutput("frequencyTable")        
           )
)

##############################################################################
# Server Side of App
##############################################################################
server <- function(input, output, session, ...) {
    # Load Basic Map Background with default zoom at center of USA
    output$routemap <- renderLeaflet({
      leaflet()%>%
        addProviderTiles(providers$CartoDB.Positron)%>%
       # addResetMapButton()%>%
        setView(lat = 39.809253334942575,
                lng = -98.55663889876627,
                zoom = 5)
    })
    ##############################################################################
    # Reactive Variables
    ##############################################################################
    # Reactive variable for gtfs zip file. (Only allow known files listed below)
    gtfs_file <- reactive({
        req(input$selectFile$datapath)
        x <- tidytransit::read_gtfs(input$selectFile$datapath,
                       files = c('agency', 'stops', 'routes', 'trips',
                                 'stop_times', 'calendar', 'calendar_dates',
                                 'shapes'),
                       parse_dates = T)
        incProgress(0.5, message = "GTFS Loaded") # Progress bar message
        return(x)
    })
    
    keyfigures <- reactive({
        req(input$selectFile$datapath)
        data <- gtfs_file()
        modes <- length(unique(data$routes$route_type))
        routes <- length(unique(as.character(sapply(strsplit(as.character(gtfs_file()$routes$route_id),"-"),'[',1)))) # Remove dash
        stops <- length(unique(gtfs_file()$stops$stop_id))
        trips <- length(unique(gtfs_file()$trips$trip_id))
        keyfigures <- list("modes" = HTML(paste(format(modes, big.mark = ","))),
                           "routes" = HTML(paste(format(routes, big.mark = ","))),
                           "stops" = HTML(paste(format(stops, big.mark = ","))),
                           "trips" = HTML(paste(format(trips, big.mark = ","))))
        return(keyfigures)
    })
    
    routeGeoms <- reactive({
        geoms <- gtfsFunctions::gtfs2RouteLines(gtfs_file()$routes, gtfs_file()$trips, gtfs_file()$shapes)
        geoms$route_id_NoDash <- as.character(sapply(strsplit(as.character(geoms$route_id),"-"),'[',1)) # Remove dash
        return (geoms)
    })
    
    stops<- reactive({
        stopTable <- gtfsFunctions::routesAtStops(gtfs_file())
        stopTable$route_id<-unlist(lapply(strsplit(as.character(stopTable$route_id), "-"), '[[', 1)) #Remove route_id after dash 
        
        stopTableSimple <- gtfsFunctions::simplifyRoutesAtStops(stopTable)
        incProgress(0.7, message = "Stops Loaded") # Progress bar message
        return (stopTableSimple)
    })
    
    stopRouteTable_Long<- reactive({
    return(stops()[, strsplit(as.character(routesAtStop), ",", fixed=TRUE), 
                                   by = .(stop_id, routesAtStop)][,.(routesAtStop = stringr::str_squish(V1), stop_id)])
    })
    
##############################################################################
# GTFS Interactive Map Tab
##############################################################################
# Generate spatial df for route geoms

# Set that no routes were previously picked with this gtfs file (used when exlcuding layers in map)
previousRoutes <- c("") # Set basic empty list
    
observeEvent(input$selectFile, {
  withProgress(message = 'Loading...', value = 0, {
      # Dropdown menu to select routes to view
      output$routeOptions <- renderUI({  
      pickerInput(
          inputId = "routeOptionsInput",
          label = "Select routes to view",
          choices = mixedsort(unique(as.character(sapply(strsplit(as.character(gtfs_file()$routes$route_id),"-"),'[',1)))),
          options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 5",
              `count-selected-text` = "{0} routes selected"
          ),
          multiple = TRUE)
      })
      
      #Dropdown menu to select stop to zoom to
      output$zoom2Stop <- renderUI({  
        pickerInput(
          inputId = "zoom2StopInput",
          label = "Select stop_id to zoom to (Optional)",
          choices = mixedsort(unique(as.character(gtfs_file()$stops$stop_id))),
          options = pickerOptions(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 5",
            `count-selected-text` = "{0} stop selected",
             maxOptions = 1), # Only allow 1 selection but maintain formatting
          multiple = TRUE)
      })
  
      
      # Set that no routes were previously picked with this gtfs file (used when exlcuding layers in map)
      previousRoutes <<- c("") # Set basic empty list
        
      leafletProxy("routemap")%>%
        fitBounds(lng1 = min(as.numeric(stops()[,stop_lon])),
                  lat1 = min(as.numeric(stops()[,stop_lat])),
                  lng2 = max(as.numeric(stops()[,stop_lon])),
                  lat2 = max(as.numeric(stops()[,stop_lat])))%>%
        addMapPane("AllStops", zIndex = 420) %>% # Select map order to be below selected stops but above route liness
        addCircleMarkers(data = stops(),
                         group = ~paste0("AllStops"),
                         lat = ~stop_lat,
                         lng = ~stop_lon,
                         radius= 4,
                         color = 'black',
                         stroke = T,
                         weight = 0.2,
                         fillOpacity = 0.4,
                         popup = ~paste(sep = "<br/>",
                                        sprintf("<b>%s</b> (Stop_Id: %s)", stop_name, stop_id),
                                        #paste("<b>Stop_Id:</b>", sprintf("<b>%s</b>", stop_id)),
                                        paste("<b>Routes Serving Stop:</b>", routesAtStop, sep = '<br/>')
                         ),
                         options = pathOptions(pane = "AllStops"))%>%
        groupOptions("AllStops", zoomLevels = 14:25) # Only show stops when zoomed in enough
    })
})
# Add Leaflet Lines
observeEvent(input$routeOptionsInput, {
  # If some routes are selected then plot, if not, remove all shapes from map
  if (is.null(input$routeOptionsInput) == F){
    # Get Route Geometry Data
    spatialData <- routeGeoms()[routeGeoms()$route_id_NoDash %in% input$routeOptionsInput, ]
   
    # Get List of Stop Ids on Selected Route(s)
    selectedStopIds <- unique(stopRouteTable_Long()[with(stopRouteTable_Long(), routesAtStop %in% c(input$routeOptionsInput)), stop_id])
    
    # Filter Stops data to only include those selected stops
    stopsData <- stops()[stop_id %in% selectedStopIds,]

    # Get bbox to center map around    
    mapbbox <- st_bbox(spatialData)

      leafletProxy("routemap")%>%
        fitBounds(lng1 = as.numeric(mapbbox["xmin"]),
                  lat1 = as.numeric(mapbbox["ymin"]),
                  lng2 = as.numeric(mapbbox["xmax"]),
                  lat2 = as.numeric(mapbbox["ymax"]))%>%
        clearGroup(group = previousRoutes[!previousRoutes %in% input$routeOptionsInput])%>%
        clearGroup(group = "SelectedStops")%>% # Clear stops not selected
        addMapPane("SelectedRoutes", zIndex = 410) %>%   # Set Selected Routes to Be Bottom Layer
        addMapPane("SelectedStops", zIndex = 430) %>%   # Set Selected Stops to Be Top Layer
        addPolylines(data = spatialData,
                     group = ~as.character(route_id_NoDash),
                     color = ~ route_color,
                     label = ~paste("Route:", as.character(route_id_NoDash)),
                     options = pathOptions(pane = "SelectedRoutes"))%>%
        addCircleMarkers(data = stopsData,
                         group = ~paste0("SelectedStops"),
                         lat = ~stop_lat,
                         lng = ~stop_lon,
                         radius= 6,
                         weight = 4,
                         stroke = T,
                         color = 'black',
                         fill = T,
                         fillColor = 'white',
                         fillOpacity = 0.9,
                         popup = ~paste(sep = "<br/>",
                                        sprintf("<b>%s</b> (Stop_Id: %s)", stop_name, stop_id),
                                        #paste("<b>Stop_Id:</b>", sprintf("<b>%s</b>", stop_id)),
                                        paste("<b>Routes Serving Stop:</b>", routesAtStop, sep = '<br/>')
                         ),
                         options = pathOptions(pane = "SelectedStops"))%>%
        groupOptions(paste0("Stops_", as.character(stopsData$stop_id)), zoomLevels = 12:25)#%>% # Only show stops when zoomed in enough     
      previousRoutes <<- input$routeOptionsInput

  }else{
    leafletProxy("routemap")%>%
      clearShapes()%>%
      clearGroup(group = "SelectedStops") # Clear stops not selected
  }
    
}, ignoreNULL = FALSE)

# When Stop Is Selected from Dropdown Zoom To It
observeEvent(input$zoom2StopInput,{
  # If A Stop Is Selected Zoom To it. Otherwise keep at basic extent of all data
  req(input$selectFile) # Make Sure data exists
  if (is.null(input$zoom2StopInput) == FALSE){
    zoomedStop = stops()[as.character(stop_id) == as.character(input$zoom2StopInput), ]
    leafletProxy("routemap")%>%
      clearGroup("zoomedStop")%>%
      setView(lng = as.numeric(zoomedStop$stop_lon),
              lat = as.numeric(zoomedStop$stop_lat),
              zoom = 20)%>%
      addMarkers(data = zoomedStop,
                 lng = ~as.numeric(stop_lon),
                 lat = ~as.numeric(stop_lat),
                 popup = ~paste(sep = "<br/>",
                                sprintf("<b>%s</b> (Stop_Id: %s)", stop_name, stop_id),
                                #paste("<b>Stop_Id:</b>", sprintf("<b>%s</b>", stop_id)),
                                paste("<b>Routes Serving Stop:</b>", routesAtStop, sep = '<br/>')
                 ),
                 layerId = "zoomedStop")
    
  }else{
    leafletProxy("routemap")%>%
      removeMarker("zoomedStop")%>%
      fitBounds(lng1 = min(as.numeric(stops()[,stop_lon])),
                lat1 = min(as.numeric(stops()[,stop_lat])),
                lng2 = max(as.numeric(stops()[,stop_lon])),
                lat2 = max(as.numeric(stops()[,stop_lat])))
  }
}, ignoreNULL = FALSE)

##############################################################################
# GTFS Overview Tab
##############################################################################
output$staticBox_modes <- renderValueBox({
    valueBox(
        keyfigures()$modes,
        subtitle = "Unique Modes",
        icon     = icon("filter"),
        color    = "red",
        width    = NULL
    )
})

output$staticBox_stops <- renderValueBox({
    valueBox(
        keyfigures()$stops,
        subtitle = "Unique Stops",
        icon     = icon("map-marker"),
        color    = "red",
        width    = NULL
    )
})

output$staticBox_routes <- renderValueBox({
    valueBox(
        keyfigures()$routes,
        subtitle = "Unique Routes",
        icon     = icon("route"),
        color    = "red",
        width    = NULL
    )
})

output$staticBox_trips <- renderValueBox({
    valueBox(
        keyfigures()$trips,
        subtitle = "Planned Trips",
        icon     = icon("bus"),
        color    = "red",
        width    = NULL
    )
})

output$routesTable <- DT::renderDataTable(DT::datatable(gtfs_file()$routes, filter = 'top', options = list(pageLength = 25))
                                )
##############################################################################
# Frequency Analysis Overview Tab
##############################################################################
# Takes 13 seconds
output$frequencyTable <- DT::renderDataTable(DT::datatable(gtfsFunctions::calculateFrequenciesByRoute(gtfs_file()),
                                                           filter = 'top'))#,
                                                           # options = list(pageLength = 25,
                                                           #                order = list(list(2, 'asc')))))

} # End of Server


##############################################################################
shinyApp(ui, server)
##############################################################################

# Test script to get frequencies
# library(data.table)
# library(gtfsFunctions) #devtools::install_github("b-tomhave/gtfsFunctions")
# library(tidytransit)
# library(dplyr)
# 
# # Takes 5 seconds to load gtfs data
# data <- tidytransit::read_gtfs("/Users/bentomhave/Documents/Data_GTFS/MetroTransit.zip",
#                             files = c('agency', 'stops', 'routes', 'trips',
#                                       'stop_times', 'calendar', 'calendar_dates',
#                                       'shapes'),
#                             parse_dates = T)


# test <- gtfsFunctions::routesAtStops(data)
# test$route_id<-unlist(lapply(strsplit(as.character(test$route_id), "-"), '[[', 1)) #Remove route_id after dash
# #test <- test%>%distinct() # Remove excess rows
# simpleStops <- gtfsFunctions::simplifyRoutesAtStops(test)
# simpleStops2 <- as.data.table(simpleStops)
# class(simpleStops2)
# 
# aLine <- simpleStops[routesAtStop %like% "921"]
#  eitherOr <- simpleStops[Reduce(`|`, Map(`%like%`, list(routesAtStop), c("921", "84")))]

# # # Get Headways by Trip
#freqsByTrip <- gtfsFunctions::calculateFrequenciesByTrip(data) # Takes 15 seconds
# # 
# # # Get Headways by Route and Time Period
# freqs <- gtfsFunctions::calculateFrequenciesByRoute(data) # Takes 13 seconds
# 
# tripHeadways <- gtfsFunctions::calculateFrequenciesByTrip(data)
# tripHeadways$headway_mins <- tripHeadways$headway_secs/60
# 
# # Join trips.txt to this to get route_ids (only use route_ids to limit size)
# tripHeadways <- tripHeadways[dplyr::select(data$trips, route_id, trip_id) , on = "trip_id"] # Join with route_ids
# 
# # Reference table of seconds after midnight time od day (TOD) table for reference
# TOD <- data.table::data.table(BeginTime = c(0,14400,21600,32400,54000,66600,75600,86400),
#                               EndTime = c(14400,21600,32400,54000,66600,75600,86400,100800),
#                               Period = factor(c("Owl","Early","AM Peak","Midday",
#                                                 "PM Peak","Evening", "Night","Owl"),
#                                               levels = c("Early","AM Peak","Midday", "PM Peak",
#                                                          "Evening", "Night", "Owl"),
#                                               ordered = T))
# # Join time of day categorization to table
# tripHeadways <- tripHeadways[TOD, period := Period, on = .(start_time > BeginTime, start_time<= EndTime)]
# 
# # Calculate average headway for route-time of day group. Round down to nearest minute
# tripHeadways2 <- tripHeadways[, .(avgHeadway_Mins = floor(mean(headway_mins))), by = c("route_id", "period")]


# [ , .(mean_speed = mean(speed)), by = dive]
# tripHeadways2 <- tripHeadways2[, route_id := gtools::mixedsort(route_id)]
# 
# test <- dplyr::select(tripHeadways2, route_id, period, avgHeadway_Mins)%>%dplyr::distinct()
# 
# tehis <- tripHeadways2[route_id == "921-114" & period == "Early",]
# 
# mean(tehis$avgHeadway_Mins)

# # 
# # # Convert from seconds past midnight to HH:MM:SS
# # gtfsFunctions::transitTime2HHMMSS(66600)
# # 
# # # Convert HH:MM:SS to seconds past midnight
# # as.integer(gtfsFunctions::as.TransitTime("21:00:00"))
