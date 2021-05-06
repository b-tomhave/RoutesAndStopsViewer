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
                    h2("Overview of GTFS Files"),
                    h5(id= "gtfsFileLoadWarning", "GTFS Zip File Must Be Loaded on 'Interactive Map' tab to load data below."),
                    tags$style(HTML("#gtfsFileLoadWarning{color: red;}")), # Set warning text to be red
                    fluidRow(
                        valueBoxOutput("staticBox_modes", width = 2),
                        valueBoxOutput("staticBox_stops", width = 2),
                        valueBoxOutput("staticBox_routes", width = 2),
                        valueBoxOutput("staticBox_trips", width = 2),
                        column(1),
                        column(3,
                               selectInput(
                                 "gtfsFileSelect",
                                 label = h4("Select a GTFS File to View Details"),
                                 choices = c("routes.txt" = "routes",
                                             "stops.txt"  = "stops",
                                             "agency.txt" = "agency"),
                                 selected = NULL,
                                 multiple = FALSE,
                                 selectize = TRUE
                               ))
                    ),
                    hr(),br(), #Insert blank space and horizontal line
                    h3(textOutput("selectedFile")),
                    "'formattedRouteName' from the routes.txt table shown below is the route selection field on the interactive map tab.",
                    br(),
                    DT::dataTableOutput("routesTable")
                    ),
           
           # Frequency Analysis Preview
           tabPanel("Frequency Analysis",
                    h2("Overview of Route Level Frequency/Headway"),
                    h5(id= "gtfsFileLoadWarning", "GTFS Zip File Must Be Loaded on 'Interactive Map' tab to load data below."),
                    tags$style(HTML("#gtfsFileLoadWarning{color: red;}")), # Set warning text to be red
                    fluidRow(
                        column(5,
                             DT::dataTableOutput("frequencyTable")  
                        )
                    )
           )
)

##############################################################################
# Server Side of App
##############################################################################
server <- function(input, output, session, ...) {
    # Load Basic Map Background with default zoom at center of USA
    output$routemap <- renderLeaflet({
      leaflet()%>%
        #Add Basemap Options
        addProviderTiles(providers$CartoDB.Positron,
                         group   = "Sketch (Default)")%>%
        addProviderTiles(providers$OpenStreetMap,
                         group   = "OpenStreetMap")%>%
        addProviderTiles(providers$Esri.WorldImagery,
                         group   = "Aerial")%>%
        setView(lat = 39.809253334942575,
                lng = -98.55663889876627,
                zoom = 5)%>%
        # Layers control
        addLayersControl(
          baseGroups = c("Sketch (Default)", "OpenStreetMap","Aerial"),
          options    = layersControlOptions(collapsed = FALSE),
          position = c("bottomright"))%>%
        addMeasure(position = c("bottomright")) #Add distance (and area measure)
      # addResetMapButton()%>%
    })
    ##############################################################################
    # Reactive Variables
    ##############################################################################
    # Reactive variable for gtfs zip file. (Only allow known files listed below)
    gtfs_file <- reactive({
        req(input$selectFile$datapath)
      
        # Load initially formatted gtfs object
        x <- gtfsFunctions::formatGTFSObject(input$selectFile$datapath)
        
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
        if (nrow(able(afterDash)) != 0){
          potentialIssueCorrectedIds <- as.character(ifelse(na.omit(as.numeric(afterDashTable[afterDash]), 0) >= 50,
                                                      as.character(sapply(strsplit(as.character(x$routes$route_id[potentialIssueRows]),"-"),'[',1)),
                                                      as.character(x$routes$route_id[potentialIssueRows])))
          # Create Named list with the potential issue dashed routes AND the fine routes
          old2NonDashedRouteId <- c(potentialIssueCorrectedIds,
                                    as.character(x$routes$route_id[!potentialIssueRows]))
          
          names(old2NonDashedRouteId) <- c(as.character(testData$routes$route_id[potentialIssueRows]),
                                           as.character(testData$routes$route_id[!potentialIssueRows]))
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
        
        incProgress(0.5, message = "GTFS Loaded") # Progress bar message
        return(x)
    })
    
    # Get Key Summary Stats (i.e. how many routes, stops, trips etc.)
    keyfigures <- reactive({
        req(input$selectFile$datapath)
        data <- gtfs_file()
        modes <- length(unique(data$routes$route_type))
        routes <- length(unique(as.character(gtfs_file()$routes$route_id)))#length(unique(as.character(sapply(strsplit(as.character(gtfs_file()$routes$route_id),"-"),'[',1)))) # Remove dash
        stops <- length(unique(gtfs_file()$stops$stop_id))
        trips <- length(unique(gtfs_file()$trips$trip_id))
        keyfigures <- list("modes" = HTML(paste(format(modes, big.mark = ","))),
                           "routes" = HTML(paste(format(routes, big.mark = ","))),
                           "stops" = HTML(paste(format(stops, big.mark = ","))),
                           "trips" = HTML(paste(format(trips, big.mark = ","))))
        return(keyfigures)
    })
    
    # Get Route Line Geometry from Loaded GTFS File
    routeGeoms <- reactive({
        geoms <- gtfsFunctions::gtfs2RouteLines(gtfs_file()$routes, gtfs_file()$trips, gtfs_file()$shapes)
        return (geoms)
    })
    
    # Get stops data and create routesAtStop field
    stops<- reactive({
        #stopTable <- gtfsFunctions::routeIDAtStops(gtfs_file())
        stopTableSimple <- gtfsFunctions::simpleRoutesAtStops(gtfs_file())
        incProgress(0.7, message = "Stops Loaded") # Progress bar message
        return (stopTableSimple)
    })
    
    # Reformat above reactive stops object to have a single record for each stop-route pair
    stopRouteTable_Long<- reactive({
    return(stops()[, strsplit(as.character(routesAtStop), ",", fixed=TRUE), 
                                   by = .(stop_id, routesAtStop)][,.(routesAtStop = stringr::str_squish(V1), stop_id)])
    })
    
    # Create formatted list so that user sees route_short_name but shiny uses route_id
    routeIdAsFormatedRoute<- reactive({
      return(gtfsFunctions::routeId2routeShortName(gtfs_file()$routes))
    })


##############################################################################
# GTFS Interactive Map Tab
##############################################################################
# Set that no routes were previously picked with this gtfs file (used when exlcuding layers in map)
previousRoutes <- c("") # Set basic empty list
    
observeEvent(input$selectFile, {
  withProgress(message = 'Loading...', value = 0, {

    # Get Named List with name as formatted route and value as route_id (the opposite of routeIdAsFormatedRoute())
    routeChoicesFormatted <- as.character(gtfs_file()$routes$route_id)
    names(routeChoicesFormatted) <- as.character(gtfs_file()$routes$formattedRouteName)#

    
    # Dropdown menu to select routes to view
      output$routeOptions <- renderUI({  
      pickerInput(
          inputId = "routeOptionsInput",
          label = "Select routes to view",
          choices = mixedsort(routeChoicesFormatted), #unique(as.character(gtfs_file()$routes$formattedRouteName)),#mixedsort(unique(as.character(gtfs_file()$routes$route_id))), 
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
                         color = 'blue',
                         stroke = T,
                         weight = 0.2,
                         fillOpacity = 0.4,
                         popup = ~paste(sep = "<br/>",
                                        sprintf("<b>%s</b> (Stop_Id: %s)", stop_name, stop_id),
                                        paste("<b>Routes Serving Stop:</b>", routesAtStop, sep = '<br/>')
                         ),
                         options = pathOptions(pane = "AllStops"))%>%
        groupOptions("AllStops", zoomLevels = 15:25) # Only show stops when zoomed in enough
    })
})
# Add Leaflet Lines
observeEvent(input$routeOptionsInput, {
  # If some routes are selected then plot, if not, remove all shapes from map
  if (is.null(input$routeOptionsInput) == F){
    # Get Route Geometry Data
    spatialData <- routeGeoms()[routeGeoms()$route_id %in% input$routeOptionsInput, ]

    # Get List of Stop Ids on Selected Route(s)
    selectedStopIds <- unique(stopRouteTable_Long()[with(stopRouteTable_Long(), routesAtStop %in% routeIdAsFormatedRoute()[c(input$routeOptionsInput)]), stop_id])
    
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
                     group = ~as.character(route_id),
                     color = ~ as.character(route_color),
                     weight = 8,
                     label = ~paste("Route:", as.character(routeIdAsFormatedRoute()[route_id])),
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
        icon     = icon("filter")
    )
})

output$staticBox_stops <- renderValueBox({
    valueBox(
        keyfigures()$stops,
        subtitle = "Active Stops",
        icon     = icon("map-marker")
    )
})

output$staticBox_routes <- renderValueBox({
    valueBox(
        keyfigures()$routes,
        subtitle = "Active Routes",
        icon     = icon("route")
    )
})

output$staticBox_trips <- renderValueBox({
    valueBox(
        keyfigures()$trips,
        subtitle = "Planned Trips",
        icon     = icon("bus")
    )
})

# Return Data.Table For Selected GTFS File
observeEvent(input$gtfsFileSelect, {
  
  output$selectedFile <- renderText({ 
    paste0("Showing: ", input$gtfsFileSelect, ".txt File")
  })
  
  output$routesTable <- DT::renderDataTable(DT::datatable(gtfs_file()[[as.character(input$gtfsFileSelect)]], filter = 'top',
                                                          options = list(pageLength = 25, ordering=F),
                                                          rownames= FALSE,
                                                          escape = FALSE,
                                                          selection = 'none')) # Set Strings as Factor so that filter is a dropdown not typing
  }, ignoreNULL = FALSE
)


##############################################################################
# Frequency Analysis Overview Tab
##############################################################################
# Takes 13 seconds
output$frequencyTable <- DT::renderDataTable(DT::datatable(na.omit(gtfsFunctions::calculateFrequenciesByRoute(gtfs_file())),
                                                           filter = 'top',
                                                           rownames= FALSE
                                                           ))

} # End of Server

##############################################################################
shinyApp(ui, server)
##############################################################################

# Test script to get frequencies
# library(data.table)
# library(gtfsFunctions) #devtools::install_github("b-tomhave/gtfsFunctions")
# library(tidytransit)
# library(dplyr)
# #
# # # Takes 5 seconds to load gtfs data
# data <- tidytransit::read_gtfs("/Users/bentomhave/Documents/Data_GTFS/MetroTransit_MSP.zip",
#                             files = c('agency', 'stops', 'routes', 'trips',
#                                       'stop_times', 'calendar', 'calendar_dates',
#                                       'shapes'),
#                             parse_dates = T)
# # 
# # #x$routes$formattedRouteName <- with(x$routes, ifelse(route_short_name == "", as.character(route_id), as.character(route_short_name)))
# # 
# #   
# data <- gtfsFunctions::formatGTFSObject("/Users/bentomhave/Documents/Data_GTFS/MetroTransit_MSP.zip")

# # # # Get Headways by Trip
# #freqsByTrip <- gtfsFunctions::calculateFrequenciesByTrip(data) # Takes 15 seconds
# # # 
# # # # Get Headways by Route and Time Period
# # freqs <- gtfsFunctions::calculateFrequenciesByRoute(data) # Takes 13 seconds
# # 
# # tripHeadways <- gtfsFunctions::calculateFrequenciesByTrip(data)
# # tripHeadways$headway_mins <- tripHeadways$headway_secs/60
# # 
# # # Join trips.txt to this to get route_ids (only use route_ids to limit size)
# # tripHeadways <- tripHeadways[dplyr::select(data$trips, route_id, trip_id) , on = "trip_id"] # Join with route_ids
# # 
# # # Reference table of seconds after midnight time od day (TOD) table for reference
# # TOD <- data.table::data.table(BeginTime = c(0,14400,21600,32400,54000,66600,75600,86400),
# #                               EndTime = c(14400,21600,32400,54000,66600,75600,86400,100800),
# #                               Period = factor(c("Owl","Early","AM Peak","Midday",
# #                                                 "PM Peak","Evening", "Night","Owl"),
# #                                               levels = c("Early","AM Peak","Midday", "PM Peak",
# #                                                          "Evening", "Night", "Owl"),
# #                                               ordered = T))
# # # Join time of day categorization to table
# # tripHeadways <- tripHeadways[TOD, period := Period, on = .(start_time > BeginTime, start_time<= EndTime)]
# # 
# # # Calculate average headway for route-time of day group. Round down to nearest minute
# # tripHeadways2 <- tripHeadways[, .(avgHeadway_Mins = floor(mean(headway_mins))), by = c("route_id", "period")]
# 
# 
# # [ , .(mean_speed = mean(speed)), by = dive]
# # tripHeadways2 <- tripHeadways2[, route_id := gtools::mixedsort(route_id)]
# # 
