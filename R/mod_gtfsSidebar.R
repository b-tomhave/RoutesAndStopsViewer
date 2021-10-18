# Module to Take Input GTFS Zip File and Return Reactive Data Path ----
# Module Component 1: Get FileInput for GTFS Zip File Path
gtfsZipInput_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fileInput(ns("gtfsZipFileInput"),
              h4("Select GTFS Zip File:"),
              multiple = FALSE,
              accept = ".zip")
  )
}


gtfsZipInput_server <- function(id, rvals){
  moduleServer(id, function(input, output, session) { 
    
    routes_Keep = c("formattedRouteName", "route_id", "route_short_name", "route_long_name",
                    "route_desc", "route_type", "route_url", "agency_id", "route_color") # Need color still for mapping
    
    agency_Keep = c("agency_id", "agency_name", "agency_url")
    
    stops_Keep = c("stop_id", "stop_name", "stop_desc",
                   "stop_lat", "stop_lon", "stop_url")
    
    # Return the Reactive GTFS Object
    # return(
    #   reactive({
        # Wait to load until datapath given
    observeEvent(input$gtfsZipFileInput,{

        req(input$gtfsZipFileInput$datapath)
        
        # Reset all reactive values when new gtfsfile selected
        rvals$gtfsData <- NULL                # GtfsData Object
        # rvals$stops <- NULL                   # DataTable of GTFS Stops with a field for all routes at stop
        # rvals$routeGeoms <- NULL              # sf of route geometries
        # rvals$routeIdAsFormattedRoute <- NULL # named list/dictionary that converts route_id to a formatted route id (typically route_short_name)
        # rvals$stopRouteTable_Long <- NULL     # DataTable of GTFS Stops with a row for every unique stop-route combination
        # rvals$routeChoices <- NULL            # formatted named list based on routeIdAsFormattedRoute in sorted order
        # rvals$routeOptions <- NULL            # selected route_id(s) from route input_dropdown
        # rvals$namedStopList <- NULL           # named list of all stops in GTFS object with concatenated stop_id and stop_name as the associated name
        # rvals$zoom2StopId <- NULL             # selected stop_id to zoom map to
        # rvals$formattedStopsTableForTT <- NULL
        # rvals$RouteFreqs <- NULL
        # rvals$RouteFreqsNoDir <- NULL

        
        # Load initially formatted gtfs object
        x <- gtfsFunctions::formatGTFSObject(input$gtfsZipFileInput$datapath)
        
        # Filter and Set Column Order for Each Table to only include necessary columns
        x$routes[ , setdiff(names(x$routes), routes_Keep) := NULL]
        
        # Remove columns in desired column that don't exist and column order based on those remaining
        routes_orderedPossibleColumns <- routes_Keep[routes_Keep %in% names(x$routes)]
        data.table::setcolorder(x$routes, routes_orderedPossibleColumns)
        
        # Filter Routes to only include those that are present in trips file
        x$routes <- x$routes[as.character(x$routes$route_id) %in% as.character(unique(x$trips$route_id)),]
        
        # Filter Agency & Set Column Order
        x$agency[ , setdiff(names(x$agency), agency_Keep) := NULL]
        # Remove columns in desired column that don't exist and column order based on those remaining
        agency_orderedPossibleColumns <- agency_Keep[agency_Keep %in% names(x$agency)]
        data.table::setcolorder(x$agency, agency_orderedPossibleColumns)
        
        
        # Filter Stops & Set Column Order
        x$stops[ , setdiff(names(x$stops), stops_Keep) := NULL]
        x$stops <- x$stops[gtools::mixedorder(as.character(stop_id))] # Set row order based on stop_id
        
        # Remove columns in desired column that don't exist and column order based on those remaining
        stops_orderedPossibleColumns <- stops_Keep[stops_Keep %in% names(x$stops)]
        data.table::setcolorder(x$stops, stops_orderedPossibleColumns)
        
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
        
        rvals$gtfsData <- x
    })
    
    
    # Set Reactive Data
    observeEvent(rvals$gtfsData, {
      req(rvals$gtfsData)
      rvals$stops      <- gtfsFunctions::simpleRoutesAtStops(rvals$gtfsData)
      rvals$routeGeoms <- gtfsFunctions::gtfs2RouteLines(rvals$gtfsData$routes, rvals$gtfsData$trips, rvals$gtfsData$shapes)
      rvals$routeIdAsFormattedRoute <- gtfsFunctions::routeId2routeShortName(rvals$gtfsData$routes)
      rvals$stopRouteTable_Long <- rvals$stops[, strsplit(as.character(routesAtStop), ",", fixed=TRUE), 
                                               by = .(stop_id, routesAtStop)][,.(routesAtStop = stringr::str_squish(V1), stop_id)]
      
      
      # Get Stop Options as Formatted List
      rvals$namedStopList <- as.character(rvals$gtfsData$stops$stop_id)
      names(rvals$namedStopList) <- paste0(as.character(rvals$gtfsData$stops$stop_id),
                                             ": ",
                                             as.character(rvals$gtfsData$stops$stop_name))
        
      
      # Get Route Options as Formatted List
      routeChoicesFormatted <- as.character(rvals$gtfsData$routes$route_id)
      names(routeChoicesFormatted) <- as.character(rvals$gtfsData$routes$formattedRouteName)#
      
      # Get RouteFormatted Names that Have a letter as the first element (these will be placed before numbered routes)
      letterFirstIndices <- grep("^[A-Za-z]", names(routeChoicesFormatted)) # Carrot indicates only looking at first/start character
      
      # If there is at least one route with a letter first then split order to put those first
      if (length(letterFirstIndices) != 0){
        # Routes that start with a letter
        routesWithLetterStart <- routeChoicesFormatted[letterFirstIndices]
        
        # Of the routes that start with a letter, which only have one character
        singleLetterIndices <- nchar(names(routesWithLetterStart)) == 1
        singleLetterNames <- names(routesWithLetterStart[singleLetterIndices]) 
        
        # Which routes do not have a letter as first character
        nonLetterFirstNames <- names(routeChoicesFormatted[stringr::str_sort(names(routeChoicesFormatted[-letterFirstIndices]), numeric = TRUE)])
        
        # Combine routes. First the single letter routes, then the routes that have a letter as first value but have length >1, then non-letter first routes
        newOrder <- routeChoicesFormatted[c(gtools::mixedsort(singleLetterNames), # Single letter routes
                                            gtools::mixedsort(names(routesWithLetterStart[singleLetterIndices == FALSE])), #  Routes with letter first but longer than one 1 character
                                            nonLetterFirstNames)] # Routes with non-letter as first character
      }else{
        newOrder = gtools::mixedsort(routeChoicesFormatted)
      }

      rvals$routeChoices <- newOrder
    })
    
  })
}


# Module Component 2: Get formatted Route_Id's from GTFS Data
routeOptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('routeOptions'))
  )
}


routeOptions_server <- function(id, rvals){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$routeOptions <- renderUI({
      req(rvals$gtfsData)
      div(
        pickerInput(
          ns("routeOptionsInput"),
          label = "Select routes to view",
          choices = rvals$routeChoices, 
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = T,
            liveSearchStyle = 'contains',
            size = 10,
            selectedTextFormat = "count > 5",
            countSelectedText = "{0} routes selected"
          ),
          multiple = TRUE)
      )
    })
    
    
    observeEvent(input$routeOptionsInput,{
      rvals$routeOptions <- input$routeOptionsInput
    }, ignoreNULL = FALSE) # Need to allow NULL Values so that I can deselect last route when removed

  })
}

    
# Module Component 3: Get stop_id's from GTFS Data
zoom2Stop_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('zoom2Stop'))
  )
}


zoom2Stop_server <- function(id, rvals){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$zoom2Stop <- renderUI({  
      req(rvals$gtfsData)
      div(
        id = "stopsNoSelectAll", # This Matches with CSS File to remove select all option
        class = 'zoom2StopClass',
        pickerInput(
          ns("zoom2StopInput"),
          label = "Select stop_id to zoom to (Optional)",
          choices = rvals$namedStopList,
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = T,
            liveSearchStyle = 'contains',
            size = 10,
            selectedTextFormat = "count > 5",
            countSelectedText = "{0} stops selected",
            maxOptions = 1,
            virtualScroll = 500), # If over 500 stops only render choices shown in box at any one time
          multiple = TRUE)
      )
    })
    

    
    observeEvent(input$zoom2StopInput,{
      rvals$zoom2StopId <- input$zoom2StopInput
    }, ignoreNULL = FALSE) # Need to allow NULL Values so that I can deselect last route when removed
    
    
  })
}

    
    
# Module Component 4: Combine Modules 1-3 into Single Input Panel
gtfsInputPanel_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    # Shiny versions prior to 0.11 should use class = "modal" instead.
    absolutePanel(id = "controls",  # Set ID so that CSS can format 
                  class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                  width = 330, height = "auto",
                  HTML('<button type="button" class="btn btn-primary" data-toggle="collapse" data-target="#gtfsMapControls">Collapse/Expand Controls</button>'),
                  br(),
                  tags$div(id = 'gtfsMapControls',  class="collapse in",
                           gtfsZipInput_ui(ns("gtfsZip_1")),
                           helpText("Initial load time of 10-15 seconds. Only .txt files allowed in Zip file, no sub-folders"),
                           routeOptions_ui(ns("routeOptions_1")),
                           zoom2Stop_ui(ns('zoom2StopInput'))
                  )
    )
  )
}


gtfsInputPanel_server <- function(id, rvals){
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    # Nested UI From Select File
    gtfsZipInput_server("gtfsZip_1", rvals)
    routeOptions_server("routeOptions_1", rvals)
    zoom2Stop_server("zoom2StopInput", rvals)
  })
  
}   


################################################################################

    
# # Main application
# library(shiny)
# library(leaflet)
# library(shinyWidgets) # For Pickerinput
# library(shinyhelper) # For helper()
# library(gtfsFunctions) #devtools::install_github("b-tomhave/gtfsFunctions", force = TRUE)
# library(gtools) # For mixedsort
# library(data.table) # For setcolorder
# 
# rvals <- reactiveValues()
# 
# # Allow input zip file to be up to 200mb in size
# options(shiny.maxRequestSize = 200*1024^2)
# 
# app_ui <- function() {
#   fluidPage(
#     shinyjs::useShinyjs(), # Alows show/hide buttons
#     gtfsInputPanel_ui("gtfsInputPanel_ui_1")#,
#   )
# }
# 
# app_server <- function(input, output, session) {
#   gtfsInputPanel_server("gtfsInputPanel_ui_1", rvals)
# }
# 
# shinyApp(app_ui, app_server)
