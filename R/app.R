# Main application
# Load Libraries ---------------------------------------------------------------
library(shiny)
library(leaflet)
library(sf)
library(shinyjs) # To enable and disable buttons
library(shinyWidgets) # For Pickerinput
library(gtfsFunctions) #devtools::install_github("b-tomhave/gtfsFunctions", force = TRUE) #(DON'T UPDATE DATA.TABLE)
library(gtools) # For mixedsort
library(data.table) # For setcolorder
library(knitr)
library(shinydisconnect) # For disconnect error message
library(stringr) # For Str_trunc
library(waiter) # For spinners
library(shinyhelper)

# For Nice Table
library(htmltools)
library(reactable)

# Load Source Files ============================================================
# Potentially use local = T for each source call.
# Load Helper for button click spinners validation
source("helpers.R") # Load all the code needed to show feedback on a button click
#
# Tab 1 (Explorer)
source("mod_gtfsSidebar.R")      # Use to define GTFS inputs for mapping. Contains the majority of rval objects
source("mod_leaflet_map_base.R") # Creates basic Leaflet basemap for use in mod_gtfsMap.R
source("mod_gtfsMap.R")          # Creates GTFS Leaflet map based on the basemap defined in mod_leaflet_map_base.R

# Tab 2 (GTFS Overview)
source("mod_gtfsSummaryStats.R") # Creates row of formated value boxes describing high level gtfs stats
source("mod_tabularGTFS.R")      # Creates DataTable for selected GTFS txt file
source("mod_gtfsOverviewPage.R") # Creates GTFS Overview Page containing high level unique route, stop counts and datatable to view txt files

# Tab 3 (Route Density)
source("mod_routeDensityMap.R")

# Tab 4 (Route Frequency Analysis)
source("mod_frequencyPage.R")

# Allow input zip file to be up to 200mb in size & initialize reactiveValues
options(shiny.maxRequestSize = 200*1024^2)
rvals <- reactiveValues()

# Create App UI ================================================================
app_ui <- function() {
  navbarPage("Routes & Stops Viewer", id="nav",
             windowTitle = "Routes/Stops Viewer",
             #shinyjs::useShinyjs(), # Alows show/hide buttons
             # This error message pop-up is only shown in the browser (to avoid odd rendering put with navbarPage param not currently using)
             header = shinydisconnect::disconnectMessage(), 
              # Map Page
              tabPanel("Explorer",
                       # autoWaiter(),
                       # use_waiter(),
                       # Use CSS Formatting
                       div(class="outer",
                           
                           tags$head(
                             # Include custom CSS
                             includeCSS("../styles.css") # Go Back up a directory to css file
                             ),
                           mod_gtfsMap_ui("routesMap"),
                           gtfsInputPanel_ui("gtfsZipInput_ui_1")
                       )
              ),
             # Header Preview Page
             tabPanel("GTFS Overview",
                      # div(class="outer2",
                      #     tags$head(
                      #       # Include custom CSS
                      #       includeCSS("../styles.css")
                      #     ),
                        #tags$style(HTML("#gtfsFileLoadWarning{color: red; font-size: 10px;}")), # Set warning text to be red
                        gtfsOverviewPage_ui("gtfsOverviewPage_ui_1")
                      # )
             ),
             # Stop/Route Density Panel
             tabPanel("Route Density",
                      shinyjs::useShinyjs(), # Allow show/hide buttons,
                      div(class="outer2",
                          tags$head(
                            # Include custom CSS
                            includeCSS("../styles.css")
                          ),
                          hexMap_ui("hexMap_ui_1")
                      )
             ),
             # Frequency Analysis Preview
             tabPanel("Frequency Analysis",
                      shinyjs::useShinyjs(), # Allow show/hide buttons
                      frequencyOverview_ui("frequencyOverview_ui_1")
             )
                          
  )
}
# Create App server ============================================================
app_server <- function(input, output, session) {
  observe_helpers()  # Use for Shiny Helper
  mod_gtfsMap_server("routesMap", rvals)
  gtfsInputPanel_server("gtfsZipInput_ui_1", rvals)
  gtfsOverviewPage_server("gtfsOverviewPage_ui_1", rvals)
  hexMap_server("hexMap_ui_1", rvals, hexSizeInput = 0.02)
  frequencyOverview_server("frequencyOverview_ui_1", rvals)
}
# Run App ======================================================================
shinyApp(app_ui, app_server)



# Reactive Values Definition ===================================================
# The Following Reactive Variables Are Defined
# mod_gtfsSidebar.R
#     rvals$gtfsData:                GtfsData Object
#     rvals$stops:                   DataTable of GTFS Stops with a field for all routes at stop
#     rvals$routeGeoms:              sf of route geometries
#     rvals$routeIdAsFormattedRoute: named list/dictionary that converts route_id to a formatted route id (typically route_short_name)
#     rvals$stopRouteTable_Long:     DataTable of GTFS Stops with a row for every unique stop-route combination
#     rvals$routeChoices:            formatted named list based on routeIdAsFormattedRoute in sorted order
#     rvals$routeOptions:            selected route_id(s) from route input_dropdown
#     rvals$namedStopList:           named list of all stops in GTFS object with concatenated stop_id and stop_name as the associated name
#     rvals$zoom2StopId:             selected stop_id to zoom map to 

# mod_gtfsMap.R
#     rvals$routesMapbbox:           bounding box around selected routes

# mod_travelTime.R
#     rvals$formattedStopsTableForTT:  DataTable of GTFS Stops with a row for every unique stop-route combination with direction_id and stop_seq


