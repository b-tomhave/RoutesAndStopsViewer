# Module Component: Get Summary Statistics from reactive gtfs file
gtfsSummaryBoxes_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::valueBoxOutput(ns("staticBox_modes"),  width = 3),
      shinydashboard::valueBoxOutput(ns("staticBox_stops"),  width = 3),
      shinydashboard::valueBoxOutput(ns("staticBox_routes"), width = 3),
      shinydashboard::valueBoxOutput(ns("staticBox_trips"),  width = 3)
    )
  )
}


gtfsSummaryBoxes_server <- function(id, rvals){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Summarize Key Statistics From Reactive GTFS into a reactive object
    keyFigures <- reactive({
      req(rvals$gtfsData)
      modes <- length(unique(rvals$gtfsData$routes$route_type))
      routes <- length(unique(as.character(rvals$gtfsData$routes$route_id)))#length(unique(as.character(sapply(strsplit(as.character(gtfs_file()$routes$route_id),"-"),'[',1)))) # Remove dash
      stops <- length(unique(rvals$gtfsData$stops$stop_id))
      trips <- length(unique(rvals$gtfsData$trips$trip_id))
      keyFigures <- list("modes" = HTML(paste(format(modes, big.mark = ","))),
                         "routes" = HTML(paste(format(routes, big.mark = ","))),
                         "stops" = HTML(paste(format(stops, big.mark = ","))),
                         "trips" = HTML(paste(format(trips, big.mark = ","))))
      return(keyFigures)
    })
    
    
    # Based on keyFigures Reactive List output statistics to individual valueboxes
    output$staticBox_modes <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        keyFigures()$modes,
        subtitle = "Unique Modes",
        icon     = icon("filter")
      )
    })
    
    output$staticBox_stops <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        keyFigures()$stops,
        subtitle = "Active Stops",
        icon     = icon("map-marker")
      )
    })
    
    output$staticBox_routes <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        keyFigures()$routes,
        subtitle = "Active Routes",
        icon     = icon("route")
      )
    })
    
    output$staticBox_trips <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        keyFigures()$trips,
        subtitle = "Planned Trips",
        icon     = icon("bus")
      )
    })
    
  })
}
    
    

################################################################################


# # Main application
# library(shiny)
# library(shinydashboard) # For valueBoxOutput
# library(data.table) # For setcolorder
# 
# rvals <- reactiveValues()
# 
# # Source other Module Files
# #source("mod_gtfsSidebar.R") # Potentially use local = T
# 
# # Allow input zip file to be up to 200mb in size
# options(shiny.maxRequestSize = 200*1024^2)
# 
# app_ui <- function() {
#   fluidPage(
#     gtfsZipInput_ui("gtfsZip_1"), # This would not be included in final version but is included here to load data
#     gtfsSummaryBoxes_ui("gtfsSummaryBoxes_ui_1")
#   )
# }
# 
# app_server <- function(input, output, session) {
#   gtfsZipInput_server("gtfsZip_1", rvals) # This would not be included in final version but is included here to load data
#   gtfsSummaryBoxes_server("gtfsSummaryBoxes_ui_1", rvals)
# }
# 
# shinyApp(app_ui, app_server)

