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
library(leaflet.extras)
library(gtools) # For mixed sort

#setwd("~/Documents/R Projects/RoutesAndStopsViewer")

# Allow input zip file to be up to 100mb in size
options(shiny.maxRequestSize = 100*1024^2)
##############################################################################
# Functions
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
                                      fileInput("selectFile", h4("Select GTFS Zip File:"),
                                                multiple = FALSE,
                                                accept = ".zip"),
                                      uiOutput('routeOptions')
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
        addProviderTiles(providers$CartoDB.Positron)%>%
        addResetMapButton()%>%
        setView(lat = 39.809253334942575,
                lng = -98.55663889876627,
                zoom = 5)
    })
    ##############################################################################
    # Reactive Variables
    ##############################################################################
    # TODO make the input file request more open
    # Reactive variable for gtfs zip file. (Only allow known files listed below)
    gtfs_file <- reactive({
        x <- tidytransit::read_gtfs(input$selectFile$datapath,
                       files = c('agency', 'stops', 'routes', 'trips',
                                 'stop_times', 'calendar', 'calendar_dates',
                                 'shapes'),
                       parse_dates = T)
        return(x)
    })
    
    # Generate spatial df for route geoms
    routeGeoms <- reactive({
      geoms <- gtfsFunctions::gtfs2RouteLines(gtfs_file()$routes, gtfs_file()$trips, gtfs_file()$shapes)
      geoms$route_id_NoDash <- as.character(sapply(strsplit(as.character(geoms$route_id),"-"),'[',1)) # Remove dash
      return (geoms)
    })
    
    # Set that no routes were previously picked with this gtfs file (used when exlcuding layers in map)
    previousRoutes <- c("") # Set basic empty list
    
observeEvent(input$selectFile, {
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
    
    # Set that no routes were previously picked with this gtfs file (used when exlcuding layers in map)
    previousRoutes <<- c("") # Set basic empty list
})

#Add Leaflet Lines
observeEvent(input$routeOptionsInput, {
  # If some routes are selected then plot, if not, remove all shapes from map
  if (is.null(input$routeOptionsInput) == F){
    
    spatialData <- routeGeoms()[routeGeoms()$route_id_NoDash %in% input$routeOptionsInput, ]

    # Get bbox to center map around    
    mapbbox <- st_bbox(spatialData)
    
      leafletProxy("routemap")%>%
        fitBounds(lng1 = as.numeric(mapbbox["xmin"]),
                  lat1 = as.numeric(mapbbox["ymin"]),
                  lng2 = as.numeric(mapbbox["xmax"]),
                  lat2 = as.numeric(mapbbox["ymax"]))%>%
        clearGroup(group = previousRoutes[!previousRoutes %in% input$routeOptionsInput])%>%
        addPolylines(data = spatialData,
                     group = ~as.character(route_id_NoDash),
                     color = ~ route_color)
      
      previousRoutes <<- input$routeOptionsInput
  }else{
    leafletProxy("routemap")%>%
      clearShapes()
  }
    


}, ignoreNULL = FALSE)
  
  
    
}


##############################################################################
shinyApp(ui, server)
##############################################################################
