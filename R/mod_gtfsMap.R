# Module to Plot GTFS Routes and Stops ----
mod_gtfsMap_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Add a slider to select a number
    # If not using custom CSS, set height of leafletOutput to a number instead of percent
    leafletOutput(ns("routemap"), width="100%", height="100%") #, width="100%", height="100%"
  )

}

mod_gtfsMap_server <- function(id, rvals){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    previousRoutes <- c("") # Set basic empty list
    
  #w <- Waiter$new(ns("routemap")) # , html="Please wait")#, hide_on_render=T)

    # output$map ----
    output$routemap <- mod_leaflet_map_base_server("baseMap")
    
    # Once stop data has loaded plot that data
    observeEvent(rvals$stops,{
      req(rvals$stops) # Ensure stop data has been loaded (slowish) before mapping

      
    # adding proxys --------
      leafletProxy("routemap") %>%
          clearGroup("AllStops")%>%
          clearGroup("Stops on Selected Route(s)")%>%
          clearGroup("SelectedRoutes") %>%
      fitBounds(lng1 = min(as.numeric(rvals$stops[,stop_lon])),
                lat1 = min(as.numeric(rvals$stops[,stop_lat])),
                lng2 = max(as.numeric(rvals$stops[,stop_lon])),
                lat2 = max(as.numeric(rvals$stops[,stop_lat]))) %>%
        addMapPane("AllStops", zIndex = 420) %>% # Select map order to be below selected stops but above route lines
        addCircleMarkers(data = rvals$stops,
                         group = "AllStops",
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
        hideGroup("AllStops")%>% # Don't show All Stops unless turned on
        #groupOptions("AllStops", zoomLevels = 13:25)%>% # Only show stops when zoomed in enough
          addLayersControl(
            baseGroups = c("Sketch (Default)", "Black & White", "OpenStreetMap","Aerial"),
            overlayGroups = c("AllStops", "Stops on Selected Route(s)"),
            options    = layersControlOptions(collapsed = T),
            position = c("topleft"))
    })
    
    # When Selected Route is Changed Update Selected Route Polyline and Points
    # Add Leaflet Lines
    observeEvent(rvals$routeOptions, {
      req(rvals$stops)
      req(rvals$routeIdAsFormattedRoute)

      # If some routes are selected then plot, if not, remove all shapes from map
      if (is.null(rvals$routeOptions) == F){
        # Get Route Geometry Data
        spatialData <- rvals$routeGeoms[rvals$routeGeoms$route_id %in% rvals$routeOptions, ]
        
        # Get List of Stop Ids on Selected Route(s)
        selectedStopIds <- unique(rvals$stopRouteTable_Long[with(rvals$stopRouteTable_Long, routesAtStop %in% rvals$routeIdAsFormattedRoute[c(rvals$routeOptions)]), stop_id])
        
        # Filter Stops data to only include those selected stops
        stopsData <- rvals$stops[stop_id %in% selectedStopIds,]
        
        # Get bbox to center map around    
        rvals$routesMapbbox <- sf::st_bbox(spatialData) # Might need a way to reset this?
        
        leafletProxy("routemap")%>%
          fitBounds(lng1 = as.numeric(rvals$routesMapbbox["xmin"]),
                    lat1 = as.numeric(rvals$routesMapbbox["ymin"]),
                    lng2 = as.numeric(rvals$routesMapbbox["xmax"]),
                    lat2 = as.numeric(rvals$routesMapbbox["ymax"]))%>%
          clearGroup(group = previousRoutes[!previousRoutes %in% rvals$routeOptions])%>%
          clearGroup(group = "Stops on Selected Route(s)")%>% # Clear stops not selected
          addMapPane("SelectedRoutes", zIndex = 410) %>%   # Set Selected Routes to Be Bottom Layer
          addMapPane("Stops on Selected Route(s)", zIndex = 430) %>%   # Set Selected Stops to Be Top Layer
          addPolylines(data = spatialData,
                       group = ~as.character(route_id),
                       color = ~ as.character(route_color),
                       weight = 8,
                       label = ~paste("Route:", as.character(rvals$routeIdAsFormattedRoute[route_id])),
                       options = pathOptions(pane = "SelectedRoutes"))%>%
          addCircleMarkers(data = stopsData,
                           group = "Stops on Selected Route(s)",
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
                           options = pathOptions(pane = "Stops on Selected Route(s)"))

        previousRoutes <<- rvals$routeOptions #Can't recall why this is the double <<-
        
      }else{
        leafletProxy("routemap")%>%
          fitBounds(lng1 = min(as.numeric(rvals$stops[,stop_lon])),
                    lat1 = min(as.numeric(rvals$stops[,stop_lat])),
                    lng2 = max(as.numeric(rvals$stops[,stop_lon])),
                    lat2 = max(as.numeric(rvals$stops[,stop_lat])))%>%
          clearShapes()%>%
          clearGroup(group = "Stops on Selected Route(s)") # Clear stops not selected
      }
      
    }, ignoreNULL = FALSE)
    
    
    # When Stop Is Selected from Dropdown Zoom To It
    observeEvent(rvals$zoom2StopId,{
      # If A Stop Is Selected Zoom To it. Otherwise keep at basic extent of all data
      #req(rvals$zoom2StopId) # Make Sure data exists
      
      if (is.null(rvals$zoom2StopId) == FALSE){
        zoomedStop = rvals$stops[as.character(stop_id) == as.character(rvals$zoom2StopId), ]
        leafletProxy("routemap")%>%
          clearGroup("zoomedStop")%>%
          setView(lng = as.numeric(zoomedStop$stop_lon),
                  lat = as.numeric(zoomedStop$stop_lat),
                  zoom = 16)%>%
          addMarkers(data = zoomedStop,
                     lng = ~as.numeric(stop_lon),
                     lat = ~as.numeric(stop_lat),
                     popup = ~paste(sep = "<br/>",
                                    sprintf("<b>%s</b> (Stop_Id: %s)", stop_name, stop_id),
                                    paste("<b>Routes Serving Stop:</b>", routesAtStop, sep = '<br/>')
                     ),
                     layerId = "zoomedStop")
        
      }else{
        req(rvals$routesMapbbox)
        leafletProxy("routemap")%>%
          removeMarker("zoomedStop")%>%
          fitBounds(lng1 = as.numeric(rvals$routesMapbbox["xmin"]),
                    lat1 = as.numeric(rvals$routesMapbbox["ymin"]),
                    lng2 = as.numeric(rvals$routesMapbbox["xmax"]),
                    lat2 = as.numeric(rvals$routesMapbbox["ymax"]))
      }
    }, ignoreNULL = FALSE)
    
  })
}


################################################################################


# Main application
# library(shiny)
# library(leaflet)
# library(shinyWidgets) # For Pickerinput
# library(gtfsFunctions) #devtools::install_github("b-tomhave/gtfsFunctions", force = TRUE)
# library(gtools) # For mixedsort
# library(data.table) # For setcolorder

# Allow input zip file to be up to 200mb in size
options(shiny.maxRequestSize = 200*1024^2)


# TO RUN SEPARATELY WITHOUT CSS REMOVE WIDTHS IN LEAFLET PLOT ABOVE
#leafletOutput(ns("routemap")) #, width="100%", height="100%"

# Source other Module Files
# source("mod_gtfsSidebar.R") # Potentially use local = T
# source("mod_leaflet_map_base.R")

rvals <- reactiveValues()

app_ui <- function() {
  fluidPage(
    mod_gtfsMap_ui("routesMap"),
    gtfsInputPanel_ui("gtfsZipInput_ui_1")#,
  )
}

app_server <- function(input, output, session) {
  mod_gtfsMap_server("routesMap", rvals)
  gtfsInputPanel_server("gtfsZipInput_ui_1", rvals)
}

shinyApp(app_ui, app_server)
