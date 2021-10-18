# Module Component 1: Create UI Select Input Drop Down of Possible GTFS Text Tiles To Explore
hexMap_ui <- function(id){
  ns <- NS(id)
  tagList(
      h2("Route Density Hex Map"),
      fluidRow(column(4,
                      "Hover over hexagon to show route alignments that serve that area."),
               column(1),
               column(6,
                      htmlOutput(ns('routesInHexText'))
                      )
               ),
      withBusyIndicatorUI(actionButton(ns("loadRouteDensityMap"), "Load Route Density Map")),
      leafletOutput(ns("hexMap"), width="100%", height="80%") #, width="100%", height="80%")
  )
}



hexMap_server <- function(id, rvals, hexSizeInput = 0.02){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Start With Load Data Button disabled until GTFS data is loaded in
    shinyjs::disable("loadRouteDensityMap")
    
    # Set BaeMap For Hex Map
    output$hexMap <- mod_leaflet_map_base_server("hexMap")
  
    
    # Reset Map When New Data Loaded
    observeEvent(rvals$gtfsData, {
      shinyjs::enable("loadRouteDensityMap")
      # Clear HexText
      output$routesInHexText <- renderUI({HTML(paste("Routes w/ Stop in Hex Area: "))})
      
      leafletProxy("hexMap")%>%
        clearControls()%>%
        clearShapes()%>%
        setView(lat = 39.809253334942575,
                lng = -98.55663889876627,
                zoom = 5)
    })
    
    # Get Hex Count Data
    countPerHex <- reactive({
          req(rvals$gtfsData)
          return(gtfsFunctions::uniqueRoutesInHexTessalation(rvals$gtfsData, hexSize = hexSizeInput))
      })
    

    # Load Map/Text when Button Pushed
    observeEvent(input$loadRouteDensityMap, {
      req(rvals$gtfsData)
      
      withBusyIndicatorServer(ns("loadRouteDensityMap"), { # Set Spinner While Loading
        # Set Palatte Colors  Hex Map
        colorVector <- c("grey75","#FFFFB2","#FD8D3C", "#BD0026")
        hexPallete <- leaflet::colorBin(
          palette = colorVector
          , domain = countPerHex()$uniqueRouteCount
          , bins =  c(1, 3, 5, 7, max(max(countPerHex()$uniqueRouteCount),10))
          , reverse = F
        )
        
  
        leafletProxy("hexMap")%>%
          clearControls()%>%
          clearShapes() %>%
          setView(lng = mean(sf::st_coordinates(countPerHex())[,1]),
                  lat = mean(sf::st_coordinates(countPerHex())[,2]),
                  zoom = 10)%>%
          addMapPane("hexPolygons", zIndex = 430) %>%   # Set Selected Routes to Be Bottom Layer
          addPolygons(data = countPerHex(),
                      layerId = ~hex_id,
                      group = "hexLayer",
                      fillColor = ~hexPallete(uniqueRouteCount),
                      color = "grey",
                      fillOpacity = 0.4,
                      weight = 1,
                      smoothFactor = 0.2,
                      options = pathOptions(pane = "hexPolygons"),
                      label = ~paste("Count of Unique Routes Serving Area:", uniqueRouteCount),
                      highlightOptions = highlightOptions(color = "Black", weight = 4,
                                                          bringToFront = T))%>%
          addLegend(
            position = "bottomright",
            na.label = "0 Routes",
            colors = colorVector,
            labels = c("1-2 Route", "3-4 Routes", "5-6 Routes","7+ Routes"), opacity = 1,
            title = "# of Unique Routes with Stop In Area"
          )
        
      })
    })
    
    
    
    ## track mouseover events
    observeEvent(input$hexMap_shape_mouseover, {
      # Get Routes in Hex
      hoverHexId <- input$hexMap_shape_mouseover$id
      routesInHexVector <- countPerHex()[countPerHex()$hex_id == hoverHexId, ]$routes

      hoverRouteIds <- gtools::mixedsort(as.character(unlist(routesInHexVector)))

      # Have to Reverse order of initial list to be able to call based on routeIDs to get name
      routeId2FormattedString <- setNames(names(rvals$routeChoices), rvals$routeChoices)

      hoverFormattedRouteIds <- gtools::mixedsort(as.character(routeId2FormattedString[as.character(hoverRouteIds)]))

      # Output Routes in Hex Text
      output$routesInHexText <- renderUI({HTML(paste("Routes w/ Stop in Hex Area: ",
                                                     knitr::combine_words(hoverFormattedRouteIds))
                                               )
      })
      hoverRoutes <- rvals$routeGeoms[rvals$routeGeoms$route_id %in% hoverRouteIds, ]
      
      if (nrow(hoverRoutes) !=0){
        leafletProxy("hexMap")%>%
          clearGroup("hoverRoutes")%>%
          addMapPane("hoverRoutes", zIndex = 410) %>%   # Set Selected Routes to Be Bottom Layer
          addPolylines(data = hoverRoutes,
                       group = "hoverRoutes",
                       color = ~ as.character(route_color),
                       weight = 8,
                       opacity = 0.9,
                       label = ~paste("Route:", as.character(rvals$routeIdAsFormattedRoute[route_id])),
                       options = pathOptions(pane = "hoverRoutes"))
      }else{
        leafletProxy("hexMap")%>%
          clearGroup("hoverRoutes")
      }
    })
      
  })
}







################################################################################


# Main application
# library(shiny)
# library(leaflet)
# library(sf)
# library(shinyjs) # To enable and disable buttons
# library(shinyWidgets) # For Pickerinput
# library(gtfsFunctions) #devtools::install_github("b-tomhave/gtfsFunctions", force = TRUE)
# library(gtools) # For mixedsort
# library(data.table) # For setcolorder
# library(knitr)
# 
# # Allow input zip file to be up to 200mb in size
# options(shiny.maxRequestSize = 200*1024^2)
# 
# #Source other Module Files
# source("mod_gtfsSidebar.R") # Potentially use local = T
# source("mod_leaflet_map_base.R")
# source("helpers.R") #Used 
# 
# # TO RUN SEPARATELY WITHOUT CSS REMOVE WIDTHS IN LEAFLET PLOT ABOVE
# #leafletOutput(ns("hexMap")) #, width="100%", height="100%"
# 
# rvals <- reactiveValues()
# 
# app_ui <- function() {
#   fluidPage(
#     shinyjs::useShinyjs(), # Allow show/hide buttons,
#     gtfsZipInput_ui("gtfsZipInput_ui_1"),# Don't need in final combined version
#     hexMap_ui("hexMap_ui_1")#,
#   )
# }
# 
# app_server <- function(input, output, session) {
#   gtfsZipInput_server("gtfsZipInput_ui_1", rvals) # Don't need in final combined version
#   hexMap_server("hexMap_ui_1", rvals, hexSizeInput = 0.02)
# }
# 
# shinyApp(app_ui, app_server)






