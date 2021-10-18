# Module Component 1: Create Route Input
singleSegmentTravelTime_ui <- function(id){
  ns <- NS(id)
  tagList(
    withBusyIndicatorUI(actionButton(ns("loadTTData"), "Load Data")),
    fluidRow(
      column(2,
             uiOutput(ns('TT_FirstRoute'))
             ),
      column(2,
             uiOutput(ns('TT_FirstDirection'))
             ),
      column(2,
             uiOutput(ns('TT_FirstBoardStop'))
             ),
      column(2,
             uiOutput(ns('TT_FirstAlightStop'))
             ),
      column(2, 
             actionButton(ns('loadTT1'),class = "btn-primary", "Calculate Travel Time")
             ),
      column(2,
             textOutput(ns('tt1Output')),
             )
    )
  )
}



singleSegmentTravelTime_server <- function(id, rvals){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Start With Load Data Button disabled until GTFS data is loaded in
    shinyjs::disable("loadTTData")
    shinyjs::disable("loadTT1")
    
    # When GTFS Data Is Loaded Enable Ability to Load TT Data
    observeEvent(rvals$gtfsData, {
      shinyjs::enable("loadTTData")
      
      # Hide Time Boxes
      shinyjs::hide("TT_FirstRoute")
      shinyjs::hide("TT_FirstDirection")
      shinyjs::hide("TT_FirstBoardStop")
      shinyjs::hide("TT_FirstAlightStop")
    })
    
    # When Load Data Table Button Is Pressed Get Formatted Stop Data
    observeEvent(input$loadTTData,{
      req(rvals$gtfsData)
      shinyjs::disable("loadTTData")
      
      # Show Boxes
      shinyjs::show("TT_FirstRoute")
      
      withBusyIndicatorServer(ns("loadTTData"), { # Set Spinner While Loading
        rvals$formattedStopsTableForTT <- gtfsFunctions::routeIDAtStopsWithDirAndSeq(rvals$gtfsData)

        output$TT_FirstRoute <- renderUI({
          req(isolate(rvals$routeChoices))
          
          # This is a self-reactive code because it depends on input$TT_FirstRouteInput which automatically triggers reevaulation of all reactive expressions that depend on it
          selectInput(ns('TT_FirstRouteInput'),
                      'Select Route',
                      choices = isolate(rvals$routeChoices),
                      multiple = F,
                      selectize= TRUE)
        })
        
      })
    })
    
    
    
    
    # # When Route Is Selected Adjust Possible Directions
    observeEvent(input$TT_FirstRouteInput, {
      #req(input$TT_FirstRouteInput)
      
      shinyjs::show("TT_FirstDirection")

      directionSubset <- unique(rvals$formattedStopsTableForTT[with(rvals$formattedStopsTableForTT, as.character(route_id) == input$TT_FirstRouteInput), direction_id])

      output$TT_FirstDirection <- renderUI({
        req(isolate(rvals$formattedStopsTableForTT))
        selectInput(ns('TT_FirstDirectionInput'),
                       'Direction',
                       choices =  directionSubset,
                       selected = directionSubset[1],
                       multiple = F,
                       selectize = T)
      })
    })

    # When Direction is changed alter first boarding stop
    observeEvent(c(input$TT_FirstRouteInput, input$TT_FirstDirectionInput), {
      req(input$TT_FirstRouteInput)
      req(input$TT_FirstDirectionInput)
      req(isolate(rvals$formattedStopsTableForTT))
      
      shinyjs::show("TT_FirstBoardStop")

      rvals$routeSubsetStopsTable <- unique(rvals$formattedStopsTableForTT[with(rvals$formattedStopsTableForTT, as.character(route_id) == input$TT_FirstRouteInput), ]) %>%
                                  subset(as.character(direction_id) == as.character(input$TT_FirstDirectionInput))
      # Order By Stop Sequence Ascending
      rvals$routeSubsetStopsTable <- rvals$routeSubsetStopsTable[with(rvals$routeSubsetStopsTable, order(stop_sequence)), ]

      if(nrow(rvals$routeSubsetStopsTable) != 0){
        subsetNamedStopList <- as.character(rvals$routeSubsetStopsTable$stop_id)
        names(subsetNamedStopList) <- paste0(as.character(rvals$routeSubsetStopsTable$stop_id),
                                             ": ",
                                             as.character(rvals$routeSubsetStopsTable$stop_name))
  
        
        output$TT_FirstBoardStop <- renderUI({
          req(rvals$gtfsData)
          req(input$TT_FirstDirectionInput)
          selectInput(ns('TT_FirstBoardStopInput'),
                         'Select Boarding Stop',
                         choices =  subsetNamedStopList[1:(length(subsetNamedStopList)-1)], # Don't include last stop as it can only be an alighting stop
                         selected = subsetNamedStopList[1],
                         multiple = F,
                         selectize = T)
        })
      }
      
      
    })
    
    # Update alighting stop based off boarding stop (must be after)
    observeEvent(input$TT_FirstBoardStopInput, {
      req(input$TT_FirstBoardStopInput)
      req(rvals$formattedStopsTableForTT)
      
      shinyjs::show("TT_FirstAlightStop")

      boardingStopRow <- rvals$routeSubsetStopsTable %>% subset(as.character(stop_id) == as.character(input$TT_FirstBoardStopInput))

      # Only Allow selection of alighting stop if boarding stop exists
      if (nrow(boardingStopRow) !=0){
        boardingStopSequence <- as.numeric(boardingStopRow[1, "stop_sequence"]) # Take first row to get minimum stop sequence value in case multiple trips serve stop at different points

        # Order By Stop Sequence Ascending
        potentialAlightingStopTable <- rvals$routeSubsetStopsTable %>% subset(as.numeric(stop_sequence) > as.numeric(boardingStopSequence))

        subsetAlightingNamedStopList <- as.character(potentialAlightingStopTable$stop_id)
        names(subsetAlightingNamedStopList) <- paste0(as.character(potentialAlightingStopTable$stop_id),
                                             ": ",
                                             as.character(potentialAlightingStopTable$stop_name))

        
        output$TT_FirstAlightStop <- renderUI({
          req(input$TT_FirstBoardStopInput)
          req(rvals$routeSubsetStopsTable)
          selectInput(ns('TT_FirstAlightStopInput'),
                      'Select Alighting Stop',
                      choices =  subsetAlightingNamedStopList,
                      selected = subsetAlightingNamedStopList[1],
                      multiple = F,
                      selectize = T)
        })
        
        
      }
    })
 
    observeEvent(c(input$TT_FirstRouteInput, input$TT_FirstDirectionInput, input$TT_FirstBoardStopInput, input$TT_FirstAlightStopInput), {
      req(input$TT_FirstRouteInput)
      req(input$TT_FirstDirectionInput)
      req(input$TT_FirstBoardStopInput)
      req(input$TT_FirstAlightStopInput)
      req(rvals$gtfsData)
      shinyjs::enable("loadTT1")

    })
    
    observeEvent(c(input$loadTT1),{
      req(input$TT_FirstRouteInput)
      req(input$TT_FirstDirectionInput)
      req(input$TT_FirstBoardStopInput)
      req(input$TT_FirstAlightStopInput)

      tt1Minutes <- gtfsFunctions::calculateMedianTravelTime(rvals$gtfsData,
                                                             as.character(input$TT_FirstBoardStopInput),
                                                             as.character(input$TT_FirstAlightStopInput),
                                                             as.character(input$TT_FirstRouteInput))
      output$tt1Output <- renderText({tt1Minutes})
      print(tt1Minutes)
    })
    
    
    
  })
  

}







################################################################################


#Main application
# library(shiny)
# library(leaflet)
# library(sf)
# library(shinyjs) # To enable and disable buttons
# library(shinyWidgets) # For Pickerinput
# library(gtfsFunctions) #devtools::install_github("b-tomhave/gtfsFunctions", force = TRUE)
# 
# 
# # Allow input zip file to be up to 200mb in size
# options(shiny.maxRequestSize = 200*1024^2)
# 
# #Source other Module Files
# source("mod_gtfsSidebar.R") # Potentially use local = T
# source("helpers.R") #Used for busy indicator
# 
# rvals <- reactiveValues()
# 
# app_ui <- function() {
#   fluidPage(
#     shinyjs::useShinyjs(), # Allow show/hide buttons,
#     gtfsZipInput_ui("gtfsZipInput_ui_1"),# Don't need in final combined version
#     singleSegmentTravelTime_ui("singleSegmentTravelTime_ui_1")#,
#   )
# }
# 
# app_server <- function(input, output, session) {
#   gtfsZipInput_server("gtfsZipInput_ui_1", rvals) # Don't need in final combined version
#   singleSegmentTravelTime_server("singleSegmentTravelTime_ui_1", rvals)
# }
# 
# shinyApp(app_ui, app_server)

