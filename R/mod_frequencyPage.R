# Module Component 1: Create UI Load Frequency Action Button, Route Dropdown and Map
frequencyOverview_ui <- function(id){
  ns <- NS(id)
  #shinyjs::useShinyjs() # Allow show/hide buttons
  tagList(
    h3("Bus Frequency Overview -- Times are Approximate"),
    fluidRow(column(3,
                    withBusyIndicatorUI(
                      helper(
                        actionButton(ns("loadFreqData"), "Load Frequency Data"),
                        icon = 'question-circle',
                        type = 'inline',
                        title = 'Individudal Route Frequency Method',
                        content = "GTFS Zip File Must Be Loaded on 'Explorer' tab to load data below. Route Frequencies are obtained by averaging the stop level frequency (Unique Trip Count/Time Period Duration) for all stops on the selected route.")
                    )
                    )
             ),
    br(),
    tabsetPanel(
      tabPanel("Frequency Comparison",
               fluidRow(
                 column(8,
                        uiOutput(ns("frequencyAnalysisPeriod")),
                        plotly::plotlyOutput(ns("comparisonFreqBarPlot")),
                        br()
                 ),
                 column(4,
                        h4('High Frequency: 15 Minutes or Better'),
                        textOutput(ns('highFreqRoutes')),
                        h4('15-20 Minutes'),
                        textOutput(ns('mediumHighFreqRoutes')),
                        h4('20-30 Minutes'),
                        textOutput(ns('mediumLowFreqRoutes')),
                        h4('30-60 Minutes'),
                        textOutput(ns('lowFreqRoutes')),
                        h4('60+ Minutes'),
                        textOutput(ns('veryLowFreqRoutes'))
                 )
               )#,
               # fluidRow(
               #   column(8,
               #          #reactableOutput(ns("testTable"))
               #          plotly::plotlyOutput(ns("comparisonFreqBarPlot"))#,
               #         # "Plot Placeholder"
               #          ),
               #   column(4,
               #          h3('High Frequency: 15 Minutes or Better'),
               #          textOutput(ns('highFreqRoutes')),
               #          h3('15-20 Minutes'),
               #          textOutput(ns('mediumHighFreqRoutes')),
               #          h3('20-30 Minutes'),
               #          textOutput(ns('mediumLowFreqRoutes')),
               #          h3('30-60 Minutes'),
               #          textOutput(ns('lowFreqRoutes')),
               #          h3('60+ Minutes'),
               #          textOutput(ns('veryLowFreqRoutes'))
               #          )
               #   )
               ),
      tabPanel("Single Route Frequency",
               fluidRow(
                 column(5,
                        uiOutput(ns("frequencyAnalysisRoute")),
                        # uiOutput(ns("frequencyAnalysisDirectionId")),
                        br()
                 )
               ),
               fluidRow(
                 column(12, align = "center",
                        plotly::plotlyOutput(ns("freqScatterPlot")),
                        br(),
                        br(),
                        tableOutput(ns('renderedTodRefTable'))
                 )
               )
      )
      )
    )
    # fluidRow(column(3,
    #                 withBusyIndicatorUI(
    #                   actionButton(ns("loadFreqData"), "Load Frequency Data")
    #                 )
    #                 )
    #          ),
    # fluidRow(column(5,
    #                 uiOutput(ns("frequencyAnalysisRoute")),
    #                 "Frequencies obtained by calculating time between same route & direction service at stop_sequence = 1",
    #                 br()
    #                 )
    #          ),
    # fluidRow(column(12, align = "center",
    #                 plotly::plotlyOutput(ns("freqScatterPlot")),
    #                 br(),
    #                 br(),
    #                 br(),
    #                 br(),
    #                 tableOutput(ns('renderedTodRefTable'))
    #                 )
    # )
  #)
}

# Server Component -------------------------------------------------------------
frequencyOverview_server <- function(id, rvals){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Start With Load Data Button disabled until GTFS data is loaded in
    shinyjs::disable("loadFreqData")
    shinyjs::hide("loadRouteDensityMap")
    # When Load Data Button Pushed show potential route options for dropdown and load initial scatter plot
    observeEvent(input$loadFreqData, {
      req(rvals$gtfsData)
      shinyjs::disable("loadFreqData") # Disable after pushed until new GTFS Loaded
      withBusyIndicatorServer(ns("loadFreqData"), { # Set Spinner While Loading
        #rvals$allFreqData <- na.omit(gtfsFunctions::calculateFrequenciesByRoute(rvals$gtfsData))
        #selectedGTFS_RouteChoices <- rvals$routeChoices
        rvals$RouteFreqs <-  gtfsFunctions::get_route_frequency(rvals$gtfsData,
                                                 by_directionId = T,
                                                 by_headsign = F,
                                                 by_TOD = T,
                                                 tod_earlyStart   = 14400,
                                                 tod_AMPeakStart  = 22000,
                                                 tod_MiddayStart  = 32400,
                                                 tod_PMPeakStart  = 54000,
                                                 tod_EveningStart = 66600,
                                                 tod_NightStart   = 75600,
                                                 tod_NightEnd     = 86400,
                                                 service_ids = NULL
                                                 )
        # Create Formatted Route ID Column
        rvals$RouteFreqs$formattedRouteId <- as.character(rvals$routeIdAsFormattedRoute[rvals$RouteFreqs$route_id])
        
        rvals$RouteFreqsNoDir <-  get_route_frequency(rvals$gtfsData,
                                                 by_directionId = F,
                                                 by_headsign = F,
                                                 by_TOD = T,
                                                 tod_earlyStart   = 14400,
                                                 tod_AMPeakStart  = 22000,
                                                 tod_MiddayStart  = 32400,
                                                 tod_PMPeakStart  = 54000,
                                                 tod_EveningStart = 66600,
                                                 tod_NightStart   = 75600,
                                                 tod_NightEnd     = 86400,
                                                 service_ids = NULL
        )
        # Create Formatted Route ID Column
        rvals$RouteFreqsNoDir$formattedRouteId <- as.character(rvals$routeIdAsFormattedRoute[rvals$RouteFreqsNoDir$route_id])

        # Show hidden elements
        shinyjs::show("frequencyAnalysisRoute")
        shinyjs::show("frequencyAnalysisPeriod")
        shinyjs::show("freqScatterPlot")
        shinyjs::show("comparisonFreqBarPlot")

      })
    })
    
    # When a new GTFS File is loaded hide the route choices input dropdown and the scatterplot
    # whie showing the load data button
    observeEvent(rvals$gtfsData,{
      shinyjs::hide("frequencyAnalysisRoute")
      shinyjs::hide("frequencyAnalysisPeriod")
      shinyjs::hide("freqScatterPlot")
      shinyjs::hide("comparisonFreqBarPlot")
      shinyjs::enable("loadFreqData")

    })
    
    observeEvent(rvals$RouteFreqs, {
      #req(rvals$RouteFreqs)
      

      # Route Input
      output$frequencyAnalysisRoute <- renderUI({
        selectInput(
          ns("frequencyAnalysisRoute_Input"),
          label = h4("Select a Route"),
          choices = gtools::mixedsort(unique(rvals$RouteFreqs$formattedRouteId)), #selectedGTFS_RouteChoices, #rvals$RouteFreqs$route_id
          selected = gtools::mixedsort(unique(rvals$RouteFreqs$formattedRouteId))[1],
          multiple = FALSE,
          selectize = TRUE
        )
      })
      
      # Route Input
      output$frequencyAnalysisPeriod <- renderUI({
        selectInput(
          ns("frequencyAnalysisPeriod_Input"),
          label = h4("Select a Period of Day"),
          choices = gtools::mixedsort(unique(rvals$RouteFreqs$period)), 
          selected = gtools::mixedsort(unique(rvals$RouteFreqs$period))[1],
          multiple = FALSE,
          selectize = TRUE
        )
      })
      
      # Direction Input
      # output$frequencyAnalysisDirectionId <- renderUI({
      #   checkboxGroupInput(
      #     ns("frequencyAnalysisDirectionId_Input"),
      #     label = h4("Select a Direction_Id"),
      #     choices = gtools::mixedsort(unique(rvals$RouteFreqs$direction_id)), #selectedGTFS_RouteChoices, #rvals$RouteFreqs$route_id
      #     selected = gtools::mixedsort(unique(rvals$RouteFreqs$direction_id))[1],
      #     inline = T#,
      #    # multiple = T,
      #     #selectize = TRUE
      #   )
      # })
    })
    
    # Get TOD Table ============================================================
    todTable <- gtfsFunctions::todTable(earlyStart   = 14400,
                                        AMPeakStart  = 21600,
                                        MiddayStart  = 32400,
                                        PMPeakStart  = 54000,
                                        EveningStart = 66600,
                                        NightStart   = 75600,
                                        NightEnd     = 86400)
    
    # Remove OWL
    todTable <- todTable[Period != 'Owl']
    
    # Transpose TOD #and set column 1 (OWL) to show up last rather than first
    transposedTOD <- t(todTable[, .(Period, timeSpan)])
    
    # Output TOD ref table removing Owl value from 24:00:00-28:00:00 for legibility because we have 00:00:00-04:00:00 also
    output$renderedTodRefTable <- renderTable({transposedTOD},
                                              striped = TRUE, align = 'c', rownames = TRUE, colnames = FALSE)
    
    
    # Update Route Freq Comparison =============================================
    observeEvent(c(input$frequencyAnalysisPeriod_Input, input$loadFreqData), { #, input$frequencyAnalysisDirectionId_Input
      req(rvals$gtfsData)
      req(input$frequencyAnalysisPeriod_Input)
      
      # Add Frequency Category/Class to data
      comparisonFreqData <- data.table::data.table(rvals$RouteFreqsNoDir)[period == input$frequencyAnalysisPeriod_Input]
      
      
      comparisonFreqData[, freqClass := ifelse(median_headways <= 15, "15 Minutes or Better", "Other")]
      comparisonFreqData[, freqClass := ifelse(median_headways > 15 & median_headways <=20, "15-20 Minutes", freqClass)]
      comparisonFreqData[, freqClass := ifelse(median_headways > 20 & median_headways <=30, "20-30 Minutes", freqClass)]
      comparisonFreqData[, freqClass := ifelse(median_headways > 30 & median_headways <=60, "30-60 Minutes", freqClass)]
      comparisonFreqData[, freqClass := ifelse(median_headways > 60, "60+ Minutes", freqClass)]
      
      # Set Desired Legend Order
      comparisonFreqData$freqClass <- factor(comparisonFreqData$freqClass, levels = c("15 Minutes or Better", "15-20 Minutes",
                                                                                      "20-30 Minutes", "30-60 Minutes", "60+ Minutes"))
      
      # Order Routes by median headway
      comparisonFreqData$formattedRouteId <- factor(comparisonFreqData$formattedRouteId, levels = unique(comparisonFreqData$formattedRouteId)[order(comparisonFreqData$median_headways, decreasing = T)])
      
      uniqueRouteCount <- length(unique(comparisonFreqData$formattedRouteId))
      
      # Get List of Routes Per Frequency Type
      highFreq <- unique(gtools::mixedsort(as.character(comparisonFreqData[freqClass == "15 Minutes or Better", formattedRouteId])))
      mediumHighFreq <- unique(gtools::mixedsort(as.character(comparisonFreqData[freqClass == "15-20 Minutes", formattedRouteId])))
      mediumLowFreq <- unique(gtools::mixedsort(as.character(comparisonFreqData[freqClass == "20-30 Minutes", formattedRouteId])))
      lowFreq <- unique(gtools::mixedsort(as.character(comparisonFreqData[freqClass == "30-60 Minutes",  formattedRouteId])))
      veryLowFreq <- unique(gtools::mixedsort(as.character(comparisonFreqData[freqClass == "60+ Minutes",  formattedRouteId])))
      
      # Output Routes of Each Category
      output$highFreqRoutes <- renderText({
        if(length(highFreq) != 0){
          paste('Route(s)', knitr::combine_words(highFreq))
          }else{"None"}
          })
      output$mediumHighFreqRoutes <- renderText({
        if(length(mediumHighFreq) != 0){
          paste('Route(s)', knitr::combine_words(mediumHighFreq))
          }else{"None"}
          })
      output$mediumLowFreqRoutes <- renderText({
        if(length(mediumLowFreq) != 0){
          paste('Route(s)', knitr::combine_words(mediumLowFreq))
          }else{"None"}
          })
      output$lowFreqRoutes <- renderText({
        if(length(lowFreq) != 0){
          paste('Route(s)', knitr::combine_words(lowFreq))
          }else{"None"}
          })
      output$veryLowFreqRoutes <- renderText({
        if(length(veryLowFreq) != 0){
          paste('Route(s)', knitr::combine_words(veryLowFreq))
          }else{"None"}
          })
      
      # Get Plot
      output$comparisonFreqBarPlot <- plotly::renderPlotly({plotly::plot_ly(comparisonFreqData, x = ~median_headways, y = ~formattedRouteId,
                                                                                color = ~freqClass, type = 'bar',
                                                                                height = max(500, uniqueRouteCount*16),
                                                                                text = ~paste0('Route ',formattedRouteId,': ', median_headways, ' min'),
                                                                                hoverinfo = 'text') %>%
          plotly::layout(title = sprintf('Weekday Median Headway Comparison (%s)', input$frequencyAnalysisPeriod_Input),
                         xaxis = list(title = 'Median Headway (Minutes)'),
                         yaxis = list (title = 'Route') #, showticklabels = FALSE
          )%>%
          plotly::config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d",
                                                    "zoomOut2d", "autoScale2d", "resetScale2d", "resetScale2d",
                                                    "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian"))
      })
      # Render a bar chart with a label on the left
      bar_chart <- function(label, width = "100%", height = "16px", fill = "#00bfc4", background = NULL) {
        bar <- div(style = list(background = fill, width = width, height = height))
        chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
        div(style = list(display = "flex", alignItems = "center"), label, chart)
      }
      
      output$testTable <- renderReactable({
        reactable(
          comparisonFreqData[, .(formattedRouteId, median_headways)],#[, freqCass := NULL],
          defaultSorted = c('median_headways'),
          pagination = FALSE,
          columns = list(
            formattedRouteId = colDef(name = "Route"),
            median_headways = colDef(name = "Median Headway (minutes)", align = "left", defaultSortOrder = "asc", cell = function(value) {
              width <- paste0(value / max(comparisonFreqData$median_headways) * 100, "%")
              
              if (value <= 15) {
                color <- "#008000"
              } else if (value > 15 & value <=20) {
                color <- "#e00000"
              } else {
                color <- "#777"
              }
              
              bar_chart(value, width = width, fill = color, background = "#e1e1e1")
            })
          )
        )
      })
      
    })
    
    
    # Show Single Route Frequency Graph ===========================================
    observeEvent(c(input$frequencyAnalysisRoute_Input), { #, input$frequencyAnalysisDirectionId_Input
      req(rvals$gtfsData)
      req(input$frequencyAnalysisRoute_Input)
     # req(input$frequencyAnalysisDirectionId_Input)

      # Subset frequency data to selected route
      freqData <- data.table::data.table(rvals$RouteFreqs)[as.character(formattedRouteId) == as.character(input$frequencyAnalysisRoute_Input)] #& direction_id %in% input$frequencyAnalysisDirectionId_Input
      
      #Exclude Owl Service
      freqData <- freqData[period != "Owl"]
      #data.table::setorder(freqData, direction_id, period)
      
      #   data.table::data.table(rvals$allFreqData)[as.character(route_id) == as.character(input$frequencyAnalysisRoute_Input)]
      # data.table::setorder(freqData, direction_id, period)

      # Get Plot
      output$freqScatterPlot <- plotly::renderPlotly({plotly::plot_ly(freqData, x = ~period, y = ~median_headways,
                                                      type = 'scatter', mode = 'lines+markers', linetype = ~direction_id,
                                                      name = ~paste("Direction:", direction_id),
                                                      text = ~paste0('(',period,', ', median_headways, ' min)'),
                                                      hoverinfo = 'text') %>%
          plotly::layout(title = sprintf('Weekday Route %s Median Headway By Time of Day & Route Direction', input$frequencyAnalysisRoute_Input),
                 xaxis = list(title = 'Time of Day'),
                 yaxis = list (title = 'Median Headway (Minutes)', rangemode = "tozero"),
                 hovermode = "x.unified")%>%
          plotly::config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d",
                                            "zoomOut2d", "autoScale2d", "resetScale2d", "resetScale2d",
                                            "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian"))
      })

    })
 
  })
}




################################################################################

# 
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
# library(shinyhelper)
# 
# library(htmltools)
# library(reactable)
# 
# # Allow input zip file to be up to 200mb in size
# options(shiny.maxRequestSize = 200*1024^2)
# 
# # Source other Module Files
# source("helpers.R") # Load all the code needed to show feedback on a button click
# source("mod_gtfsSidebar.R") # Potentially use local = T
# 
# rvals <- reactiveValues()
# 
# app_ui <- function() {
#   fluidPage(
#     shinyjs::useShinyjs(), # Allow show/hide buttons,
#     gtfsZipInput_ui("gtfsZipInput_ui_1"),# Don't need in final combined version
#     frequencyOverview_ui("frequencyOverview_ui_1")#,
#   )
# }
# 
# app_server <- function(input, output, session) {
#   observe_helpers()  # Use for Shiny Helper
#   gtfsZipInput_server("gtfsZipInput_ui_1", rvals) # Don't need in final combined version
#   frequencyOverview_server("frequencyOverview_ui_1", rvals)
# }
# 
# shinyApp(app_ui, app_server)
# 
# 
# 
# 
# #
# #
# 
# 


# data <- gtfsFunctions::formatGTFSObject("C:\\Users\\ben.tomhave\\OneDrive - AECOM\\Documents\\Projects\\03_GTFS_Data\\MSP_MetroTransit_June21.zip")
# gtfsFunctions::get_route_frequency(data,
#                                    by_directionId = T,
#                                    by_headsign = F,
#                                    by_TOD = T,
#                                    tod_earlyStart   = 14400,
#                                    tod_AMPeakStart  = 22000,
#                                    tod_MiddayStart  = 32400,
#                                    tod_PMPeakStart  = 54000,
#                                    tod_EveningStart = 66600,
#                                    tod_NightStart   = 75600,
#                                    tod_NightEnd     = 86400,
#                                    service_ids = NULL
# )
