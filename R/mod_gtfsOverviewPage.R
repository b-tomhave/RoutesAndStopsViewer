# Module Component 1: Create UI Select Input Drop Down of Possible GTFS Text Tiles To Explore
gtfsOverviewPage_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Overview of GTFS Files"),
    h5(id= "gtfsFileLoadWarning", "GTFS Zip File Must Be Loaded on 'Explorer' tab to load data below."),
    #gtfsZipInput_ui(ns("gtfsZip_1")), # This would not be included in final version but is included here to load data
    fluidRow(
      column(8,
             gtfsSummaryBoxes_ui(ns("gtfsSummaryBoxes_ui_1"))
             ),
      column(1),
      column(3,
             gtfsTxtFileDropdown_ui(ns("gtfsTxtFileDropdown_ui_1"))
             )
    ),
    hr(),br(), #Insert blank space and horizontal line
    tabularGTFS_ui(ns("tabularGTFS_ui_1"))
  )
}



gtfsOverviewPage_server <- function(id, rvals){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #gtfsZipInput_server("gtfsZip_1", rvals) # This would not be included in final version but is included here to load data
    gtfsSummaryBoxes_server("gtfsSummaryBoxes_ui_1", rvals)
    gtfsTxtFileDropdown_server("gtfsTxtFileDropdown_ui_1", rvals)
    tabularGTFS_server("tabularGTFS_ui_1", rvals)
    
    
  })
}


################################################################################

# 
# # Main application
# library(shiny)
# library(DT)
# 
# rvals <- reactiveValues()
# 
# # Source other Module Files
# # source("mod_gtfsSidebar.R")
# # source("mod_gtfsSummaryStats.R") # Potentially use local = T
# # source("mod_tabularGTFS.R") # Potentially use local = T
# 
# # Allow input zip file to be up to 200mb in size
# options(shiny.maxRequestSize = 200*1024^2)
# 
# app_ui <- function() {
#   fluidPage(
#     h2("Overview of GTFS Files"),
#     h5(id= "gtfsFileLoadWarning", "GTFS Zip File Must Be Loaded on 'Explorer' tab to load data below."),
#     tags$style(HTML("#gtfsFileLoadWarning{color: red; font-size: 10px;}")), # Set warning text to be red
#     gtfsOverviewPage_ui("gtfsOverviewPage_ui_1")
#   )
# }
# 
# app_server <- function(input, output, session) {
#   gtfsOverviewPage_server("gtfsOverviewPage_ui_1", rvals)
# }
# 
# shinyApp(app_ui, app_server)
