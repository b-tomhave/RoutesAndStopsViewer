# Module Component 1: Create UI Select Input Drop Down of Possible GTFS Text Tiles To Explore
# gtfsTxtFileDropdown_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#     selectInput(
#       ns("gtfsTxtFileSelect"),
#       label = h4("Select a Key GTFS File to View Details"),
#       choices = names(rvals)[!(names(gtfs3) %in% c('.'))],
#       selected = NULL,
#       multiple = FALSE,
#       selectize = TRUE
#     )
#   )
# }

gtfsTxtFileDropdown_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('gtfsTxtFileSelectUI'))
  )
}



gtfsTxtFileDropdown_server <- function(id, rvals){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$gtfsTxtFileSelectUI <- renderUI({
      req(rvals$gtfsData)
      div(
        selectInput(
          ns("gtfsTxtFileSelect"),
          label = h4("Select a Key GTFS File to View Details"),
          choices = names(rvals$gtfsData)[!(names(rvals$gtfsData) %in% c('.'))],
          selected = 'routes',
          multiple = FALSE,
          selectize = TRUE
        )
      )
    })
    
    
    observeEvent(input$gtfsTxtFileSelect,{
      rvals$selectedGtfsTxtFile <- input$gtfsTxtFileSelect
    })
    
    

  })
}
  

# Module Component 1: Create Table to visualize selected gtfs TXT file
tabularGTFS_ui <- function(id){
  ns <- NS(id)
  tagList(
      h3(textOutput(ns("selectedTxtFile"))),
      textOutput(ns("formattedRouteNameExplainText")),
      br(),
      DT::dataTableOutput(ns("txtTable"))
  )
}



tabularGTFS_server <- function(id, rvals){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get Selected Text File
    selectedTxt <- reactive({rvals$selectedGtfsTxtFile})
    
    
    observeEvent(selectedTxt(),{
      # Print Output Text Indicating Selected File
      output$selectedTxtFile <- renderText({
        paste0("Showing: ", selectedTxt(), ".txt File")
      })
      
      if (selectedTxt() == "routes"){
        output$formattedRouteNameExplainText <- renderText({
          "'formattedRouteName' in the routes.txt table is the route selection field on the interactive map tab."
        })
      }else{
        output$formattedRouteNameExplainText <- renderText({''})
      }
    })
    


    # Return Data.Table For Selected GTFS File when either a new GTFS file has been loaded or the selected txt file changes
    observeEvent(c(selectedTxt(), rvals$gtfsData), {
      req(rvals$gtfsData) # Make sure gtfs is already loaded
      req(selectedTxt()) # Make sure selected txt file isn't null
      output$txtTable <- DT::renderDataTable(DT::datatable(rvals$gtfsData[[as.character(selectedTxt())]], filter = 'top',
                                                              options = list(pageLength = 50, ordering=T),
                                                              rownames= FALSE,
                                                              escape = FALSE,
                                                              selection = 'none')) # Set Strings as Factor so that filter is a dropdown not typing
    }, ignoreNULL = FALSE
    )

  })
}




################################################################################
# 
# 
# # Main application
# library(shiny)
# library(DT)
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
#     gtfsTxtFileDropdown_ui("gtfsTxtFileDropdown_ui_1"),
#     tabularGTFS_ui("tabularGTFS_ui_1")
#   )
# }
# 
# app_server <- function(input, output, session) {
#   gtfsZipInput_server("gtfsZip_1", rvals) # This would not be included in final version but is included here to load data
#   gtfsTxtFileDropdown_server("gtfsTxtFileDropdown_ui_1", rvals)
#   tabularGTFS_server("tabularGTFS_ui_1", rvals)
# }
# 
# shinyApp(app_ui, app_server)

