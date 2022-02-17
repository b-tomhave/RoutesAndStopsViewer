# Module to Create Leaflet Base Map ----


mod_leaflet_map_base_ui <- function(id) {
  ns <- NS(id)
  tagList()
  # use_waiter()
}


mod_leaflet_map_base_server <- function(id){
  moduleServer(id, function(input, output, session) {
  ns <- session$ns
  # w <- Waiter$new()#, html="Please wait")#, hide_on_render=T)
  
  output$ns <- renderLeaflet(quoted = F, {
    # w$show()
    # waiter_show()
    leaflet() %>%
      setView(lat = 39.809253334942575,
              lng = -98.55663889876627,
              zoom = 5) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       group   = "Sketch (Default)") %>%
      addProviderTiles(providers$Stamen.Toner,
                       group   = "Black & White") %>%
      addProviderTiles(providers$OpenStreetMap,
                       group   = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group   = "Aerial") %>%
      addLayersControl(
        baseGroups = c("Sketch (Default)", "Black & White", "OpenStreetMap","Aerial"),
        options    = layersControlOptions(collapsed = T),
        position = c("topleft")) %>%
      addMeasure(position = c("topleft")) %>% #Add distance (and area measure)
      leaflet::addScaleBar(position = c("bottomleft"))

    # waiter_hide()
    # w$hide
  })
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
# 
# # Allow input zip file to be up to 200mb in size
# options(shiny.maxRequestSize = 200*1024^2)
# 
# app_ui <- function() {
#   fluidPage(
#     mod_leaflet_map_base_ui("baseMap")
#   )
# }
# 
# app_server <- function(input, output, session) {
#   mod_leaflet_map_base_server("baseMap")
# }
# 
# shinyApp(app_ui, app_server)
