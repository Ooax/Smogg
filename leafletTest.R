require(rJava)
require(shiny)
require(leaflet)

ui <- basicPage(
  leafletOutput("mymap"),
  p()
)

server <- function(input, output, session) {
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
}
  
shinyApp(ui, server)