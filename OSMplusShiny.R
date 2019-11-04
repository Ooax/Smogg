require(rJava)
require(shiny)
require(OpenStreetMap)
require(maps)
require(ggplot2)

ui <- basicPage(
  plotOutput("plot1", click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  map=openmap(c(43.611522,1.428533), c(43.598903,1.461059),type="osm")
  
  output$plot1 <- renderPlot({
    plot(map, raster=TRUE)
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
}

shinyApp(ui, server)