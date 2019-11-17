library(shiny)
library(httr)
library(jsonlite)
library(stringr)
library(tibble)
library(dplyr)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Jakość powietrza"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      textInput(inputId = "town", h3("Miejscowość"), 
                value = ""),
      
      textOutput(outputId = "stacjeMiejscowości")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(

      textOutput(outputId = "textOut")
      
    )
  )
)


server <- function(input, output) {
  
  path <- "http://api.gios.gov.pl/pjp-api/rest/station/findAll"
  info_stacje <- GET(url = path)
  info_stacje <- content(info_stacje, as = "text", encoding = "UTF-8")
  info_stacje_dane <- fromJSON(info_stacje,flatten = TRUE)
  info_stacje_dane_filtered <- info_stacje_dane[c(1,6)]


  observeEvent(input$town, {
    twni <- input$town
    
    stacja_miasto <- info_stacje_dane
    stacja_miasto_tbl <- as_data_frame(stacja_miasto) %>%
      select("id", "city.name", "stationName")  %>%
      filter(city.name == twni)
    proponowane_stacje <- stacja_miasto_tbl %>%
      select("stationName")
    
    
    
    output$stacjeMiejscowości <- renderText({
      paste("Proponowane stacje dla", input$town, ": ", proponowane_stacje)
    })
  })

  # stacja_miasto <- info_stacje_dane
  # stacja_miasto_tbl <- as_data_frame(stacja_miasto) %>%
  #   select("id", "city.name", "stationName")  %>%
  #   filter(city.name == twni)
  #   # filter(str_detect(city.name, "Toruń"))
  # proponowane_stacje <- stacja_miasto_tbl %>%
  #   select("stationName")

  
  output$textOut <- renderText({
    paste("Stacje w wybranym mieście:", info_stacje_dane_filtered )
  })
  
  # output$stacjeMiejscowości <- renderText({
  #   paste("Proponowane stacje dla", input$town, ": ", proponowane_stacje)
  # })

  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)