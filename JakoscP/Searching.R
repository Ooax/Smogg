library(shiny)
library(httr)
library(jsonlite)
library(stringr)
library(tibble)
library(dplyr)
require(leaflet)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Jakość powietrza"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      textInput(inputId = "town", h3("Miejscowość/Stacja"), 
                value = ""),
      actionButton("miastoButton", "Wyszukaj w miejscowości"),
      
      actionButton("stacjaButton", "Wyszukaj stację"),
      
      textOutput(outputId = "stacjeMiejscowości"),
      
      textOutput(outputId = "stacjeMiejscowościOut")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      leafletOutput("mymap")
      
    )
  )
)


server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  # path <- "http://api.gios.gov.pl/pjp-api/rest/station/findAll"
  # info_stacje <- GET(url = path)
  # info_stacje <- content(info_stacje, as = "text", encoding = "UTF-8")
  # info_stacje_dane <- fromJSON(info_stacje,flatten = TRUE)
  # info_stacje_dane_filtered <- info_stacje_dane[c(1,6)]
  
  # PRZYCISK DO WYSZUKIWANIA STACJI W PODANYM MIEŚCIE
  observeEvent(input$miastoButton, {
    # observeEvent(input$town, {
    path <- "http://api.gios.gov.pl/pjp-api/rest/station/findAll"
    info_stacje <- GET(url = path)
    info_stacje <- content(info_stacje, as = "text", encoding = "UTF-8")
    info_stacje_dane <- fromJSON(info_stacje,flatten = TRUE)
    # info_stacje_dane_filtered <- info_stacje_dane[c(1,6)]
    
    twni <- input$town
    
    stacja_miasto <- info_stacje_dane
    stacja_miasto_tbl <- as_data_frame(stacja_miasto) %>%
      select("id", "city.name", "stationName")  %>%
      filter(city.name == twni)
    proponowane_stacje <- stacja_miasto_tbl %>%
      select("stationName")
    
    
    
    output$stacjeMiejscowości <- renderText({
      paste("Proponowane stacje: ")
    })
    
    output$stacjeMiejscowościOut <- renderText({
      paste(proponowane_stacje$stationName, "|")
    })
    
    
    
    # })
    
  })
  
  
  # PRZYCISK DO WYSZUKIWANIA DANYCH DLA DANEJ STACJI POMIAROWEJ
  observeEvent(input$stacjaButton, {
    # observeEvent(input$town, {
    path <- "http://api.gios.gov.pl/pjp-api/rest/station/findAll"
    info_stacje <- GET(url = path)
    info_stacje <- content(info_stacje, as = "text", encoding = "UTF-8")
    info_stacje_dane <- fromJSON(info_stacje,flatten = TRUE)
    # info_stacje_dane_filtered <- info_stacje_dane[c(1,6)]
    
    twni <- input$town
    
    stacja_miasto <- info_stacje_dane
    stacja_miasto_tbl <- as_data_frame(stacja_miasto) %>%
      select("id", "stationName", "gegrLat", "gegrLon")  %>%
      filter(stationName == twni)
    proponowane_stacje <- stacja_miasto_tbl %>%
      select("id")
    
    
    
    output$stacjeMiejscowości <- renderText({
      paste("Dane dla wybranej stacji: ")
    })
    
    path2 <- paste("http://api.gios.gov.pl/pjp-api/rest/aqindex/getIndex/",proponowane_stacje$id, sep = "")
    
    dane_stacji <- GET(url = path2)
    dane_stacji <- content(dane_stacji, as = "text", encoding = "UTF-8")
    dane_stacji_dane <- fromJSON(dane_stacji,flatten = TRUE)
    
    dane <- dane_stacji_dane
    # dane_no2 <- as_data_frame(dane_stacji_dane) %>%
    dane_pomiarow <- as.list(dane) 
    
    
    st <- c("ST z godziny: ", dane_pomiarow$stSourceDataDate, ": ", dane_pomiarow$stIndexLevel$indexLevelName)
    so2 <- c("SO2 z godziny: ", dane_pomiarow$so2SourceDataDate, ": ", dane_pomiarow$so2IndexLevel$indexLevelName)
    no2 <- c("NO2 z godziny: ", dane_pomiarow$no2SourceDataDate, ": ", dane_pomiarow$no2IndexLevel$indexLevelName)
    co <- c("CO z godziny: ", dane_pomiarow$coSourceDataDate, ": ", dane_pomiarow$coIndexLevel$indexLevelName)
    pm10 <- c("PM10 z godziny: ", dane_pomiarow$pm10SourceDataDate, ": ", dane_pomiarow$pm10IndexLevel$indexLevelName)
    pm25 <- c("PM25 z godziny: ", dane_pomiarow$pm25SourceDataDate, ": ", dane_pomiarow$pm25IndexLevel$indexLevelName)
    o3 <- c("O3 z godziny: ", dane_pomiarow$o3SourceDataDate, ": ", dane_pomiarow$o3IndexLevel$indexLevelName)
    c6h6 <- c("C6H6 z godziny: ", dane_pomiarow$c6h6SourceDataDate, ": ", dane_pomiarow$c6h6IndexLevel$indexLevelName)
    
    lista_jakosci <- list(st,so2,no2,co,pm10,pm25,o3, c6h6)
    
    output$stacjeMiejscowościOut <- renderText({
      paste(lista_jakosci)
    })
    
    # output$stacjeMiejscowościOut <- renderText({
    #   paste("NO2 z godziny: ", dane_pomiarow$no2SourceDataDate, ": ", dane_pomiarow$no2IndexLevel$indexLevelName)
    # })
    
    gLongitude <- stacja_miasto_tbl$gegrLon
    gLatitude <- stacja_miasto_tbl$gegrLat
    
    
    # output$stacjeMiejscowościOut <- renderText({
    #   paste(dane_no2$no2SourceDataDate)
    # })
    
    
    # })
    
  })
  
  
  
  
  
  ######################################## STATION COORDINATES
  
  
  
  
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)