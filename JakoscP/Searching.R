library(shiny)
library(httr)
library(jsonlite)
library(stringr)
library(tibble)
library(dplyr)
require(leaflet)
library(ggplot2)

ui <- fluidPage(
  
  h1(id="big-heading", "Stan powietrza", icon("leaf")),
  tags$style(HTML("#big-heading{color: darkgreen; font-size: 60px; font-style: oblique; font-family: Times, serif;}")),
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel( tags$head(
      tags$style(type="text/css", ".well { min-width: 255px; max-width:700px }")
    ),
    
    textInput(inputId = "town", h3("Wpisz nazwę miejscowości:", style="color: green; font-size: 30px;"), 
              value = "", width = "100%"),
    
    actionButton("miastoButton", "Wyszukaj", icon("search-location"), 
                 style="color: green; background-color: white; border-color: green; ", width="200px"),
    
    actionButton("zapisButton", "Zapisz jako domyślne", icon("save"), 
                 style="color: white; background-color: green; border-color: green; ", width="200px"),
    
    plotOutput("plotOutputUI", height="600")
    
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width="8", style="border:groove 10px green; padding: 0",
              
              leafletOutput("mymap", height = "80vh"),
              
    )
  )
)




server <- function(input, output) {
  
  colorIcons <- iconList(
    bdobry = makeIcon(iconUrl = "green",
                      iconWidth = 38, iconHeight = 95),
    dobry = makeIcon(iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-red.png",
                     iconWidth = 38, iconHeight = 95)
    
  )
  
  kolor <- function(inputStan){
    returnString = "black"
    if(inputStan == ""){
      returnString = "lightgray"
    }
    if(inputStan == "Bardzo dobry"){
      returnString = "green"
    }
    if(inputStan == "Dobry"){
      returnString = "lightgreen"
    }
    if(inputStan == "Umiarkowany"){
      returnString = "beige"
    }
    if(inputStan == "Dostateczny"){
      returnString = "orange"
    }
    if(inputStan == "Zły"){
      returnString = "red"
    }
    if(inputStan == "Bardzo zły"){
      returnString = "darkred"
    }
    
    
    return(returnString)
    
  }
  
  saveToRDS <- function(){
    twni <- input$town
    # Save a single object to a file
    saveRDS(twni, "wynik.rds")
  }
  
  setTownMarkers <- function(inputTown) {
    # observeEvent(input$town, {
    path <- "http://api.gios.gov.pl/pjp-api/rest/station/findAll"
    info_stacje <- GET(url = path)
    info_stacje <- content(info_stacje, as = "text", encoding = "UTF-8")
    info_stacje_dane <- fromJSON(info_stacje,flatten = TRUE)
    # info_stacje_dane_filtered <- info_stacje_dane[c(1,6)]
    
    twni <- inputTown
    
    stacja_miasto <- info_stacje_dane
    stacja_miasto_tbl <- as_data_frame(stacja_miasto) %>%
      select("id", "city.name", "stationName",  "gegrLat", "gegrLon")  %>%
      filter(city.name == twni)
    proponowane_stacje <- stacja_miasto_tbl %>%
      select("stationName", "id",  "gegrLat", "gegrLon")
    
    
    test_stacji <- as.list(stacja_miasto_tbl)
    
    # TUTAJ ROBIMY WSTAWIANIE WSZYSTKICH PUNKTÓW
    
    lista_stacji <- list()
    
    # print(test_stacji)
    # 
    # print(test_stacji$city.name[1])
    # for(i in test_stacji){
    i <- 1
    while(i<nrow(stacja_miasto_tbl) + 1){
      
      
      
      url_pomiar_stacji <- paste("http://api.gios.gov.pl/pjp-api/rest/aqindex/getIndex/",test_stacji$id[i] , sep = "")
      
      
      dane_stacji <- GET(url = url_pomiar_stacji)
      dane_stacji <- content(dane_stacji, as = "text", encoding = "UTF-8")
      dane_stacji_dane <- fromJSON(dane_stacji,flatten = TRUE)
      
      dane_stacji_lista <- as.list(dane_stacji_dane)
      
      if(length(dane_stacji_lista$so2SourceDataDate) == 1){
        so2_pasted <- paste("SO2", "</br>", dane_stacji_lista$so2SourceDataDate, ": ", dane_stacji_lista$so2IndexLevel$indexLevelName, "</br>")
      }
      else{
        so2_pasted <- ""
      }
      if(length(dane_stacji_lista$no2SourceDataDate) == 1){
        no2_pasted <- paste("NO2", "</br>", dane_stacji_lista$no2SourceDataDate, ": ", dane_stacji_lista$no2IndexLevel$indexLevelName, "</br>")
      }
      else{
        no2_pasted <- ""
      }
      if(length(dane_stacji_lista$coSourceDataDate) == 1){
        co_pasted <- paste("CO", "</br>", dane_stacji_lista$coSourceDataDate, ": ", dane_stacji_lista$coIndexLevel$indexLevelName, "</br>")
      }
      else{
        co_pasted <- ""
      }
      if(length(dane_stacji_lista$pm10SourceDataDate) == 1){
        pm10_pasted <- paste("PM10", "</br>", dane_stacji_lista$pm10SourceDataDate, ": ", dane_stacji_lista$pm10IndexLevel$indexLevelName, "</br>")
        pm10holder <- dane_stacji_lista$pm10IndexLevel$indexLevelName
      }
      else{
        pm10_pasted <- ""
        pm10holder <- ""
      }
      if(length(dane_stacji_lista$pm25SourceDataDate) == 1){
        pm25_pasted <- paste("PM25", "</br>", dane_stacji_lista$pm25SourceDataDate, ": ", dane_stacji_lista$pm25IndexLevel$indexLevelName, "</br>")
      }
      else{
        pm25_pasted <- ""
      }
      if(length(dane_stacji_lista$o3SourceDataDate) == 1){
        o3_pasted <- paste("O3", "</br>", dane_stacji_lista$o3SourceDataDate, ": ", dane_stacji_lista$o3IndexLevel$indexLevelName, "</br>")
      }
      else{
        o3_pasted <- ""
      }
      if(length(dane_stacji_lista$c6h6SourceDataDate) == 1){
        c6h6_pasted <- paste("C6H6", "</br>", dane_stacji_lista$c6h6SourceDataDate, ": ", dane_stacji_lista$c6h6IndexLevel$indexLevelName, "</br>")
      }
      else{
        c6h6_pasted <- ""
      }
      # so2_pasted <- paste("SO2", "</br>", dane_stacji_lista$so2SourceDataDate, ": ", dane_stacji_lista$so2IndexLevel$indexLevelName)
      # no2_pasted <- paste("NO2", "</br>", dane_stacji_lista$no2SourceDataDate, ": ", dane_stacji_lista$no2IndexLevel$indexLevelName)
      # co_pasted <- paste("CO", "</br>", dane_stacji_lista$coSourceDataDate, ": ", dane_stacji_lista$coIndexLevel$indexLevelName)
      # pm10_pasted <- paste("PM10", "</br>", dane_stacji_lista$pm10SourceDataDate, ": ", dane_stacji_lista$pm10IndexLevel$indexLevelName)
      # pm25_pasted <- paste("PM25", "</br>", dane_stacji_lista$pm25SourceDataDate, ": ", dane_stacji_lista$pm25IndexLevel$indexLevelName)
      # o3_pasted <- paste("O3", "</br>", dane_stacji_lista$o3SourceDataDate, ": ", dane_stacji_lista$o3IndexLevel$indexLevelName)
      # c6h6_pasted <- paste("C6H6", "</br>", dane_stacji_lista$c6h6SourceDataDate, ": ", dane_stacji_lista$c6h6IndexLevel$indexLevelName)
      
      
      obiekt_klasy_stacji <- new("stacja", nazwaStacji = test_stacji$stationName[i], idStacji = test_stacji$id[i],
                                 latitude = as.numeric(test_stacji$gegrLat[i]), longitude = as.numeric(test_stacji$gegrLon[i]),
                                 so2 = so2_pasted,
                                 no2 = no2_pasted,
                                 co = co_pasted,
                                 pm10 = pm10_pasted,
                                 pm25 = pm25_pasted,
                                 o3 = o3_pasted,
                                 c6h6 = c6h6_pasted,
                                 pm10prep = pm10holder
      )
      
      lista_stacji[[i]] <- obiekt_klasy_stacji
      
      
      i = i+1
    }
    
    
    map_leaflet <- leaflet()
    map_leaflet <- addTiles(map_leaflet)
    
    i = 1
    while(i<nrow(stacja_miasto_tbl)+1){
      ssAwesome <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'white',
        library = 'ion',
        markerColor = kolor(lista_stacji[[i]]@pm10prep)
      )
      
      print(lista_stacji[[i]]@pm10prep)
      
      
      
      map_leaflet <- addAwesomeMarkers(map_leaflet, lng=lista_stacji[[i]]@longitude, lat = lista_stacji[[i]]@latitude,label=lista_stacji[[i]]@nazwaStacji,
                                       icon = ssAwesome, layerId = lista_stacji[[i]]@idStacji,
                                       popup = paste( lista_stacji[[i]]@so2, lista_stacji[[i]]@no2, lista_stacji[[i]]@co, lista_stacji[[i]]@pm10, lista_stacji[[i]]@pm25, lista_stacji[[i]]@o3, lista_stacji[[i]]@c6h6))
      i = i+1
    }
    
    mymap2 <- renderLeaflet(map_leaflet)
    
    output$mymap <- mymap2
    
    
    # output$stacjeMiejscowoĹ›ci <- renderText({
    #   paste("Stacje: ")
    # })
    # 
    # output$stacjeMiejscowoĹ›ciOut <- renderText({
    #   paste(proponowane_stacje$stationName, "|")
    # })
    
    
    
    # })
    
  }
  
  
  #-------------------------------------------------------------------------------------------------
  #                     KONIEC FUNKCJI setTownMarkers
  #-------------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------------
  #                     FUNKCJA getStationHistory
  #-------------------------------------------------------------------------------------------------
  getStationHistory <- function(markerStatId) {
    
    url_stanowiska <- paste("http://api.gios.gov.pl/pjp-api/rest/station/sensors/",markerStatId , sep = "")
    
    dane_stanowisk <- GET(url = url_stanowiska)
    dane_stanowisk <- content(dane_stanowisk, as = "text", encoding = "UTF-8")
    dane_stanowisk_dane <- fromJSON(dane_stanowisk,flatten = TRUE)
    
    dane_stanowisk_pm10 <- as_data_frame(dane_stanowisk_dane) %>%
      select("id", "stationId", "param.paramCode")  %>%
      filter(param.paramCode == "PM10")
    
    if(nrow(dane_stanowisk_pm10) == 1){
      url_stanowiska_pm10 <- paste("http://api.gios.gov.pl/pjp-api/rest/data/getData/",dane_stanowisk_pm10$id , sep = "")
      
      dane_stanowiska_pm10 <- GET(url = url_stanowiska_pm10)
      dane_stanowiska_pm10 <- content(dane_stanowiska_pm10, as = "text", encoding = "UTF-8")
      dane_stanowiska_pm10_dane <- fromJSON(dane_stanowiska_pm10,flatten = TRUE)
      df <- as.data.frame(dane_stanowiska_pm10_dane$values)
      print(df$value)
      
      x <- c(1,2,3,4,5,6)
      y<-c(234,123,435,467,345,1)
      df2 = data.frame(v1=x, v2=y)
      z = c(1:nrow(df))
      
      dffinal = data.frame(v1 = df$date, v2 = df$value, v3 = z)
      
      
      output$plotOutputUI <- renderPlot({
        ggplot(dffinal, aes(x=v3, y=v2)) +
          geom_line() +
          ggtitle(label="Jakosc powietrza PM10 dla wybranej stacji", subtitle = "Zla jakosc >110") +
          xlab("Dane z ostatnich 3 dni") +
          ylab("Wartosci") +
          theme_light() +
          theme(axis.text.x=element_blank(),
                axis.ticks.x=element_blank())
      })
      
    }
    
  }
  
  #-------------------------------------------------------------------------------------------------
  #                     KONIEC FUNKCJI getStationHistory
  #-------------------------------------------------------------------------------------------------
  
  
  setTownMarkers("")
  
  
  if(file.exists("wynik.rds")){
    my_data <- readRDS("wynik.rds")
    print(my_data)
    
    setTownMarkers(my_data)
  }
  
  # output$mymap <- mymap2
  
  
  # KLASA STACJI
  
  setClass("stacja", slots=list(nazwaStacji="character", idStacji="numeric", latitude="numeric", longitude="numeric",
                                so2="character", no2="character", co="character", pm10="character", pm25="character", o3="character", c6h6="character", pm10prep="character"))
  
  
  
  
  
  
  # path <- "http://api.gios.gov.pl/pjp-api/rest/station/findAll"
  # info_stacje <- GET(url = path)
  # info_stacje <- content(info_stacje, as = "text", encoding = "UTF-8")
  # info_stacje_dane <- fromJSON(info_stacje,flatten = TRUE)
  # info_stacje_dane_filtered <- info_stacje_dane[c(1,6)]
  
  observeEvent(input$zapisButton, saveToRDS())
  
  
  
  #------------------------------------------------------------------------------------------------------
  
  # PRZYCISK DO WYSZUKIWANIA STACJI W PODANYM MIEĹšCIE
  observeEvent(input$miastoButton, setTownMarkers(input$town))
  
  # WCISKANIE MARKERA ABY OTRZYMAc ID STACJI
  observeEvent(
    input$mymap_marker_click, {
      markerStationId <- input$mymap_marker_click$id
      getStationHistory(markerStationId)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
