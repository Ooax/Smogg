require(rJava)
require(shiny)
require(OpenStreetMap)
require(maps)
require(ggplot2)


map=openmap(c(43.611522,1.428533), c(43.598903,1.461059),type="osm")
plot(map, raster=TRUE)


