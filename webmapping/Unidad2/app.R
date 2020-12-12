library(shiny)
library(tidyverse)
library(sf)
library(rgdal)
library(raster)
library(plotly)
library(leaflet)

########### UI #######################
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput('map', width = "100%", height = "100%"),
  absolutePanel(id="controls",
                style="z-index:500;", top = 90, left = "auto", right = 20, 
                bottom = "auto",
                width = 400, height ="auto",
                class = "panel panel-default"
  )
)

############SERVER###################
server <- function(input, output, session) {
  #Load data
  valpo <- read_sf('censoINE_Valparaiso2017.gpkg') %>% st_transform(4326)
  pvl <- read_sf('puntosVerdesyLimpios_MMA.gpkg') %>% st_transform(4326)
  ## rendering base map
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      fitBounds(lng1 =-109.5 ,lat1 =-33.96 ,lng2 =-69.9 ,lat2 = -26.29) #ajustar extensión inicial del mapa
  })
}

shinyApp(ui,server)