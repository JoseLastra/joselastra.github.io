library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(shinybusy)
# Funciones
source('dataPlot.R')
################

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"), #esto hace la magia
  leafletOutput(outputId = 'map', width = "100%", height = "100%"),
  absolutePanel(id="controls",
                style="z-index:500;", top = 90, left = "auto", right = 20, 
                bottom = "auto",
                width = 400, height ="auto",
                class = "panel panel-default",
                selectInput(inputId = 'campo',label = 'Seleccione variable a visualizar',
                            choices = list('Total personas' = 'PERSONAS',
                                           'Densidad (per/ha)'='DENSIDAD',
                                           'Viviendas'='TOTAL_VIVI')),
                actionButton(inputId = 'plot',label = 'Presione para ver el histograma'),
                plotOutput(outputId = 'grafico')
  )
)

server <- function(input, output, session) {
  #Load data
  shp <- read_sf('data/viña_del_mar.gpkg') %>% st_transform(4326)
  ## rendering base map
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter)%>% 
      fitBounds(lng1 =-71.586 ,lat1 =-33.105 ,lng2 =-71.45 ,lat2 = -32.944) %>%
      addLayersControl(overlayGroups = 'Manzanas',position = 'topleft')
  })
  #proxy map changes
  observe({
    
    #preparando paletas
    tabla <- shp[,] %>% as.data.frame() 
    dominio <- tabla[, input$campo[[1]]]
    #creando paleta de colores dinámica
    pal <- colorBin(palette = "viridis",domain = dominio)
    #proxy Map
    proxyMap <- leafletProxy('map') %>% clearShapes()
    
    if(input$campo[[1]] == 'PERSONAS'){
      proxyMap <- proxyMap %>% 
        addPolygons(data = shp,group = 'Manzanas', fillColor = ~pal(PERSONAS), fillOpacity = 0.7,
                    stroke = 0.1,color = 'white',weight = 1, smoothFactor = 0.2)
    }
    if(input$campo[[1]] == 'TOTAL_VIVI'){
      proxyMap <- proxyMap %>% 
        addPolygons(data = shp,group = 'Manzanas', fillColor = ~pal(TOTAL_VIVI), fillOpacity = 0.7,
                    stroke = 0.1,color = 'white',weight = 1, smoothFactor = 0.2)
    }
    if(input$campo[[1]] == 'DENSIDAD'){
      proxyMap <- proxyMap %>% 
        addPolygons(data = shp,group = 'Manzanas', fillColor = ~pal(DENSIDAD), fillOpacity = 0.7,
                    stroke = 0.1,color = 'white',weight = 1, smoothFactor = 0.2)
    }
    #print map
    proxyMap
    
  })
  
  output$grafico <- renderPlot({
    req(input$plot) # require
    #evitar comportamiento no deseado al cambiar el campo
    campo <- isolate({input$campo[[1]]})
    
    dataPlot(datos = shp,campo = campo)
  })
  
}

#compilar
shinyApp(ui,server)