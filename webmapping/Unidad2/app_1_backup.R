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
                class = "panel panel-default",
                selectInput(inputId = "campo",label = 'Seleccione un campo de la lista',
                            choices = c("T_HOM","T_MUJ","T_POB","T_VIV"), selected = "T_POB")
  )
)

############SERVER###################
server <- function(input, output, session) {
  #Load data
  valpo <- read_sf('censoINE_Valparaiso2017.gpkg') %>% st_transform(4326)
  ## rendering base map
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      fitBounds(lng1 =-109.5 ,lat1 =-33.96 ,lng2 =-69.9 ,lat2 = -26.29) #ajustar extensión inicial del mapa
  })
  
  #mapa temático en proxy
  observeEvent(input$campo, {
    tabla <- valpo %>% as.data.frame() # convirtiendo sf a data frame simple
    valores <- tabla[,input$campo] %>% as.numeric() #extrayendo valores de columna
    pal <- colorQuantile(palette = "viridis",domain = valores,n = 10)#paleta
    pal_colors <-  sort(valores) %>% pal() %>% unique() #Colores para leyenda
    pal_labels <- quantile(valores, seq(0, 1, 0.1)) # creación de quantiles para etiquetas
    pal_labels <- paste(lag(pal_labels), pal_labels, sep = " - ")[-1] # removemos el primer lag por ser NA
    
    # mapa reactivo
    proxyMap <- leafletProxy('map') %>% clearControls() %>% clearShapes() 
    
    if(input$campo == "T_POB"){
     proxyMap %>% addPolygons(data = valpo,group = 'CENSO INE, 2017', fillColor = ~pal(T_POB), 
                               fillOpacity = 0.8,stroke = T,weight = 0.25, smoothFactor = 0.2,
                              label = ~T_POB,labelOptions = labelOptions(style = list("color" = "black", 
                                                                                      "font-size" = "16px",
                                                                                      "font-family" = "serif",
                                                                                      "font-weight" = "bold")),
                              highlight = highlightOptions(weight = 5,color = "white",
                                fillOpacity = 1,bringToFront = TRUE)) %>%
        addLegend("bottomleft", colors = pal_colors, labels = pal_labels, 
                  title = "Total de personas por comuna",opacity = 0.8,group = 'Leyenda') %>% 
        addLayersControl(overlayGroups = c('CENSO INE, 2017'),position ="bottomleft")
    }
    if(input$campo == "T_HOM"){
    proxyMap %>% addPolygons(data = valpo,group = 'CENSO INE, 2017', fillColor = ~pal(T_HOM), 
                             fillOpacity = 0.8, stroke = T,weight = 0.25, smoothFactor = 0.2) %>%
        addLegend("bottomleft", colors = pal_colors, labels = pal_labels, 
                  title = "Total de hombres por comuna",opacity = 0.8,group = 'Leyenda')%>% 
        addLayersControl(overlayGroups = c('CENSO INE, 2017'),position ="bottomleft")
    }
    if(input$campo == "T_MUJ"){
    proxyMap %>% addPolygons(data = valpo,group = 'CENSO INE, 2017', fillColor = ~pal(T_MUJ), 
                             fillOpacity = 0.8,stroke = T,weight = 0.25, smoothFactor = 0.2) %>%
        addLegend("bottomleft", colors = pal_colors, labels = pal_labels, 
                  title = "Total de mujeres por comuna",opacity = 0.8,group = 'Leyenda')%>% 
        addLayersControl(overlayGroups = c('CENSO INE, 2017'),position ="bottomleft")
    }
    if(input$campo == "T_VIV"){
    proxyMap %>% addPolygons(data = valpo,group = 'CENSO INE, 2017', fillColor = ~pal(T_VIV), 
                             fillOpacity = 0.8, stroke = T,weight = 0.25, smoothFactor = 0.2) %>%
        addLegend("bottomleft", colors = pal_colors, labels = pal_labels, 
                  title = "Total de viviendas por comuna",opacity = 0.8,group = 'Leyenda')%>% 
        addLayersControl(overlayGroups = c('CENSO INE, 2017'),position ="bottomleft")
    }
  })
  
  
}

shinyApp(ui,server)