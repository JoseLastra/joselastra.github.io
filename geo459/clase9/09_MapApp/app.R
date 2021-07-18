library(shiny)
library(leaflet)
library(htmltools)
library(tidyverse)
library(sf)
library(shinybusy)

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
                                           'Viviendas'='TOTAL_VIVI')),hr(),
                htmlOutput("mouse_over"),hr(),
                htmlOutput("mouse_click"),hr(),
                htmlOutput("mouse_center"),hr(),
                htmlOutput("mouse_zoom"),hr(),
                htmlOutput("mouse_bounds")
  )
)

server <- function(input, output, session) {
  #Load data
  shp <- read_sf('data/viña_del_mar.gpkg') %>% st_transform(4326)
  ## custom labels for map
  shp$label_pop <- paste0('<strong>', 'Total población:' , '</strong>', shp$PERSONAS,'</br>',
                          '<strong>', 'Densidad:' , '</strong>',shp$DENSIDAD) %>% 
    lapply(htmltools::HTML)
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
    pal <- colorQuantile(palette = "viridis",domain = dominio,n = 7)
    #proxy Map
    proxyMap <- leafletProxy('map') 
    
    if(input$campo[[1]] == 'PERSONAS'){
      proxyMap <- proxyMap %>% clearShapes() %>% clearControls() %>% 
        addPolygons(data = shp,group = 'Manzanas', fillColor = ~pal(PERSONAS), fillOpacity = 0.7,
                    stroke = 0.1,color = 'white',weight = 1, smoothFactor = 0.2, 
                    label = ~label_pop, 
                    labelOptions = labelOptions(style = list("font-size" = "14px"))) %>%
        addLegend("bottomright", pal = pal, values = dominio,
                  title = "Total de personas por manzana censal",
                  opacity = 0.7,group = 'Leyenda')
    }
    if(input$campo[[1]] == 'TOTAL_VIVI'){
      proxyMap <- proxyMap %>% clearShapes() %>% clearControls() %>% 
        addPolygons(data = shp,group = 'Manzanas', fillColor = ~pal(TOTAL_VIVI), fillOpacity = 0.7,
                    stroke = 0.1,color = 'white',weight = 1, smoothFactor = 0.2, 
                    popup = ~htmlEscape(TOTAL_VIVI))%>%
        addLegend("bottomright", pal = pal, values = dominio,
                  title = "Total de viviendas por manzana censal",
                  opacity = 0.7,group = 'Manzanas')
    }
    if(input$campo[[1]] == 'DENSIDAD'){
      proxyMap <- proxyMap %>% clearShapes() %>% clearControls() %>% 
        addPolygons(data = shp,group = 'Manzanas', fillColor = ~pal(DENSIDAD), fillOpacity = 0.7,
                    stroke = 0.1,color = 'white',weight = 1, smoothFactor = 0.2, 
                    label = ~htmlEscape(DENSIDAD))%>%
        addLegend("bottomright", pal = pal, values = dominio,
                  title = "Densidad (personas/hectáreas)",
                  opacity = 0.7,group = 'Manzanas')
    }
    #print map
    proxyMap
    
  })
 
  #mouse events
  #mouseover
  observeEvent(input$map_shape_mouseover,{
    data_mouse_over <- input$map_shape_mouseover
    output$mouse_over <- renderText({
      paste('<b>Mouse shape over: </b>',round(data_mouse_over$lat,digits = 4),'|',
            round(data_mouse_over$lng,digits = 4))
    })
  })
  #click
  observeEvent(input$map_click,{
    data_mouse_click <- input$map_click
    output$mouse_click <- renderText({
      paste('<b>Mouse map click: </b>',round(data_mouse_click$lat,digits = 4),'|',
            round(data_mouse_click$lng,digits = 4))
    })
  })
  #center
  observeEvent(input$map_center,{
    data_center <- input$map_center
    output$mouse_center <- renderText({
      paste('<b>Mouse map center: </b>',round(data_center$lat,digits = 4),'|',
            round(data_center$lng,digits = 4))
    })
  })
  #zoom
  observeEvent(input$map_zoom,{
    data_zoom <- input$map_zoom
    output$mouse_zoom <- renderText({
      paste('<b>Mouse map zoom: </b>',data_zoom)
    })
  })
  #bounds
  observeEvent(input$map_bounds,{
    data_bounds <- input$map_bounds
    output$mouse_bounds <- renderText({
      paste('<b>Map bounds </b>','</br>',
            '<b>Norte: </b>',round(data_bounds$north,digits = 4),'</br>',
            '<b>Sur: </b>',round(data_bounds$south,digits = 4),'</br>',
            '<b>Este: </b>',round(data_bounds$east,digits = 4),'</br>',
            '<b>Oeste: </b>',round(data_bounds$west,digits = 4),'</br>')
    })
  })
}

#compilar
shinyApp(ui,server)