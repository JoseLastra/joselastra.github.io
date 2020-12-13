library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(raster)
library(plotly)
library(shinythemes)
library(ggfortify)
##########UI####################
ui <- navbarPage(title = "SST data",theme = shinytheme('cerulean'),
                 #primer panel
                 tabPanel(title = "Mapa principal",
                          div(class='outer',
                          tags$style(type = "text/css",".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0;
                                         overflow: hidden; padding: 0}"),
                          leafletOutput(outputId = "map", width = "100%", height = "100%"), # Main map
                          # Option panel
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = F,
                                        draggable = F, top = 90, left = "auto", right = 20, bottom = "auto",
                                        width = 300, height ="auto",
                                        style="z-index:500;",
                                        uiOutput('fechasInput')),
                          absolutePanel(id="tSeries",
                                        style="z-index:500;background-color: transparent;
                                                         opacity: 1;margin: auto;border-color: transparent;
                                                         padding-bottom: 2mm;padding-top: 1mm;",
                                        class = "panel panel-default",
                                        fixed = TRUE,draggable = F, top = 'auto', left = 5,
                                        right = 10, bottom = 10,width = '100%', height = "auto",
                                        plotlyOutput(outputId = 'ts',height = "250px"))
                          )
                          ),
                 #segundo panel
                 tabPanel(title = "Tab secundario")
  
)

server <- function(input, output, session) {
  #fechas
  dates.table <- read_csv('data/tables/allDates_sst.csv') 
  dates <- dates.table$x
  #raster data
  sst.list <- list.files(path = 'data/monthly_sst/',pattern = glob2rx('*sst*.tif'),full.names = T)
  #cargar una banda para seleccionar el número de pixel
  d.select <- sst.list[1] %>% raster()
  #load pixels data
  load('data/tables/pixels.RData')
  
  #reactive coordinates
  xy.map <- eventReactive(input$map_click,{
    click <- isolate({input$map_click})
    clat <- click$lat
    clng <- click$lng
    dts <- SpatialPoints(data.frame(clng,clat),
                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    dts
  })
  #Creando vector con la serie
  r.cell <-  reactive({
    a1 <- cellFromXY(d.select,xy.map())
    c <- tabla[a1,3:ncol(tabla)] %>% as.numeric()
    return(c)
  })
  
  #renderizar la ui
  output$fechasInput <- renderUI({
    selectInput(inputId = 'fechas',label = 'Seleccione una fecha',
                choices = rev(dates),selected = tail(dates,1))
  })
  #basemap
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -71.5, lat = -33.12,zoom = 6)
  })
  
  #render plotly
  output$ts <- renderPlotly({
    ts.px <- ts(r.cell(),start = c(2003,1),end = c(2019,12),frequency = 12)
    g <- ts.px %>% autoplot(ylab = 'SST °C',asp = 0.2)
    #plot
    ggplotly(g)
  })
  
  #proxy raster plot
  observeEvent(input$fechas,{
    #select file
    n <- which(dates==input$fechas)
    #colorramp
    colores <- c("#ad0000","#ff0000","#ff9901","#fbff01","#11ff01","#05e8ff","#0519ff","#030d81")
    pal <- colorNumeric(palette = rev(colores), domain = seq(10,25,1), na.color = "transparent")
    #raster file
    r <- sst.list[n] %>% raster()
    #add to map
    leafletProxy('map') %>% clearControls() %>% clearImages() %>% 
      addRasterImage(r,group = 'MODIS Aqua 4km', method = 'ngb', colors = pal) %>% 
      addLegend(pal = pal, values = seq(10,25,1),title = "SST °C",group = 'Leyenda',
                position = 'topleft',layerId = 'anoma') %>% 
      addLayersControl(overlayGroups = c('MODIS Aqua 4km'),position = 'topleft')
  })
  
  
  # click markers
  observeEvent(input$map_click,{
    click <- input$map_click
    clat <- click$lat %>% round(4)
    clng <- click$lng %>% round(4)
    leafletProxy("map")  %>% clearMarkers() %>% addMarkers(lng=click$lng, lat=click$lat, 
                                                           label= paste('lat:',clat,'lng',clng))
  })
}

shinyApp(ui, server)