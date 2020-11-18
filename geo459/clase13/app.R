library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(raster)
library(shinythemes)
library(ggfortify)
library(plotly)

ui <- navbarPage(title = div(img(src = "logo_labgrs.png", style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
                 id = 'nav',theme = shinytheme("cerulean"), # App theme 
                 tabPanel('Mapa principal', # Main page view
                          div(class="outer",
                           tags$style(type = "text/css",".outer {position: fixed; top: 41px; left: 0;
                                                right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                           leafletOutput(outputId = "map", width = "100%", height = "100%"), # Main map
                          # Option panel
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = F,
                                        draggable = F, top = 90, left = "auto", right = 20, bottom = "auto",
                                        width = 300, height ="auto",
                                        style="z-index:500;",
                                        uiOutput('fechasInput')),
                          conditionalPanel(condition = 'input.map_click != 0',
                                           absolutePanel(id="tSeries",
                                           style="z-index:500;background-color: transparent;
                                           opacity: 1;margin: auto;border-color: transparent;
                                           padding-bottom: 2mm;padding-top: 1mm;",
                                           class = "panel panel-default",
                                           fixed = TRUE,draggable = F, top = 'auto', left = 5,
                                           right = 10, bottom = 10,width = '100%', height = "auto",
                                           plotlyOutput(outputId = 'ts',width = '100%',height = 250))
                                        ),
                          absolutePanel(id = "histo", class = "panel panel-default", fixed = F,
                                        draggable = F, top = 90, left = 10, right = 'auto', bottom = "auto",
                                        width = 400, height ="auto",
                                        style="z-index:500;",
                                        plotlyOutput('histogram')))
                 ),
                 #Description
                 tabPanel('Tab secundario',
                          sidebarLayout(sidebarPanel(
                            fileInput(inputId = 'target_upload', 'Suba su tabla', accept = c('.csv')),br(),
                            checkboxInput("desc", label = "Descomponer serie", value = F),br(),
                            actionButton('plot',label = 'Plot'),
                            downloadButton('descargar','Descargar plot'),
                            fluid=T,width = 2
                          ),
                          mainPanel(plotOutput('decom')))
                 )
)

server <- function(input, output, session) {
  
  #fechas
  dates.table <- read_csv('data/tables/allDates_sst.csv') 
  dates <- dates.table$x
  
  #raster data
  sst.list <- list.files(path = 'data/monthly_sst/',pattern = glob2rx('*sst*.tif'),full.names = T)
  d.select <- sst.list[1] %>% raster()
  #load pixels data
  load('data/tables/pixels.RData')
  
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
  
  # guarda las coordenadas
  xy.map <- eventReactive(input$map_click,{
    click <- isolate({input$map_click})
    clat <- click$lat
    clng <- click$lng
    dts <- SpatialPoints(data.frame(clng,clat),
                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    dts
  })
  
  r.cell <-  reactive({
    a1 <- cellFromXY(d.select,xy.map())
    c <- tabla[a1,3:ncol(tabla)] %>% as.numeric()
    return(c)
  })
  
  #proxy raster plot
  observeEvent(input$fechas,{
    #select file
    n <- which(dates==input$fechas)
    #colorramp
    colores <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090',
                 '#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')
    pal <- colorNumeric(palette = rev(colores), domain = seq(10,25,1), na.color = "transparent")
    #raster file
    r <- sst.list[n] %>% raster()
    #add to map
    leafletProxy('map') %>% clearControls() %>% clearImages() %>% 
      addRasterImage(r,group = 'SST°C', method = 'ngb', colors = pal)
  })
  
  # click markers
  observeEvent(input$map_click,{
    #extraer celda
    n.cell <- cellFromXY(d.select,xy.map())
    #Extraer columna
    n.col <- which(dates==input$fechas)
    #valor
    tabla1 <- tabla[,3:206]
    px.value<- tabla1[n.cell,n.col] %>% as.numeric()
    #coordinates
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng
    #ProxyMap
    leafletProxy("map")  %>% clearMarkers() %>% 
      addMarkers(lng=click$lng, lat=click$lat,label= paste('| lat:',round(clat,3),'| lng',
                                                           round(clng,3), '| SST:',round(px.value,2)),
                 labelOptions = labelOptions(style = list("color" = "black", "font-size" = "14px",
                                                          "font-family" = "serif",
                                                          "font-weight" = "bold")))
  })
  
  output$ts <- renderPlotly({
    ts.px <- ts(r.cell(),start = c(2003,1),end = c(2019,12),frequency = 12)
    g <- ts.px %>% autoplot(ylab = 'SST °C',asp = 0.2)
    #plot
    ggplotly(g)
  })
  
  output$histogram <- renderPlotly({
    req(input$fechas)
    n.col <- which(dates==input$fechas)
    #valor
    tabla1 <- tabla[,3:206]
    dates.value <- tabla1[,n.col] %>% as.data.frame()
    names(dates.value)<- 'valor' 
    #plot
    p <- ggplot(dates.value, aes(x = valor)) + 
      geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
      geom_density(fill = "#ff4d4d", alpha = 0.5) + 
      theme(panel.background = element_rect(fill = '#ffffff'))
    ggplotly(p)
  })
  
######################################################################
  #tab secundario
  #subida de archivos
  datos <- reactive({
    req(input$target_upload)
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read_csv(inFile$datapath)
    df_filter <- df[,'sst'] 
    return(df_filter)
  })
  
  ## creación serie reactiva
  graf <- eventReactive(input$plot,{
    req(!is.null(datos()))
    ts.serie <- ts(datos(),start=c(1981,244),end=c(2020,117),frequency=365)
    
    if(input$desc == T){
    g <- decompose(ts.serie) %>% autoplot()
    }
    if(input$desc == F){
    g <- autoplot(ts.serie, ylab = 'SST °C',asp = 0.2)
    }
    g
  })
  
  #render gráfico
  observeEvent(input$plot,{
    req(!is.null(datos()))
    output$decom <- renderPlot({
      graf()
    })
  })
  
  ## habilitar descarga
  output$descargar <- downloadHandler(
    #nombre de archivo
    filename = "serie.jpg",
    content = function(file){
      ggsave(file, graf(),dpi = 150,width = 15,height = 10)
    }
  )
  

  
}
shinyApp(ui, server)