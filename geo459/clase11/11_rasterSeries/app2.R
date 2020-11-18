library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(raster)
library(dygraphs)
library(plotly)
library(shinythemes)
library(ggfortify)

ui <- navbarPage(title = 'RasterSeries',id = 'nav',theme = shinytheme("spacelab"), # App theme 
                 
                 tabPanel('Mapa principal', # Main page view
                          leafletOutput("map", width = "100%", height = 600), # Main map
                          # Option panel
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = F,
                                        draggable = F, top = 90, left = "auto", right = 20, bottom = "auto",
                                        width = 300, height ="auto",
                                        style="z-index:500;", ## html
                                        uiOutput('fechasInput')),
                          #time series
                          dygraphOutput("ts",width = '100%',height = 300),
                          plotlyOutput("ts2",width = '100%',height = 300),
                          absolutePanel(top = 85,left = 80,right = 'auto',width = 200,
                                        height = 200,bottom = 'auto',fixed = T,style="z-index:500",
                                        HTML('<img height="90"  src="logo_labgrs.png" asp="1" 
                                          class="img-responsive" align="left">')
                                        )
                 ),
                 #Description
                 tabPanel('Tab secundario',
                          sidebarLayout(sidebarPanel(
                            fileInput('target_upload', 'Suba su tabla',
                                      accept = c('.csv')),br(),
                            checkboxInput("desc", label = "Descomponer serie", value = F),br(),
                            actionButton('plot',label = 'Plot'),
                            downloadButton('descargar','Descargar plot'),
                            fluid=T,width = 2),
                            mainPanel(plotOutput('decom'))
                          ))
)

server <- function(input, output, session) {
  #raster data
  sst.list <- list.files(path = 'data/monthly_sst/',pattern = glob2rx('*sst*.tif'),full.names = T)
  d.select <- sst.list[1] %>% raster()
  #fechas
  dates.table <- read_csv('data/tables/allDates_sst.csv') 
  dates <- dates.table$x
  #load pixels data
  load('data/tables/pixels.RData')
  ########################################
  output$fechasInput <- renderUI({
    selectInput(inputId = 'fechas',label = 'Seleccione una fecha',
                choices = rev(dates),selected = tail(dates,1))
  })
    
    
  #basemap
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
      setView(lng = -71.5, lat = -33.12,zoom = 6)
  })
  
  #proxy raster plot
  observeEvent(input$fechas,{
    #select file
    n <- which(dates==input$fechas)
    #colorramp
    colores <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090',
                 '#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')
    pal <- colorNumeric(rev(colores), seq(10,25,1), na.color = "transparent")
    #raster file
    r <- sst.list[n] %>% raster()
    #add to map
    leafletProxy('map') %>% clearControls() %>% clearImages() %>% 
      addRasterImage(r,group = 'SST°C',method = 'ngb', colors = pal)
  })
  
  xy.map <- eventReactive(input$map_click,{
    click <- isolate({input$map_click})
    clat <- click$lat
    clng <- click$lng
    dts <- SpatialPoints(data.frame(clng,clat),
                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    dts
  })
  
  # click markers
  observeEvent(input$map_click,{
    #extraer celda
    n.cell <- cellFromXY(d.select,xy.map())
    #Extraer columna
    n.col <- which(dates==input$fechas)
    #valor
    px.value<- tabla[n.cell,n.col] %>% as.numeric()
    #coordinates
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng
    #ProxyMap
    leafletProxy("map")  %>% clearMarkers() %>% addMarkers(lng=click$lng, lat=click$lat, 
                                                           label= paste('| lat:',round(clat,3),
                                                                        '| lng',round(clng,3), '| SST:',round(px.value,2)),
                                                           labelOptions = labelOptions(style = list("color" = "black", 
                                                                                                    "font-size" = "14px",
                                                                                                    "font-family" = "serif",
                                                                                                    "font-weight" = "bold")))
  })
  
  r.cell <-  reactive({
  a1 <- cellFromXY(d.select,xy.map())
  c <- tabla[a1,3:ncol(tabla)] %>% as.numeric()
  return(c)
  })
  
  output$ts2 <- renderPlotly({
    
    df <- data.frame(sst= r.cell(), fechas = dates)
    
    g <- ggplot(data = df, aes(x = fechas,y = sst)) + geom_line() + 
      geom_point()
    
    ggplotly(g)
    
  })
  
  output$ts <- renderDygraph({
    ts.px <- ts(r.cell(),start = c(2003,1),end = c(2019,12),frequency = 12)
    
    #plot
    dygraph(ts.px) %>%
      dySeries("V1", label = "SST°C") %>%
      dyRangeSelector(height = 20, strokeColor = "") %>%
      dyAxis("y", label = "Temp (C)", valueRange = c(10, 25)) %>%
      dyOptions(drawPoints = TRUE, pointSize = 2,colors = 'black')
  })
 
  #############################################
  #tabset 2
  datos <- reactive({
    req(input$target_upload)
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read_csv(inFile$datapath)
    df.s <- df[,'sst'] 
    return(df.s)
  })
  
  output$columnas<- renderUI({
    req(input$target_upload)
    df1<-datos()
    data.num<-select_if(df1, is.numeric())
    selectInput(
      "campo",
      "Seleccione un campo",
      colnames(data.num), multiple = F 
    )
  })
  
  
  graf <- eventReactive(input$plot,{
    req(!is.null(datos()))
    ts.serie <- ts(datos(),start=c(1981,244),end=c(2020,117),frequency=365)
    
    if(input$desc == T){
      g <- decompose(ts.serie) %>% autoplot() 
    }
    if(input$desc == F){
    g <- autoplot(ts.serie, ylab = 'SST °C',asp = 0.2)}
    g
  })
  
  observeEvent(input$plot,{
    req(!is.null(datos()))
    output$decom <- renderPlot({
      graf()
    })
  })
  
  output$descargar <- downloadHandler(
    #makes filename
    filename = "serie.jpg",
    content = function(file) {
      ggsave(file, graf(), dpi = 150, width = 15, height = 10)
    })
  
  
  
  
}

shinyApp(ui, server)