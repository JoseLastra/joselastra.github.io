library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(raster)
library(plotly)
library(shinybusy)
library(shinythemes)

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
                          plotlyOutput("ts",width = '85%',height = 300),
                          absolutePanel(top = 85,left = 80,right = 'auto',width = 200,
                                        height = 200,bottom = 'auto',fixed = T,style="z-index:500",
                                        HTML('<img height="90"  src="logo_labgrs.png" asp="1" 
                                             class="img-responsive" align="left">'))
                 ),
                 #Description
                 tabPanel('Tab secundario',
                          sidebarLayout(sidebarPanel(
                            fileInput('target_upload', 'Suba su tabla',
                                      accept = c('.xls','.xlsx')),
                            actionButton('tabla','Mostrar tabla'),br(),
                            uiOutput("columnas"),br(),
                            actionButton('plot',label = 'Plot'),
                            downloadButton('descargar','Descargar plot'),
                            fluid=T,width = 2),
                            mainPanel(dataTableOutput('tabla_display'),
                                      plotOutput('histo'))
                          ))
)

server <- function(input, output, session) {
  #raster data
  sst.list <- list.files(path = 'data/monthly_sst/',pattern = glob2rx('*sst*.tif'),full.names = T)
  r.extent <- sst.list[1] %>% raster() %>% st_bbox() %>% as.numeric()
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
      fitBounds(lng1 = r.extent[1],lat1 = r.extent[2] ,lng2 = r.extent[3],lat2 = r.extent[4])
  })
  
  #proxy raster plot
  observeEvent(input$fechas,{
    n <- which(dates==input$fechas)
    r <- sst.list[n] %>% raster()
    leafletProxy('map') %>% clearControls() %>% clearImages() %>% 
      addRasterImage(r,group = 'SST°C',method = 'ngb')
  })
  
  xy.map<-reactive({
    req(input$map_click)
    click <- isolate({input$map_click})
    clat <- click$lat
    clng <- click$lng
    dts <- SpatialPoints(data.frame(clng,clat),
                         proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")) %>% 
      spTransform(CRSobj = "+proj=longlat +datum=WGS84 +no_defs")
    dts
  })
  
  r.cell<-reactive({
  a1 <- cellFromXY(d.select,xy.map())
  c <- pixels.df[a1,3:ncol(tabla)]
    return(c)
  })
  
  
  # click markers
  observeEvent(input$map_click,{
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng
    leafletProxy("map")  %>% clearMarkers() %>% addMarkers(lng=click$lng, lat=click$lat, 
                                                           label= paste('lat:',clat,'lng',clng))
    
  })
  #############################################
  #tabset 2
  datos <- reactive({
    req(input$target_upload)
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read_excel(inFile$datapath)
    
    return(df)
  })
  
  output$columnas<- renderUI({
    req(input$target_upload)
    df1<-datos()
    data.num<-select_if(df1, is.character)
    selectInput(
      "campo",
      "Seleccione un campo",
      colnames(data.num), multiple = F 
    )
  })
  
  observeEvent(input$tabla,{
    req(!is.null(datos()))
    output$tabla_display <- renderDataTable({
      datos() %>% 
        datatable(rownames = FALSE, filter = 'top',
                  extensions = 'FixedColumns',
                  options = list(pageLength = 5,
                                 scrollX = TRUE,
                                 fixedColumns = TRUE))
    })
  })
  
  graf <- eventReactive(input$plot,{
    req(!is.null(datos()))
    dataPlot(tabla = datos(), campo = input$campo)
  })
  
  observeEvent(input$plot,{
    req(!is.null(datos()))
    output$histo <- renderPlot({
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