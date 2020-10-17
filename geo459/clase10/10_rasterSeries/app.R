library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(raster)
library(dygraphs)
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
                                        dateInput("date", label = 'Seleccione fecha', value = "2014-01-01",
                                                  min = '2010-01-01',max = '2020-01-01')),
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
  #basemap
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) 
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