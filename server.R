# Define server logic required to draw a histogram

server <- function(input, output, session) {
  
  
  #questi servono per estrarre il valore dagli event e observe event
  indexfile<-NULL
  myPolygons<-NULL
  outIndexFile<- file.path(pathsTemp, sprintf("%s.", session$token) )
  img <- "img/logow.png"
  ## creo file di configurazione mapserver e lo clono

 # readr::read_file(wms.url)
  nomeIndici<-c("NDVI","RGI","NDMI","NIR", "RGB")
  

  #MAPPA di base
  output$mymap <- renderLeaflet({
    leaflet.object  
  })#chiudo output mymap  
  
  #observer del bottone che mostra il pannello dei controlli/risultati
  observeEvent(input$showBtn1, {
    shinyjs::toggle('controlli1')})
  
  observeEvent(input$showBtn2, {
    shinyjs::toggle('risultati')})
  
  ### Cambio impostazioni layers ----
  observeEvent({
    input$mskCld
    input$mskSnow
    input$composite
    input$dayselected }, {
       
      wms.url<- compositeCreate(session)
      
        
      leaflet::leafletProxy('mymap') %>%
        leaflet::clearGroup(group='Color Composite')%>% 
        leaflet::clearGroup(group='INDICE')%>%
        leaflet::clearGroup(group='Scene Classification')%>%
      
        addWMSTiles(
          wms.url,
          group="Color Composite",
          layers = "100", 
          options = WMSTileOptions( format = "image/png", transparent = T  ),
          attribution = "Pirotti")  %>%
         
        addWMSTiles(
          wms.url,
          group="INDICE",
          layers = "300", 
          options = WMSTileOptions( format = "image/png", transparent = T  ),
          attribution = "Pirotti")  %>%
        
        addWMSTiles(
          wms.url,
          group="Scene Classification",
          layers = "400", 
          options = WMSTileOptions(format = "image/png", transparent = T  ),
          attribution = "Pirotti")   
    })
  
  

  
  observeEvent(input$info ,{
    shinyalert::shinyalert(
        html=TRUE,
        text=paste("<div style='text-align: justify;'>
    Il progetto InForTrac è nato dalla collaborazione tra il Dipartimento Territorio e Sistemi Agro-Forestali (TESAF), UNIPD e l'Unione Montana Agordina nell’ambito dell’iniziativa UNI-IMPRESA 2017. L’obbiettivo è di potenziare il trasferimento tecnologico nell’ambito della pianificazione e della gestione forestale, fornendo strumenti innovativi per la valorizzazione della filiera foresta-legno in Agordino",
                   "<br>Accessi totali: ",getContatore("accessi"),
                   "<br>Mappe generate: ",getContatore("mappa"),
                   "<br>Dati scaricati: ",getContatore("downdati"),
                   "<br>Grafici scaricati: ",getContatore("downgrafico"),
                   "<br>Il portale non usa cookies per la profilazione dell'utente. Versione 2.0 </div>"),
        type = "info" 
      )
    })
  
  observeEvent(input$guida ,{
    shinyalert::shinyalert(
      html=TRUE,
      text="<div style='text-align: justify;'>
    Il portale è stato realizzato con l’obiettivo, in riferimento alle aree colpite da Vaia e non solo, di monitorare le fasi di esbosco del materiale, le fasi di ripresa del bosco, e gli eventuali danni secondari dovuti ad attacchi parassitari (es. bostrico).
    Il tool analizza una serie temporale di immagini ottenute dai satelliti della missione Copernicus Sentinel-2 dell’Agenzia Spaziale Europea (ESA) per la zona dell’Agordino. 
    <br><br>L'analisi è condotta filtrando le immagini, rimuovendo le nuvole e calcolando gli indici di vegetazione. 
    <br><br>Per maggiori informazioni <a href='https://infortrac.files.wordpress.com/2020/04/ok_manuale-uso-v02.pdf'>consulta la guida</a> 
    </div>", 
      type = "info")
  })
  
  observeEvent(input$contatti ,{shinyalert::shinyalert(
    html=TRUE,
    text="<div style='text-align: justify;'>
    Marco Piragnolo (sviluppo piattaforma e telerilevamento)
    <a href = 'mailto: marco.piragnolo@unipd.it'>Email</a>
    Francesco Pirotti (ottimizzazione telerilevamento)
    <a href = 'mailto: francesco.pirotti@unipd.it'>Email</a>
    </div>", 
    type = "info")})
  
  #OBSERVE della mappa
  observeEvent(input$mymap_draw_new_feature,{ 
 
    #evento click
    gc<-input$mymap_draw_new_feature
    event <- input$mymap_click
    ## è facilmente castabile da geojson a sf e aggiungo a lista myPolygons
    if(is.null(myPolygons)){
      myPolygons <<- geojson_sf(jsonify::to_json(gc, unbox = T))
    } else {
      myPolygons <<- rbind(myPolygons, geojson_sf(jsonify::to_json(gc, unbox = T)))
    }
   
  })
  
  output$scaricaPoligoni <- downloadHandler(
    filename = function() {
      file.path(pathsTemp, sprintf("%s.gpkg", session$token))
    },
    content = function(file) {
      if(length(myPolygons)==0){
        shinyalert::shinyalert(title = "Devi disegnare almeno un poligono", type = "warning")
        return()
      }
      st_write(myPolygons,  file, append=F  )
    }
  )
  

 
  
   
  #OBSERVE CALCOLA GRAFICO indici osservando evento del pulsante calcola
  ### zonal statistics ------
  observeEvent(input$calcola,{
    
    #faccio un controllo se il poligono è disegnato
    if(length(myPolygons)==0){shinyalert::shinyalert(title = "Devi disegnare un poligono per calcolare il grafico", type = "warning")}

    else{
      #apro il pannello risultati
      shinyjs::show("risultati")
 
      scaricaDati<-getIndice(session, myPolygons)
      
      
 
      
      output$plot1 <- shiny::renderPlot({
 
      })
      
      updateContatore("grafico")
       
      output$downloadData <- downloadHandler(
        updateContatore("downdati"),
        
        filename = function() {
          #paste(nomeAnalisi[as.numeric(nomeIndice)], ".csv", sep = "")
          paste(analisi, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(scaricaDati, file, row.names = FALSE)
        }
      )
      
      output$downloadPlot <- downloadHandler(
        updateContatore("downgrafico"),
        
        filename = function() { 
          paste("plot", '.png', sep='') 
        },
        content = function(file) {
          device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
          
          ggplot2::ggsave(file, scaricaGrafico, device = device)
        }
      )
    }#chiudo else
  })
  
  
  
  ### AGGIORNA -----
  observeEvent(input$aggiorna,{
    createIndexFile(session)
    mYexpression<-gsub("(B[018][0-9A])",   "  terra.vrt.cropped.masked[['\\1']]  " , session$input$indici) 
  })
  
   
  #Output testuale del grafico
  # output$info <- renderText({
  #   paste0("x=", as.Date(as.numeric(input$plot_click$x), origin="1970-01-01"), "\ny=", round(as.numeric( input$plot_click$y,5),2))
  # })
  
  
  ### FINISCE SESSIONE  -----
  session$onSessionEnded(function() {
    mapfiles<-list.files(pathsTemp, sprintf("%s*", session$token),  full.names = T)
    file.remove(mapfiles)
  })
  
  
}