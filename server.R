# Define server logic required to draw a histogram

server <- function(input, output, session) {
  
  
  #questi servono per estrarre il valore dagli event e observe event
  polygonString<-shiny::reactiveVal(0)
  
  dateFine<-shiny::reactiveVal(0)
  
  dateInizio<-shiny::reactiveVal(0)
  
  img <- "img/logow.png"
  
  #piani2012<-readRDS(file="shapefile/piani4326.rds")
  
  nomeIndici<-c("NDVI","RGI","NDMI","NIR", "RGB")
  
  shinyalert::shinyalert(title = "Sto generando la mappa, attendi qualche secondo", imageUrl="img/loghi_totali.png", imageWidth=400, imageHeight=197, showConfirmButton = FALSE, type = "info",timer = 2000)
  
  tryCatch({
    if(file.exists("contatore.rds")){}
    else{createContatore()}
  }, warning = function(w) {
    warning-handler-code
  }, error = function(e) {
    error-handler-code
  }, finally = {
    updateContatore("accessi")
  })
  

  

  
  baseurl <- "http://localhost:8081/geoserver/infortrac/ows?service=WFS&version=1.0.0&"
  
  #estraggo il layer
  piani_wfs_request <-"request=GetFeature&typeName=infortrac%3Apiani4326_riparato_centroid&outputFormat=application%2Fjson"
  
  piani <- paste0(baseurl, piani_wfs_request)
  
  #leggo l'oggetto
  pianiGeo<- sf::st_read(piani)
  
  #rimuovo i punti nulli perchè ci sono errori topologici
  pianiGeo<-pianiGeo[!st_is_empty(pianiGeo),,drop=FALSE]
  
  #MAPPA di base
  output$mymap <- renderLeaflet({
    
    leaflet()%>%
      
      addLogo(img,  url = "img/logow.png", width = 145, height= 50)%>%
      
      leaflet::addTiles(group="OpenStreetMap")%>%
      
      ## F.PIROTTI
      leaflet::addTiles(urlTemplate  ="//idt2.regione.veneto.it/gwc/service/wmts?&REQUEST=GetTile&contextualWMSLegend=0&crs=EPSG:900913&dpiMode=7&format=image/jpeg&layer=rv:OrthoPhoto_2015_pyramid&styles=&tileMatrixSet=EPSG:900913&TILEMATRIX=EPSG:900913:{z}&TILEROW={y}&TILECOL={x}&url=https://idt2.regione.veneto.it/gwc/service/wmts",
                        #layers = "rv:OrthoPhoto_2015_pyramid",
                        options = tileOptions(crs="EPSG:900913", format = "image/jpeg", transparent = FALSE),
                        group="Ortofoto 2015 Regione Veneto",
                        attribution = "© Regione Veneto"
      )%>%
      
      
      
      leaflet::addLayersControl(
        position =("topleft"),
        baseGroups = c("OpenStreetMap","Ortofoto 2015 Regione Veneto"),
        overlayGroups = c("Piani di riassetto","Mappa Indice","Color Composite", "Estensione massima"),
        layersControlOptions(autoZIndex = FALSE)
      )%>%
      

      
      leaflet.extras::addDrawToolbar(
        targetGroup='draw',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polygonOptions = drawPolygonOptions(showArea = TRUE, metric = TRUE, shapeOptions = drawShapeOptions(), repeatMode = FALSE),
        circleMarkerOptions = FALSE,
        markerOptions  = FALSE,
        circleOptions = FALSE,
        singleFeature = TRUE) %>%
      
      leaflet.extras::addStyleEditor(position = "topleft", 
                                     openOnLeafletDraw = TRUE)%>%
      
      enableMeasurePath()%>%
      
      leaflet.extras::addMeasurePathToolbar()%>%
      
      leaflet::addScaleBar("bottomleft")%>%
      
      leaflet::setView( lng=11.970140, lat=46.349963, zoom = 12)  %>% 
      
      # addPolygons(data=piani2012, 
      #        group="Piani di riassetto",
      #        color = "#000000", 
      #        weight = 2, 
      #        smoothFactor = 1,
      #        fillColor = "#0000FF",
      #        opacity= 0.1,
      #        fill = TRUE,
      #        label = piani2012$KEY_PIANO,
      #        #labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 0.5 , textsize='12px'),
    #        highlightOptions = highlightOptions(color = "red", weight = 3,
    #        bringToFront = TRUE,
    #        opacity = 0.3,
    #        dashArray = NULL))
    
    
    leaflet::addCircleMarkers(
      data=pianiGeo, 
      lng = st_coordinates(pianiGeo)[,1],  
      lat = st_coordinates(pianiGeo)[,2],
      layerId=~id,
      group="Piani di riassetto",
      label=paste(pianiGeo$KEY_PIANO),
      color="#377eb8",
      fillOpacity = 0.0,
      radius=0.5
    )%>%
      
      leaflet::addPolygons(c(11.6264853552656, 11.5794594555424, 12.9930021665232, 13.0656571326719),
                           c(46.9235730577955,45.9364308725672,45.8957352511058,46.8814596607493), 
                           group="Estensione massima",fill = FALSE, options = pathOptions(interactive = F), stroke = TRUE,color = "#FF0000")%>%
      
      leaflet::addWMSTiles("https://www.cirgeo.unipd.it/GeoS/geoserver/infortrac/wms?SERVICE=WMS&",
                           layerId="piani",
                           layers = "piani4326",
                           options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = 0.6),  
                           group="Piani di riassetto")
    
    
  })#chiudo output mymap  
  
  #observer del bottone che mostra il pannello dei controlli/risultati
  observeEvent(input$showBtn1, {
    shinyjs::toggle('controlli1')})
  
  observeEvent(input$showBtn2, {
    shinyjs::toggle('risultati')})
  
  

  
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
    <a href='https://infortrac.wordpress.com/'>Sito web del progetto</a>
    <br><br>
    <a href = 'mailto: infortrac@gmail.com'>Email del progetto</a>
    <br><br>
    <a href='https://twitter.com/for_trac'>Profilo Twitter</a>
    <br><br>
    Stefano Grigolato (coordinatore scientifico)
    <a href = 'mailto: stefano.grigolato@unipd.it'>Email</a>
    <br><br>
    Marco Piragnolo (sviluppo piattaforma e telerilevamento)
    <a href = 'mailto: marco.piragnolo@unipd.it'>Email</a>
    </div>", 
    type = "info")})
  
  #OBSERVE della mappa
  observeEvent(input$mymap_draw_new_feature,{
    print("here")
    #ogni volta che disegno un poligono cancello i poligoni precedenti
    leaflet::leafletProxy('mymap') %>%
      leaflet::removeShape('draw')
    
    #evento click
    event <- input$mymap_click
    
    if (is.null(event)){return()}
    else{
      #scrivo le coordinate sul pannello
      output$Clon <- renderText({ paste(event$id, "Lon: ",event$lng)})
      output$Clat <- renderText({ paste(event$id, "Lat: ",event$lat,"Zoom lef: ",input$mymap_zoom)})
    }  
    
    
    #creo un oggetto per leggere le coordiante dei poligoni (feature)
    feature <- input$mymap_draw_new_feature
    
    listaF<- input$mymap_draw_new_feature
    
    if (is.null(feature))
      return()
    
    #ciclo ogni punto e costruisco una stringa per la query leggendo le coordinate della feature
    featureString<-c()
    for (i in 1:length(feature$geometry$coordinates[[1]][]))
    {
      #print(paste("Punto",i,"lon",feature$geometry$coordinates[[1]][[i]][[1]]))
      #print(paste("Punto",i,"lat",feature$geometry$coordinates[[1]][[i]][[2]]))
      
      featureString<-c(featureString,paste0(feature$geometry$coordinates[[1]][[i]][[1]]," ",feature$geometry$coordinates[[1]][[i]][[2]] ,","))
    }
    
    #incollo gli elementie tolgo spazi
    featureString<-paste(featureString,collapse="")
    
    #tolgo l'ultima virgola
    featureString<-gsub('.{1}$', '', paste(featureString,collapse=""))
    
    #estraggo il valore tramite dall'observer tramite la funzione definita in partenza 
    isolate(polygonString(featureString))
    
    #stampo a sul pannello a video
    output$Polygon <- renderText({ paste("Poligono: ",featureString)})
    
  })
  
  
  #Questi sono i checkbox degl indici
  output$indices<- renderPrint({ input$radio })  
  
  
  #Pannello check dello Zoom  
  output$Zoom_check <- renderText({ 
    
    if(is.null(input$mymap_zoom)){}
    else if (input$mymap_zoom <= 13.8)
    {
      "<div class=\"zoomin\">Zoom IN</div>"
    }
    else if(input$mymap_zoom > 13.8){
      "<div class=\"zoomok\">Zoom OK</div>"
    }
  })  
  
  
  # OBSERVER Calendario Date
  # observeEvent(input$daterange, {
  #   
  #   dateInizio(input$daterange[1])
  #   
  #   dateFine(input$daterange[2])
  #   
  #   # print(input$daterange[1])
  #   # print(input$daterange[2])
  #   
  # #  das<-getDate(as.Date(input$daterange[1]),as.Date(input$daterange[2]))
  #   das<-images.lut$dates
  #   shinyWidgets::updateSliderTextInput(session, 'dayselected', 
  #                                     choices=das )
  # 
  # #  shinyWidgets::updateSliderTextInput(session, 'dayselected', choices=as.Date(as.numeric(das), origin="1970-01-01"))
  # })
  
  
  
  #Questa funzione mostra la data della sliderbar nella textOutput della ui
  output$selected_var <- renderText({ 
    #il paste serve per mantenere il testo formattato come stringa altrimenti viene formattato come numero
    paste("", input$dayselected) 
  })  
  
  
  #OBSERVE CALCOLA GRAFICO indici osservando evento del pulsante calcola
  observeEvent(input$calcola,{
    
    #faccio un controllo se il poligono è disegnato
    if(polygonString()==0){shinyalert::shinyalert(title = "Devi disegnare un poligono per calcolare il grafico", type = "warning")}
    else if(as.numeric(isolate(input$radio)) == 5){
      shinyalert::shinyalert(title = "Scegli un indice, non posso fare l'analisi sull'RGB", type = "warning")
    }
    else{
      #apro il pannello risultati
      shinyjs::show("risultati")
      
      inizio<-paste(dateInizio(), "00:00:00.000+00")
      
      fine<-paste(dateFine(), "00:00:00.000+00")
      
      scaricaDati<-getIndice(polygonString(), inizio, fine, isolate(input$radio))
      
      scaricaGrafico<-getGrafico(scaricaDati,isolate(input$radio))
      
      analisi<-nomeAnalisi[as.numeric(isolate(input$radio))]
      
      output$plot1 <- shiny::renderPlot({
        scaricaGrafico
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
  
  
  
  
  observeEvent(input$aggiorna,{

    #se dopo aver cliccato l'evento zoom è dettagliato allora mostro la mappa
    if(input$mymap_zoom > 13.8){

      updateMyMap()

    }
    else if(input$mymap_zoom <= 13.8){
      shinyalert::shinyalert(title = "Devi zoomare di più per visualizzare la mappa!", type = "warning")
    }
  })
  
  
  updateMyMap<-function(){  
    if( (input$radio!=6) && (input$mymap_zoom > 13.8) ){updateMapIndice()}
    else {shinyalert::shinyalert(title = "You did it!", type = "error")}
  }
  
  observeEvent(input$opac,{
    #se dopo aver cliccato l'evento zoom è dettagliato allora mostro la mappa
    
  })
  
  #Questa funzione carica la mappa NDVI, la tematizza e aggiunge la legenda.  
  updateMapIndice<-function(){
    ## FP cast to raster terra object
    mappaIndice<-getMyMap(input,input$radio,polygonString())
   
    if(is(mappaIndice,"RasterLayer") || is(mappaIndice,"RasterBrick") || is(mappaIndice, "SpatRaster")){
      #minValue(mappaIndice):maxValue(mappaIndice)
      switch(input$radio, 
             '1'={
               x <- -1:1 
               mycolor<-colorRamp(c("#663d00", "#804d00", "#995c00", "#b36b00", "#cc7a00", "#f5c700ff", "#ff0000", "#66FF33", "#33CC33", "#009933", "#006600"))
               cr <- colorNumeric(mycolor,x,na.color ="transparent" )
               cl <- colorNumeric(mycolor, x, reverse=TRUE, na.color ="transparent"  )
               titolo<-paste(nomeIndici[1], "</br>", input$dayselected)
                
               leaflet::leafletProxy('mymap') %>%
                 leaflet::addRasterImage(mappaIndice,  layerId="colormap", group = "Mappa Indice", colors = cr, opacity = input$opac, project = F) %>%
                 leaflet::addLegend(className = "legend", group ="Mappa Indice", layerId ="indice_legenda", pal = cl, values=(x), position="topleft", title=titolo, na.label = "NA",labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
             },
             '2'={
               #minValue(mappaIndice):maxValue(mappaIndice)
               x <- 0:2 
               cr <- colorNumeric(c( "#15820b", "#00ff00","#ff0000","#b10081"),x,na.color ="transparent" )
               cl <- colorNumeric(c( "#15820b", "#00ff00","#ff0000","#b10081"),x,reverse=TRUE,na.color ="transparent" )
               titolo<-paste(nomeIndici[2], "</br>", input$dayselected)
               leaflet::leafletProxy('mymap') %>%
                 
                 leaflet::addRasterImage(mappaIndice,  layerId="colormap", group = "Mappa Indice", colors = cr, opacity = input$opac) %>%
                 
                 leaflet::addLegend(className = "legend", group ="Mappa Indice", layerId ="indice_legenda", pal = cl, values=(x), position="topleft", title=titolo, na.label = "NA",labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
             },
             '3'={
               x <- -1:1 
               cr <- colorNumeric(c("#ff00ae", "#ff0000", "#fffd00","#00c1ff", "#0067ff"   ),x,na.color ="transparent" )
               cl <- colorNumeric(c("#ff00ae", "#ff0000", "#fffd00","#00c1ff", "#0067ff"  ), x, reverse=TRUE, na.color ="transparent" )
               #"#ff0000", "#f87914", "#ffff00", "#0000ff", "#000080"
               titolo<-paste(nomeIndici[3], "</br>", input$dayselected)
               leaflet::leafletProxy('mymap') %>%
                 
                 leaflet::addRasterImage(mappaIndice,  layerId="colormap", group = "Mappa Indice", colors = cr, opacity = input$opac) %>%
                 
                 leaflet::addLegend(className = "legend", group ="Mappa Indice", layerId ="indice_legenda", pal = cl, values=(x), position="topleft", title=titolo, na.label = "NA",labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
             },
             # '4'={
             #   
             #   min<-minValue(mappaIndice)
             #   
             #   max<-maxValue(mappaIndice)
             #   
             #   x <- c(min,max) #minValue(mappaIndice):maxValue(mappaIndice)
             #   
             #   
             #   cr <- colorNumeric("RdBu", x, reverse=FALSE, na.color ="transparent" )
             #   cl <- colorNumeric("RdBu", x, reverse=TRUE, na.color ="transparent" )
             #   titolo<-paste(nomeIndici[4], "</br>", input$dayselected)
             #   
             #   
             #   leaflet::leafletProxy('mymap') %>%
             #     
             #     leaflet::addRasterImage(mappaIndice,  layerId="colormap", group = "Mappa Indice", colors = cr, opacity = input$opac) %>%
             #     
             #     leaflet::addLegend(className = "legend",  group ="Mappa Indice", layerId ="indice_legenda", pal = cl, values=(x), position="topleft", title=titolo, na.label = "NA",labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
             # },
             '4'={
               
               titoloRGB<-paste("VNIR del", input$dayselected)
               
               leaflet::leafletProxy('mymap') %>%
                 leafem::addRasterRGB(mappaIndice,1,2,3, layerId="falseColor", 
                                      group = "Color Composite", opacity = input$opac)%>%
                 leaflet::addLegend(className = "legendRGB", group ="Color Composite", layerId ="indice_legendaRGB", colors = NULL, position="topleft", title=titoloRGB, na.label = "NA")
             },
             '5'={
               
               titoloRGB<-paste("RGB del", input$dayselected)
               
               leaflet::leafletProxy('mymap') %>%
                 leafem::addRasterRGB(mappaIndice,3,2,1, layerId="trueColor", 
                                      group = "Color Composite", opacity = input$opac)%>%
                 leaflet::addLegend(className = "legendRGB", group ="Color Composite", layerId ="indice_legendaRGB", colors = NULL, position="topleft", title=titoloRGB, na.label = "NA")
             },
             stop("Enter something that switches me!")
      )
    } 
    
    updateContatore("mappa")
  }
  
  #Output testuale del grafico
  output$info <- renderText({
    paste0("x=", as.Date(as.numeric(input$plot_click$x), origin="1970-01-01"), "\ny=", round(as.numeric( input$plot_click$y,5),2))
  })
  
  
}