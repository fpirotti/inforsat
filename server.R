# Define server logic required to draw a histogram

server <- function(input, output, session) {
  
  source("functions_auth.R", local=T)
  shinyjs::hide("scaricaIndice")
  shinyjs::hide("scaricaPoligoni")
  shinyjs::addCssClass(id="scaricaIndice", "sideButtons")
  shinyjs::addCssClass(id="scaricaPoligoni", "sideButtons")
  #questi servono per estrarre il valore dagli event e observe event
  
  reacts<- reactiveValues( table.index=NULL,   IS.LOGGED=F, IS.UNIPD=F)
 
  output$bandHistogram <- renderPlotly({
     
      plot_ly(type = "scatter", mode = "markers") %>% layout(
        hoverlabel= list(align="left"),
        margin = list(l = 5, r = 2, b = 20, t = 30),
        showlegend = T, legend = list(orientation = 'h'),
        
        xaxis = list(
          title = "Grey levels",   range = c(0, 18000), 
          rangeslider = list(type = "numeric")),
        yaxis = list(title = "Frequenza", range = c(0, 1.1) ),
        title = sprintf("Istogramma f(x)")
      ) %>%  add_markers(
        x = c(200,4000),
        y = 0,
        name = "Info Bande",  
        marker = list(
          symbol = "square",
          color = "black",
          size = 9
        )
      ) 

  })
  
  session$userData$indexfile <- file.path(pathsTemp, sprintf("%s.tif", session$token))
  session$userData$mapfile   <- file.path(pathsTemp, sprintf("%s.map", session$token))
  
  myPolygons<-NULL
  outIndexFile<- file.path(pathsTemp, sprintf("%s.", session$token) )
 
  

  #MAPPA di base
  output$mymap <- renderLeaflet({
    leaflet.object  
  })#chiudo output mymap  
  
  
  output$graph1 <- renderPlotly({ 
    req(reacts$table.index)
    tb<-reacts$table.index
    tb$Date<-as.Date(tb$Date,  origin = "1970-01-01")
    tb$FID<-as.factor(tb$FID)
    p <- ggplot( tb, aes_string(x="Date", y="q50", color="FID" )  )
    p <- p + geom_crossbar(aes_string(ymin = "q25", ymax = "q75") )
    p <- p + geom_point(aes(x=Date, y=mean) )
    p <- p + geom_errorbar(aes_string(ymin = "q10", ymax = "q90") ) + theme_bw()
    ggplotly(p)
  })
  #observer del bottone che mostra il pannello dei controlli/risultati
  observeEvent(reacts$table.index, {
    print("SDFSDFSDFDSFDSDF")
    })
 
  
  observeEvent(input$showBtn2, {
    shinyjs::toggle('risultati')})
  
  observeEvent(input$leafletRendered, { 
    
    updateBox( "myBox", action ="toggle")   
    shinyjs::runjs(" 
                     $('.leaflet-control-layers').appendTo( $('#legendPlaceholder') );
                     $('.leaflet-control-layers-overlays').children().after('<input type=\"range\" min=\"0\" max=\"100\" value=\"100\"   id=\"myRange\"><hr class=legendHR >'); ")
  })
  ### Cambio impostazioni layers ----
  observeEvent({
    input$mskCld
    input$mskSnw
    input$composite
    input$dayselected
    input$freezeScale
    input$indici }, {
       
      session$userData$wms.url<- compositeCreate(session)
      
      #createIndexFile(session)
      updateMap(session)
      createIndexFile(session)
    }, ignoreInit = T )
  
  

  
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
    Francesco Pirotti (ottimizzazione algoritmi telerilevamento)
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
  
  ### SCARICA POLIGONI -----
  output$scaricaPoligoni <- downloadHandler(
    filename = function() {
       sprintf("%s.gpkg", session$token)
    },
    content = function(file) {
      st_write(myPolygons,  file, append=F  )
    }
  )
  
 

  ### SCARICA RASTER -----
  output$scaricaIndice <- downloadHandler(
    filename = function() { 
      sprintf("%s.tif", session$token)
    },
    content = function(file) { 
      file.copy(session$userData$indexfile, file, overwrite = T)
    }
  )

  
   
  #OBSERVE CALCOLA GRAFICO indici osservando evento del pulsante calcola
  ### zonal statistics ------
  observeEvent(input$calcola,{
    
    #faccio un controllo se il poligono è disegnato
    if(length(myPolygons)==0){
      shinyalert::shinyalert(title = "Devi disegnare un poligono per calcolare il grafico", 
                             type = "warning")
      return()
      }

    else{
      #apro il pannello risultati
      shinyjs::show("risultati")
      

      reacts$table.index<-getIndice(session, myPolygons)
      tb<-isolate(  reacts$table.index )
      if(!is.null(tb) && nrow(tb)==nrow(myPolygons)){
        
        shinyalert::shinyalert(title = "Nessun risultato", 
                               type = "warning")
        return()
        
      }
      
      shinyjs::show("scaricaIndice")
       
    }#chiudo else
  })
  
  
  
  ### AGGIORNA -----
  observeEvent(input$aggiorna,{
    createIndexFile(session)
  })
  
    
  
  ### FINISCE SESSIONE  -----
  session$onSessionEnded(function() {
    mapfiles<-list.files(pathsTemp, sprintf("%s.*", session$token),  full.names = T)
    print(mapfiles)
    file.remove(mapfiles)
  })
  
  
}