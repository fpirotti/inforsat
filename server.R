# Define server logic required to draw a histogram 
server <- function(input, output, session) { 
  #source("functions_auth.R", local=T)
  shinyjs::hide("scaricaIndice")
  shinyjs::hide("scaricaPoligoni")
  shinyjs::addCssClass(id="scaricaIndice", "sideButtons")
  shinyjs::addCssClass(id="scaricaPoligoni", "sideButtons")
  #questi servono per estrarre il valore dagli event e observe event
  
  reacts<- reactiveValues( table.index=NULL,   IS.LOGGED=F, IS.UNIPD=F,
                           activeLUT.Table = images.lut %>% filter(tile==images.lut$tile[[1]] ) )
  
  observeEvent(input$tile, {
    
    reacts$activeLUT.Table <- images.lut %>% filter(tile==input$tile )
    
    
    #  
    # circos.clear()
    # df = data.frame(sectors = sample(letters[1:8], 12, replace = TRUE),
    #                 x = rnorm(12), y = runif(12))
    # 
    # circos.initialize(month.name, xlim=c(1,31))
    # 
    # circos.track(month.name, ylim = c(0, 1))
    # 
    # for(sector.index in all.sector.index) {
    #   circos.points(x1, y1, sector.index)
    #   circos.lines(x2, y2, sector.index)
    # }
 
    tt <- tiles.geom %>% filter(name == input$tile)
    ttb <- st_bbox(tt)
    leafletProxy("mymap")  %>%
      leaflet::fitBounds( ttb[["xmin"]]  ,  ttb[["ymin"]], ttb[["xmax"]], ttb[["ymax"]])  
 
    updateSelectInput(session = session, "dayselected", choices = c("", as.character(reacts$activeLUT.Table$date)) )
  }, ignoreInit = T)
  
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
  myPolygons <- NULL
  myPolygons.tot.area <- NULL
  ### create polygons -----
   observeEvent(input$mymap_draw_all_features,  {
    myPolygons <<-  geojson_sf(jsonify::to_json(input$mymap_draw_all_features, unbox = T)) 
    myPolygons.tot.area <<- as.numeric(sum(st_area(myPolygons))/1000000) 
    print(myPolygons)
  })
 
  
  outIndexFile<- file.path(pathsTemp, sprintf("%s.", session$token) )
 
  

  #MAPPA di base
  output$mymap <- renderLeaflet({
    leaflet.object  
  })#chiudo output mymap  
  
  ### disegna GRAFICO PLOTLY ------
  output$graph1 <- renderPlotly({ 
    req(reacts$table.index)
    tb<-reacts$table.index
    #tb$Date<-as.Date(tb$Date,  origin = "1970-01-01")
    tb$FID<-as.factor(tb$FID)
    tb<-na.omit(tb) 
    if(nrow(tb)<1){
      shinyalert::shinyalert(title = "Warning",
                             text = "No overlapping data with areas... did you choose the correct tile?",
                             type = "warning")
      return()
    }
    p <- ggplot( tb, aes_string(x="Date", y="q50", color="FID" )  ) + ylab("Index statistics") + 
      ggtitle( sprintf("TILE %s - %d images, %d areas, total size=%.2f km2", 
                       input$tile, isolate(nrow(reacts$activeLUT.Table)), nrow(myPolygons), myPolygons.tot.area ) )
    p <- p + geom_crossbar(aes_string(ymin = "q25", ymax = "q75"), position_dodge(width = 0.9) )
    p <- p + geom_point(aes(x=Date, y=Mean), position = position_dodge(width = 0.9) )
    p <- p + geom_errorbar(aes_string(ymin = "q10", ymax = "q90"), position = position_dodge(width = 0.9) ) + theme_bw()
 
    ggplotly(p)  
  })
  #observer del bottone che mostra il pannello dei controlli/risultati
  observeEvent(reacts$table.index, {
    print("SDFSDFSDFDSFDSDF")
    })
 
  
  observeEvent(input$showBtn2, {
    shinyjs::toggle('risultati')
  })
  
  observeEvent(input$leafletRendered, { 
     
  
    updateBox( "myBox", action ="toggle")   
 
    shinyjs::runjs("  
                     $('.leaflet-control-layers').appendTo( $('#legendPlaceholder') );
                     $('.leaflet-control-layers-overlays').children().after( function(){return '<input type=\"range\" min=\"0\" max=\"100\" value=\"100\"  oninput=\"changeLayerOpacity($(this))\" ><hr class=legendHR >' } ); 
                     $('.leaflet-control-layers-overlays').children('label').children('div').css(\"float\", \"left\");
                     $('.leaflet-control-layers-overlays').children('label').after('&nbsp;<span style=\"float:right;\" title=\"Split view line at mouse\" class=\"fa fa-toggle-on fa-2x fa-rotate-180 toggler\" ></span>')
                     $('.toggler').on('click', function (e) {
                        $(this).toggleClass('fa-rotate-180 off');
                            if($(this).hasClass('fa-rotate-180'))  {
                              spyGlassDeactivate($(this));
                              myMap.off('mousemove', spyGlass); 
                            } else {
                              spyGlassActivate($(this));
                              myMap.on('mousemove', spyGlass);  
                            }
                    });"
                   )
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
   
  ### SCARICA POLIGONI -----
  output$scaricaPoligoni <- downloadHandler(
    filename = function() {
       sprintf("%s.gpkg", session$token)
    },
    content = function(file) {
      
      dt.final3 <- isolate( reacts$table.index )
      
      dt.final4 <-  dt.final3 %>% 
        pivot_wider(names_from = Date, values_from = setdiff(names(dt.final3), c("FID","Date")) )
      
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
    if(is.null(myPolygons.tot.area) || length(myPolygons)==0){
      shinyalert::shinyalert(title="Warning",
                             text = "Please draw one or more rectangles or polygons to define areas in which to calculate temporal statistics ", 
                             type = "warning")
      return()
      }

    else{
      #apro il pannello risultati
      shinyjs::show("risultati")
       
      
      
      if( as.numeric(myPolygons.tot.area) >4){
        shinyalert::shinyalert("Sorry", paste0("Your area is more than 4 km2, (you have", round(myPolygons.tot.area,2) ,"km2 in total, 
                                               please try a smaller area or contact francesco.pirotti@unipd.it, thankyou."))
        return(NULL)
      }
      withProgress(message = 'Estracting data from polygons', value = 0, {    
        nimages <- nrow(reacts$activeLUT.Table)
        data.c<- list()
        for(i in 1:nimages){
          setProgress(i/(nimages+3), detail = sprintf("Image %s of %s...", i, nimages) )  
          tt <-terra::rast( reacts$activeLUT.Table$VRT[[i]])
          bands <- names(reacts$activeLUT.Table$bands[[i]])
          bands2use <- c("CLD","SNW",  bands2use<- unique( stringr::str_extract_all(   session$input$indici , "B[018][0-9A]")[[1]] ) )
          ww<-match( bands2use, bands  )
          tt2 <- tt[[ww]]
          rr <-  terra::extract(tt2, terra::vect( st_transform(myPolygons, 32632) ))
          names(rr) <- c("FID", bands2use)
          data.c[[as.character(reacts$activeLUT.Table$date[[i]])]]<-rr
        } 
      }) 
      dt.final <- data.table::rbindlist(data.c, idcol = "Date")
      dt.final$Date <- as.Date( dt.final$Date )
      
      mYexpression<-gsub("(B[018][0-9A])",   "  dt.final[['\\1']]  " ,    
                         session$input$indici) 
 
      dt.final$Index <- eval(parse(text=  mYexpression) ) 

      dt.final2 <- dt.final %>% 
        dplyr::select(FID, Date, Index, CLD, SNW) %>% 
        dplyr::group_by(FID, Date) %>% 
        dplyr::summarize(   
          x = statsFunction(Index), 
          q = c("Mean", "SD", "Min", "q10", "q25", "q50", "q75", "q90", "Max") 
          ) 
      
      tb <-  dt.final2 %>% 
        pivot_wider(names_from = q, values_from = x)
      
      reacts$table.index <- tb 
      #<-getIndice(session, terra::vect(myPolygons) )
       
      if(!is.null(tb) && nrow(tb)==nrow(myPolygons)){
        
        shinyalert::shinyalert(title = "Nessun risultato", 
                               type = "warning")
        return()
        
      }
      
      shinyjs::show("scaricaIndice")
       
    }#chiudo else
  })
  
  # ### SPYGLASS -----
  # observeEvent(input$spyGlass,{
  # 
  # }, ignoreInit = T)
  # 
  
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