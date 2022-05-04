# Define server logic required to draw a histogram 
server <- function(input, output, session) { 
  #source("functions_auth.R", local=T)
  #shinyjs::hide("scaricaPoligoni")
  shinyjs::addCssClass(id="scaricaIndice", "sideButtons")
  shinyjs::disable(selector = "#scaricaIndice")
  shinyjs::disable(selector = "#scaricaPoligoni")
  shinyjs::disable(selector = "#scaricaTabellaValori")
  shinyjs::addCssClass(id="scaricaPoligoni", "sideButtons")
  notificationID <- NULL
  output$graph1 <- renderPlotly( plotly_empty() )
  
  #questi servono per estrarre il valore dagli event e observe event
  
  reacts<- reactiveValues( table.index=NULL,   IS.LOGGED=F, IS.UNIPD=F,
                           activeLUT.Table = images.lut %>% filter(tile==images.lut$tile[[1]] ) )
  
  ### cambio TILE -----
  observeEvent(input$tile, {
    
    reacts$activeLUT.Table <- images.lut %>% filter(tile==input$tile )
     
 
    tt <- tiles.geom %>% filter(name == input$tile)
    ttb <- st_bbox(tt)
    
    leafletProxy("mymap")  %>%
      leaflet::fitBounds( ttb[["xmin"]]  ,  ttb[["ymin"]], ttb[["xmax"]], ttb[["ymax"]])  
 
    updatePickerInput(session=session, "datesInPlot", choices = as.character(as.Date(reacts$activeLUT.Table$date)), 
                      selected=as.character(as.Date(reacts$activeLUT.Table$date))   )
    
    updateSelectInput(session = session, "dayselected", choices = c("", as.character(reacts$activeLUT.Table$date)) )
    
  }, ignoreInit = T)
  
 
  
  output$bandHistogram <- renderPlotly({
 
    plot_ly(type = "scatter", mode = "markers", source="bandHistogram") %>% layout(
      hoverlabel = list(align = "left"),
      margin = list(
        l = 5,
        r = 2,
        b = 20,
        t = 30
      ),
      showlegend = T,
      legend = list(orientation = 'h'),
      
      xaxis = list(
        title = "Grey levels",
        range = c(0, 18000),
        rangeslider = list(type = "numeric")
      ),
      yaxis = list(title = "Frequency", range = c(0, 1.1)),
      title = sprintf("Frequency histogram f(x)")
    ) %>%  add_markers(
      x = c(200, 4000),
      y = 0,
      name = "Band Info",
      marker = list(
        symbol = "square",
        color = "black",
        size = 9
      )
    )# %>% 
    # add_trace(dataDf(), x = ~time, y = ~weight, 
    #           line = list(shape = "spline"),
    #           type = 'scatter', mode = 'lines',name = "weight") 
    
  })
  
  session$userData$indexfile <- file.path(pathsTemp, sprintf("%s.tif", session$token))
  session$userData$mapfile   <- file.path(pathsTemp, sprintf("%s.map", session$token))
  myPolygons <- NULL
  myPolygons.tot.area <- NULL
  
  ### create polygons -----
   observeEvent({
     input$mymap_draw_all_features
     
     input$mymap_draw_stop },  {
       
       myPolygons <<-  geojson_sf(jsonify::to_json(input$mymap_draw_all_features, unbox = T)) 
       myPolygons.tot.area <<- as.numeric(sum(st_area(myPolygons))/1000000) 

  })
 
  
  outIndexFile<- file.path(pathsTemp, sprintf("%s.", session$token) )
 
  

  #MAPPA di base
  output$mymap <- renderLeaflet({
    leaflet.object 

  })#chiudo output mymap  
  
  ### disegna GRAFICO PLOTLY ------
  observeEvent({ 
    reacts$table.index
    input$redrawAnalysisPlot
    }, { 
       
    print("triggering go!!!!")
    req(reacts$table.index)
    
      
    req(nrow(reacts$table.index)>0)
    tb<-reacts$table.index 
    tb$FID<-as.factor(tb$FID)
    
    dates <-  (unique(as.character(tb$Date) ))
    
 
    tb <- tb %>%
      dplyr::select(FID, Date, Index, CLD, SNW) %>%
      filter(CLD <= input$cloudsInPlot, SNW <= input$snowInPlot, as.character(Date) %in% input$datesInPlot )  
 
 
    tb$Date <- as.Date(tb$Date)
    if(input$xPlotAxis)  tb$Date <- as.character(tb$Date)
  
    tb.sum <- tb %>% group_by(Date, FID) %>% summarise(n=n(), avgCloud=round(mean(CLD),0),  
                                                  avgSNOW=round(mean(SNW),0),  
                                                  avgIndex=round(mean(Index),3), 
                                                  sdIndex=round(mean(Index),3), 
                                                  mdIndex=round(median(Index),3), 
                                                  q25Index=round(quantile(Index, c(0.25)),3), 
                                                  q75Index=round(median(Index, c(0.75)) ),3)
    
    if(input$xPlotAxis)  tb.sum$Date <- as.character(tb.sum$Date)

    if(input$plotType=="Line (mean)"){
      fig <- plot_ly(as.data.frame(tb.sum), x = ~Date, y = ~avgIndex, color = ~FID,   type = 'scatter', 
                     mode = 'lines+markers' ) 
    } else if(input$plotType=="Line (mean)+SD"){
      fig <- plot_ly(as.data.frame(tb.sum), x = ~Date, 
                     position = "dodge", y = ~avgIndex, 
                     color = ~FID, type = 'scatter', 
                     mode = 'lines+markers' , 
                     error_y = ~list(array = sdIndex,
                                      color = '#00000055') ) 
    } else {
      fig <- plot_ly(tb, x = ~Date, y = ~Index, color = ~FID, type = "box", 
                     boxpoints = FALSE,  
                     boxmean="sd") %>% 
        layout(boxmode = "group")
    }


    

    
    output$graph1 <- renderPlotly(fig)
 
      
  })
  
  #observer del bottone che mostra il pannello dei controlli/risultati
  # observeEvent(reacts$table.index, {
  #   print("SDFSDFSDFDSFDSDF")
  # })
 
  observeEvent(input$showNotificationPanel, { 
    if(!is.null(notificationID)) shinyjs::toggle( id="shiny-notification-panel" ) else showNotification( "No Logs at this time." )
  })
  
  observeEvent(input$showAnalysisPanel, { 
    updateBox( "myBoxAnalytics", action ="restore")  
    if (input$myBoxAnalytics$collapsed)  updateBox( "myBoxAnalytics", action ="toggle")   
  })


  observeEvent(input$leafletRendered, { 
     
  
    updateBox( "myBoxAnalytics", action ="remove")   
 
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
    input$resampling
    input$indici_formula }, {
      
      req(input$dayselected)
      session$userData$wms.url<- compositeCreate(session)
      
      #createIndexFile(session)
      updateMap(session)
      createIndexFile(session)
      shinyjs::enable("scaricaIndice")
    }, ignoreInit = T )
  
  ### CAMBIO INDICE ----
  observeEvent(input$indici, {
      
    updateTextInput(inputId = "indici_formula", value = input$indici )
       
    } )
  
  
  
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
      browser()
      st_write(myPolygons,  file, append=F  )
    }
  )
  
  
  ### SCARICA tabella -----
  output$scaricaTabellaValori <- downloadHandler(
    filename = function() { 
      sprintf("%s.xlsx", session$token)
    },
    content = function(file) { 
      if(is.null(reacts$table.index) || nrow(reacts$table.index)==0 ){
        shinyalert("Warning", "No table available... did you draw the areas and calculate indices?", type="warning")
        return()
      }
      writexl::write_xlsx(  reacts$table.index, file )
    }
  )
  ### SCARICA PNG -----
  observeEvent(input$scaricaImmagini, {
    shinyjs::runjs( sprintf("getPNGs('%s');", session$token) )
    print("===========")
  })
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
      
      shinyjs::disable(selector = "#scaricaPoligoni")
      return()
      }

    #apro il pannello risultati
    #shinyjs::show("risultati")
     

    if( as.numeric(myPolygons.tot.area) >4){
      shinyalert::shinyalert("Sorry", 
                             paste0("Your area is more than 4 km2, (you have ", 
             format(round(myPolygons.tot.area,2),  decimal.mark=".",   big.mark="'",small.mark="'"),"km2 in total, 
                                             please try a smaller area or contact francesco.pirotti@unipd.it, thankyou."))
      
      shinyjs::disable(selector = "#scaricaPoligoni")
      return(NULL)
    }
    
    if( !isTruthy(input$tile)){
      shinyalert::shinyalert(title = "Warning",
                             text = "Choose a tile over which to compute statistics: not tile has been chosen.",
                             type = "warning")
      return()
    }
    
    start <- Sys.time()
    withProgress(message = 'Estracting data from polygons', value = 0, {    
      nimages <- nrow(reacts$activeLUT.Table)
      bands2use <- c("CLD","SNW",  bands2use<- unique( stringr::str_extract_all(   session$input$indici , "B[018][0-9A]")[[1]] ) )
      data.c<- list()
      activeLUT.Table <- isolate(reacts$activeLUT.Table)
      
      if(input$parallel){
        setProgress(0.5, 
                    message = sprintf("Parallel computation with %d cores... no progress will be shown in this case.", 
                                      isolate(input$nCores) ), 
                    detail = sprintf("N. of Images: %s...",  nimages) )  
        cl <- parallel::makeForkCluster( isolate(input$nCores) )
        doParallel::registerDoParallel(cl)
        myPolygons.tmp <- myPolygons
        
        data.c <- foreach(i = 1:nimages) %dopar% {
          tt <-terra::rast( activeLUT.Table$VRT[[i]])
          bands <- names(activeLUT.Table$bands[[i]])
          ww<-match( bands2use, bands  )
          if(is.na(sum(ww))){
            warning("\nImage ", activeLUT.Table$VRT[[i]], " does not have the necessary bands to extract... please check. Skipping this image")
            return(NULL)
          }
          tt2 <- tt[[ww]]
          rr <-  terra::extract(tt2, terra::vect( st_transform(myPolygons.tmp, 32632) ))
          names(rr) <- c("FID", bands2use)
          rr
        }
        
        parallel::stopCluster(cl) 
        names(data.c)<- as.character(activeLUT.Table$date)
        
      } else {
        
        for(i in 1:nimages){
          setProgress(i/(nimages+3), detail = sprintf("Image %s of %s...", i, nimages) )  
          tt <-terra::rast( activeLUT.Table$VRT[[i]])
          bands <- names(activeLUT.Table$bands[[i]])
          ww<-match( bands2use, bands  )

          if(is.na(sum(ww))){
            warning("\nImage ", activeLUT.Table$VRT[[i]], " does not have the necessary bands to extract... please check. Skipping this image")
            return(NULL)
          }
          tt2 <- tt[[ww]]
          rr <-  terra::extract(tt2, terra::vect( st_transform(myPolygons, 32632) ))
          names(rr) <- c("FID", bands2use)
          data.c[[as.character(activeLUT.Table$date[[i]])]]<-rr
        }         
      }
 
    }) 
    
    elapsed <- Sys.time() - start
    dt.final <- data.table::rbindlist(data.c, idcol = "Date")
 
    dt.final$Date <- as.Date( dt.final$Date )
    notificationID <<- showNotification(
      paste(sep=" ", 
            "A total of",
            format(nrow(dt.final),  decimal.mark=".",   big.mark="'",small.mark="'"),
             " values extracted from ", 
            nimages, " images in ", round(elapsed,3), units(elapsed)) , duration = NULL )
    
    mYexpression<-gsub("(B[018][0-9A])",   "  dt.final[['\\1']]  " ,    
                       session$input$indici) 

    dt.final$Index <- eval(parse(text=  mYexpression) ) 

    tb <- dt.final %>% dplyr::select(FID, Date, Index, CLD, SNW)
    
     
    if(!is.null(tb) && nrow(tb)==nrow(myPolygons)){
      shinyjs::disable(selector = "#scaricaPoligoni")
      shinyalert::shinyalert(title = "Nessun risultato", 
                             type = "warning")
      return()
      
    }
    tb<-na.omit(tb) 
    print(myPolygons)
    browser()
    if(nrow(tb)<1){
      
      shinyjs::disable(selector = "#scaricaTabellaValori")
      shinyalert::shinyalert(title = "Warning",
                             text = "No overlapping data with areas... did you choose the correct tile?",
                             type = "warning")
      return()
    }
    
    reacts$table.index <- tb
    updateBox( "myBoxAnalytics", action ="restore") 
    if (input$myBoxAnalytics$collapsed)  updateBox( "myBoxAnalytics", action ="toggle")    
    #shinyjs::runjs("$('#shiny-notification-panel')")
    
    
    shinyjs::enable(selector = "#scaricaPoligoni")
    shinyjs::enable(selector = "#scaricaTabellaValori")
      
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