


get20mBandInfo<-function(bandsPath, verbose=F){
  rasters.start<-bandsPath
  
  bands<- (lapply(bands2use, function(x){ 
      idx=grep(x, rasters.start) 
      if(!(length(idx)==1)){
        #stop("No band found")
        stop("problem in band ", x)
      }
      r<- terra::rast(rasters.start[[ idx[[1]] ]])
      ss<-as.numeric( terra::spatSample(r, 1000000, na.rm=T) )
      
      colorQuant<- quantile(  ss, 
                             probs=c(0.001, 0.01, 0.05, 0.10, 0.20, 0.30, 0.40, 0.45,
                                     0.55, 0.60, 0.70, 0.80, 0.90, 0.95, 0.99, 0.999) ) 
      mmax<-colorQuant[[length(colorQuant)]]
      mmin<-colorQuant[[1]]
      breaks.hist<-seq(mmin, mmax, 
          (mmax-mmin)/100 )
      if(length(breaks.hist)<2){
        colorHist<-NA
      } else{ 
        colorHist<- hist(  ss[ss>mmin & ss<mmax  ], plot=F,
                           breaks=breaks.hist ) 
      }
      
      if(verbose) message("Fatto ",  basename(rasters.start[[ idx[[1]] ]]) )
      list(idx=which(bands2use==x), fpath=rasters.start[[ idx[[1]] ]], colorHist=colorHist, colorQuant=colorQuant )
  }))
  
  names(bands)<-bands2use
  
  if(!is.element("B08", names(bands) ) ){
    b8a<-which(names(bands)=="B8A")
    bands[["B08"]]<- bands[[b8a]]
  }
  bands
}

makeVRT<-function(path, vrtname="mapservVRT_20m.vrt", verbose=F){
  
  rasters.start<-list.files(path, ".*_20m\\.jp2$", recursive=T, full.names = T)
 
  bands.list.with.path <- get20mBandInfo(rasters.start, verbose) 
  
  vrt<-file.path(path, vrtname)
  
  gdalUtils::gdalbuildvrt( as.character( sapply( bands.list.with.path, '[[', "fpath", USE.NAMES = F ) ) , 
                           vrt, separate = T )
  
  bands.list.with.path
  
}


compositeCreate<-function(session){ 
  
  if( !shiny::isTruthy(session$input$dayselected) || !shiny::isTruthy(session$input$tile) ){
    shinyalert::shinyalert("Warning", "No tile or image date selected for analysis.")
    return()
  }
  
  masking<-processingMasking[ as.integer(session$input$mskCld)+1, as.integer(session$input$mskSnow)+1 ]
  selectedDate <- (session$input$dayselected)
  
  if(!file.exists(session$userData$mapfile)){ initMapfile(session) }
  
  mapfile<-readr::read_file(session$userData$mapfile)
   
  if(is.null(selectedDate)){
    last.date<-images.lut[ nrow(images.lut), ]
  }   else { 
    last.date<-images.lut %>% filter(date == session$input$dayselected, tile==session$input$tile)
  }
  
  print(last.date)
  if(nrow(last.date)==0){
    shinyalert::shinyalert("Immagine non trovata", "Contatta assistenza")
    return()
  }
  if(nrow(last.date)>1){
    shinyalert::shinyalert("Troppe immagini", "Contatta assistenza")
    return()
  }
  
  print(last.date)
  #vrt<-makeVRT(last.date$folder)
  #if(!file.exists(vrt[[1]])) 
   
  BANDS<-last.date$bands[[1]]
 
  composite <- strsplit( split = " ",session$input$composite)[[1]]
  bandIndex<- as.integer( na.omit( data.table::chmatch( composite, names(BANDS) ) ) )
  
  histTable<-sapply(BANDS[bandIndex], '[[', "colorHist")
 
 
  plotlyProxyInvoke(plotlyProxy("bandHistogram", session), "deleteTraces", 
                    list(as.integer(0), as.integer(1), as.integer(2),
                         as.integer(3), as.integer(4), as.integer(5), 
                         as.integer(6), as.integer(7), as.integer(8),
                         as.integer(9), as.integer(10), as.integer(11)) )

  
  scale<-c()
  for(i in colnames(histTable) ){
    qq<-histTable[ ,i] 
 
    pp<-(qq$density-min(qq$density)) / diff(range(qq$density))
    pp.sum<-sum(pp)
    ttsum<-0
    
    for(tt in 1:length(pp) ){
      ttsum<-ttsum+ pp[[tt]]
      frac<-ttsum/pp.sum
      if(frac > 0.1 && !exists("low")) {
        low<-qq$mids[[tt]] * 0.1/frac
        }
      if(frac > 0.95) {
        
        high<-qq$mids[[tt]] * frac/0.95
        #high<-qq$mids[[tt]]
        scale[[i]]<- paste0( low, ",", high )
        rm(low)
        rm(high)
        break
      }
    }
    
    #plotlyProxyInvoke(plotlyProxy("bandHistogram", session), "deleteTraces", list(as.integer(1)) )
    
    plotlyProxyInvoke(plotlyProxy("bandHistogram", session), "addTraces", list(
      list(
        x = qq$mids,
        name = sprintf("(%s)",i ),
        y =  pp,
        type = 'scatter',
        mode = 'lines',
        line = list(shape = "spline")
      )
    ))
    
  }

  
  compositeString <- sprintf("
    PROCESSING \"BANDS=%s\"
    PROCESSING \"SCALE_1=%s\"
    PROCESSING \"SCALE_2=%s\"
    PROCESSING \"SCALE_3=%s\"  
", paste0(bandIndex, collapse=","), scale[[1]], scale[[2]], scale[[3]]   )
  print(compositeString)
  ## index file is global in server
  
  mapfile <- sub(".*##--- CLOUDPROB PLACEMARK",   
               paste0("    DATA \"",  BANDS$CLD$fpath , "\" ##--- CLOUDPROB PLACEMARK"),  
                mapfile,   perl = T)
  
  mapfile <- sub(".*##--- SNOWPROB PLACEMARK",   
                 paste0("    DATA \"",  BANDS$SNW$fpath , "\" ##--- SNOWPROB PLACEMARK"),  
                 mapfile,   perl = T)
  
  
  mapfile <- sub(".*##--- RGB PLACEMARK",   
                 paste0("    DATA \"",  last.date$VRT[[1]], "\" ##--- RGB PLACEMARK"),  
                 mapfile,   perl = T)
  
  mapfile <- sub(".*##--- INDEX PLACEMARK",   
                 paste0("    DATA \"",  session$userData$indexfile, "\" ##--- INDEX PLACEMARK"),  
                 mapfile,   perl = T)
  
  mapfile <- sub(".*##--- SCL PLACEMARK",   
                 paste0("    DATA \"",  BANDS$SCL$fpath, "\" ##--- SCL PLACEMARK"),  
                 mapfile,   perl = T)
 
  
  mapfile<-gsub("##--- PROCESSINGCOMPOSITE START.*##--- PROCESSINGCOMPOSITE END",   
                         paste0("##--- PROCESSINGCOMPOSITE START\n    ", compositeString, "\n  ##--- PROCESSINGCOMPOSITE END"),  
                         mapfile)
  mapfile<-gsub("##--- PROCESSINGMASKING START.*##--- PROCESSINGMASKING END",   
                         paste0("##--- PROCESSINGMASKING START\n    ", masking, "\n  ##--- PROCESSINGMASKING END"),  
                         mapfile)
   
 
  
  readr::write_file(mapfile, session$userData$mapfile )
  sprintf("https://www.cirgeo.unipd.it/cgi-bin/mapserv?map=%s", 
           session$userData$mapfile)
}
 


initMapfile<-function(session){
  if(!dir.exists(pathsTemp)){
    dir.create(pathsTemp, recursive = T, mode = "0777")
    Sys.chmod(pathsTemp, mode = "0777", use_umask = TRUE)
    Sys.umask(mode = NA)
  }
  mapserv.template<-readr::read_file("mapservTemplate.map")
  readr::write_file(mapserv.template, session$userData$mapfile )
}
