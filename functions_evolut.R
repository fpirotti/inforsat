


get20mBandNames<-function(bandsPath){
  rasters.start<-bandsPath
  
  bands<- (lapply(bands2use, function(x){ 
      idx=grep(x, rasters.start) 
      if(!(length(idx)==1)){
        #stop("No band found")
        warning("problem in band ", x)
      }
      list(idx=which(bands2use==x), fpath=rasters.start[[ idx[[1]] ]] )
  }))
  
  names(bands)<-bands2use
  
  if(!is.element("B08", names(bands) ) ){
    b8a<-which(names(bands)=="B8A")
    bands[["B08"]]<- bands[[b8a]]
  }
  bands
}

makeVRT<-function(path, vrtname="mapservVRT_20m.vrt"){
  rasters.start<-list.files(path, ".*_20m\\.jp2$", recursive=T, full.names = T)
 
  bands.list.with.path <- get20mBandNames(rasters.start) 
  vrt<-file.path(path, vrtname)
  gdalUtils::gdalbuildvrt( as.character( sapply( bands.list.with.path, '[[', "fpath", USE.NAMES = F ) ) , 
                           vrt, separate = T )
  bands.list.with.path
}


compositeCreate<-function(session  ){
  
  masking<-processingMasking[ as.integer(session$input$mskCld)+1, as.integer(session$input$mskSnow)+1 ]
  selectedDate <- as.character(session$input$dayselected)
  composite <- session$input$composite
  
  if(!file.exists(session$userData$mapfile)){ initMapfile(session) }
  
  mapfile<-readr::read_file(session$userData$mapfile)
   
  if(is.null(selectedDate)){
    last.date<-images.lut[ nrow(images.lut), ]
  }   
    
  last.date<-images.lut[ which(images.lut$dates==selectedDate), ]
  
  #vrt<-makeVRT(last.date$folder)
  #if(!file.exists(vrt[[1]])) 
   
  BANDS<-last.date$bands[[1]]
 
  ## index file is global in server
  
  mapfile <- sub(".*##--- CLOUDPROB PLACEMARK",   
               paste0("    DATA \"",  BANDS$CLD$fpath , "\" ##--- CLOUDPROB PLACEMARK"),  
                mapfile, 
               perl = T)
  
  mapfile <- sub(".*##--- SNOWPROB PLACEMARK",   
                 paste0("    DATA \"",  BANDS$SNW$fpath , "\" ##--- SNOWPROB PLACEMARK"),  
                 mapfile, 
                 perl = T)
  
  
  mapfile<-gsub("##--- RGB START.*##--- RGB END",   
                  paste0("##--- RGB START\n     DATA \"", last.date$VRT[[1]], "\"\n   ##--- RGB END"),  
                  mapfile)
  mapfile<-gsub("##--- INDEX START.*##--- INDEX END", 
                         paste0("##--- INDEX START\n    DATA \"", session$userData$indexfile, "\"\n   ##--- INDEX END"),  
                         mapfile)
  
  mapfile<-gsub("##--- SCL START.* ##--- SCL END", 
                         paste0("##--- SCL START\n    DATA \"", BANDS$SCL$fpath, "\"\n     ##--- SCL END"),  
                         mapfile)
  
  
  mapfile<-gsub("##--- PROCESSINGCOMPOSITE START.*##--- PROCESSINGCOMPOSITE END",   
                         paste0("##--- PROCESSINGCOMPOSITE START\n    ", composite, "\n  ##--- PROCESSINGCOMPOSITE END"),  
                         mapfile)
  mapfile<-gsub("##--- PROCESSINGMASKING START.*##--- PROCESSINGMASKING END",   
                         paste0("##--- PROCESSINGMASKING START\n    ", masking, "\n  ##--- PROCESSINGMASKING END"),  
                         mapfile)
   
  
  print("Qui")

  
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
