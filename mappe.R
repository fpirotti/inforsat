
#Estraggo le date delle immagini disponibil idal server
getDate<-function(start, end){
 expr = {

   tryCatch(
     expr = {
       if(start>end){return()}
       else if(start<end){
         
             conInfo<-myconnection()
             #qui estraggo uuid delle immagini in base alla data
             #query
             uuidDate<-paste(" SELECT beginposition, uuid FROM esa_catalogue.s2_loaded_mat where beginposition>='",start,"' AND endposition<='",end,"' AND processinglevel='Level-2A'  AND relativeorbitnumber=22 AND cloudcoverpercentage<20 AND identifier ~ 'T32TQS' ORDER BY beginposition ASC");
            
             #risultato
             uuidResDate<-DBI::dbGetQuery(conInfo, uuidDate)  
             
             #creo un vettore per bande e aggiungo il suffisso 10m 
             uuidBand<-as.vector(lapply(uuidResDate[,2],paste0,'_10m'))
             
             #creo un vettore per le date 
             uuidDate<-as.vector(as.character(lapply(uuidResDate[,1],function(x) as.Date(x))))
             
             browser()
             #disconnetto
             closeconnection(conInfo) 

             message("Successfully executed the log(x) call.")
       }
     },
     error = function(e){
       message('Caught an error!')
       print(e)
     },
     warning = function(w){
       message('Caught an warning!')
       print(w)
     },
     finally = {
       return(uuidDate)
       message('All done, quitting.')
     }
   )    
 }
}


getMyMap<-function(input, nomeIndice,polygonString){
  
  withProgress(message = 'Cerco le immagini, rimuovo le nuvole e carico la mappa2', value = 0, min=0, max=100, {   
      x<-c(input$mymap_bounds$west, input$mymap_bounds$east)
      y<-c(input$mymap_bounds$north,input$mymap_bounds$south)
      d <- data.frame(lon=x, lat=y) 
      
      folder<-images.lut[images.lut$dates==input$dayselected, ]$folder
      jp2.files.10m<-list.files(folder, pattern = "10m\\.jp2$", recursive = T, full.names = T)
      
      names(jp2.files.10m)<-  gsub("_", "", 
           stringr::str_extract_all(   basename(jp2.files.10m), "_[ATBW][O018VC][0-9AIPT]_") )
      
      jp2.files.20m<-list.files(folder, pattern = "20m\\.jp2$", recursive = T, full.names = T)
      
      names(jp2.files.20m) <-  gsub("_", "", 
                 stringr::str_extract_all(   
                   basename(jp2.files.20m), c("_[SATBW][O018VC][0-9AIPLT]_|_CLDPRB_|_SNWPRB_") ) )
      
      
       
      jp2.files.20m[["B08"]]<-jp2.files.20m[["B8A"]]
      r<-terra::rast(jp2.files.20m[[1]])
      bb<- st_bbox( st_buffer( st_as_sfc( sf::st_bbox( st_transform( 
        sf::st_as_sf( d, coords=c("lon", "lat"), crs=4326), 
        crs(r)  ) ) ), 10000) )
      
      browser()
      rm(r)
       
      ff<-radio2expression[[ as.integer(input$indici) ]]
      expression.pre<- unique( stringr::str_extract_all(   ff, "B[018][0-9A]")[[1]] )
      if( length(expression.pre)==sum(expression.pre%in%names(jp2.files.10m)) ){
        idx<-which(names(jp2.files.10m)%in%expression.pre)
        files2take<-jp2.files.10m[idx] 
      } else {
        idx<-which(names(jp2.files.20m)%in%expression.pre)
        files2take<-jp2.files.20m[idx] 
      }
      
      crs <- sf::st_crs(3857)$proj4string
      croppedBands<-list()
      tot<-length(names(files2take))
      n<-0
      for(i in names(files2take)){
        n<-n+1
        setProgress(n/tot*80, message = sprintf("Ritaglio %s", i) )
      #  Sys.sleep(20)
        croppedBands[[i]]<-terra::crop(terra::rast(files2take[[i]]), bb)
        gdalUtils::gdalwarp(files2take[[i]])
        
        ## otimizzo, faccio reproject per leaflet solo se composite , 
        ## in quanto indici diventano un singolo raster e lo faccio dopo
        if(grepl(",", ff)) { croppedBands[[i]]<-terra::project( croppedBands[[i]], crs)  }
      }
      
      ff<-radio2expression[[ as.integer(input$radio) ]]
      mYexpression<-gsub("(B[018][0-9A])", 
                         "  croppedBands[['\\1']]  " , 
                         ff) 
       
     
     setProgress(85, message = sprintf("Calcolo %s", ff) )
     myMap<-eval(parse(text= paste0("c(", mYexpression, ")")) )
     
     ## se c'è una virgola è una combinazione tra bande, altrimenti un indice
     
     setProgress(95, message = "Esporto" )
     if(grepl(",", ff)) {
        myMap2<-raster::brick(myMap)
     } else {
       myMap2<-raster( terra::project( myMap, crs) )     
     }
     setProgress(100, message = "Finito" )
      
  } )
  return( myMap2 )
  
  
}
#estraggo la mappa
getMyMap.backup<-function(input, nomeIndice,polygonString){
  
  withProgress(message = 'Cerco le immagini, rimuovo le nuvole e carico la mappa2', value = 0, {    
    conInfo<-myconnection()
    
    start_time <- Sys.time()
    incProgress(1/4, message = "Ritaglio immagine", detail = "Cerco le immagini")
 
    uuidDate<-paste0("SELECT beginposition, uuid, * FROM esa_catalogue.s2_loaded_mat where date_trunc('Day', beginposition)='",input$dayselected,"' AND processinglevel='Level-2A'  AND relativeorbitnumber=22 AND cloudcoverpercentage<200 AND identifier ~ 'T32TQS' ORDER BY beginposition ASC");

    uuidResDate<-DBI::dbGetQuery(conInfo, uuidDate)  
    
    browser()
    
    end_time <- Sys.time()
    print(end_time - start_time)
    start_time <- Sys.time()
    #creo un vettore per bande e aggiungo il suffisso in base all'indice
    uuidBand<-as.vector(lapply(uuidResDate[,2],paste0,spatialResolution[as.numeric(nomeIndice)]))
    
    
    x<-c(input$mymap_bounds$west, input$mymap_bounds$east)
    y<-c(input$mymap_bounds$north,input$mymap_bounds$south)
    d <- data.frame(lon=x, lat=y)
    sp::coordinates(d) <- c("lon", "lat")
    sp::proj4string(d) <- sp::CRS("+init=epsg:4326") # WGS 84
    d32<-sp::spTransform(d, sp::CRS("+init=epsg:32632"))

    stringa<-c("s2",uuidBand[1])
    
    
    incProgress(2/4, message = "Ritaglio immagine", detail = "Ritaglio le immagini")
#IR<-rpostgis::pgGetRast(conInfo, as.character(c("s2","65dbe162-191c-4fab-b614-1334ab6909cc_10m")), rast = "rast", bands=4, boundary=c(5137326, 5135961, 732105.7 , 728978.6 ) )

      switch(nomeIndice, 
           '1'={
             b<-checkTC(rpostgis::pgGetRast(conInfo, as.character(stringa), rast = "rast", bands=c(3,4), boundary=c(d32@coords[1,2], d32@coords[2,2],d32@coords[2,1],d32@coords[1,1]) ) 
             )
             IR<-b[[2]]
             R<-b[[1]]

             
             # R<-checkTC(rpostgis::pgGetRast(conInfo, as.character(stringa), rast = "rast", bands=3,boundary=c(d32@coords[1,2], d32@coords[2,2],d32@coords[2,1],d32@coords[1,1]) ) 
             # )
             end_time <- Sys.time()
             message(end_time - start_time)
             start_time <- Sys.time()
             print("Calcolo...")
             myMap2<-checkTC( (IR-R)/(IR+R))
             
             end_time <- Sys.time()
             print(end_time - start_time)
             start_time <- Sys.time()
             #Maschero: ombre nuvole, nuvole 3 & 8 & 9 & 10 
             # myMap2<-maskRaster(uuidResDate[,2], myMap2, d32)
             },
           '2'={             
             b<-checkTC(rpostgis::pgGetRast(conInfo, as.character(stringa), rast = "rast", bands =c(2,3), boundary=c(d32@coords[1,2], d32@coords[2,2],d32@coords[2,1],d32@coords[1,1]) ) 
             )
             
             G<-b[[1]]
             R<-b[[2]]
             myMap2<-checkTC(R/G)
             
             #Maschero: ombre nuvole, nuvole 3 & 8 & 9 & 10 
             # myMap2<-maskRaster(uuidResDate[,2], myMap2, d32)
             },
           '3'={
 
             b<-checkTC(rpostgis::pgGetRast(conInfo, as.character(c("s2",as.vector(lapply(uuidResDate[,2],paste0,'_10m')))), rast = "rast", bands=c(4,5), boundary=c(d32@coords[1,2], d32@coords[2,2],d32@coords[2,1],d32@coords[1,1]) ) 
             )
             NIR8<-b[[1]]
             NIR11<-b[[2]]
              
             NIR11<-resample(NIR11,NIR8,method="ngb")
             
             myMap2<-(NIR8-NIR11)/(NIR8+NIR11)
             #Maschero: ombre nuvole, nuvole 3 & 8 & 9 & 10 
             # myMap2<-maskRaster(uuidResDate[,2], myMap2, d32)
             },
           '4'={

             N<-checkTC(rpostgis::pgGetRast(conInfo, as.character(c("s2",as.vector(lapply(uuidResDate[,2],paste0,'_10m')))), rast = "rast", bands=4, boundary=c(d32@coords[1,2], d32@coords[2,2],d32@coords[2,1],d32@coords[1,1]) ) )

             myMap2<-N/10000
             
             #Maschero: ombre nuvole, nuvole 3 & 8 & 9 & 10 
             # myMap2<-maskRaster(uuidResDate[,2], myMap2, d32)
             
           },
           '5'={
             #print(as.character(c("s2",as.vector(lapply(uuidResDate[,2],paste0,'_10m')))), rast = "rast", bands=c(1,2,3), boundary=c(d32@coords[1,2], d32@coords[2,2],d32@coords[2,1],d32@coords[1,1]))

             rgb<-checkTC(rpostgis::pgGetRast(conInfo, as.character(c("s2",as.vector(lapply(uuidResDate[,2],paste0,'_10m')))), rast = "rast", bands=c(1,2,3), boundary=c(d32@coords[1,2], d32@coords[2,2],d32@coords[2,1],d32@coords[1,1]) )) 

             rgb2<-rgb
             myMap2<-rgb2
           },
           stop("Enter something that switches me!")
           )#chiudo switch
    
    end_time <- Sys.time()
    print(end_time - start_time)
    start_time <- Sys.time()
    
    incProgress(3/4, message = "Maschero immagine")
    myMap2<-maskRaster(uuidResDate[,2], myMap2, d32)
    incProgress(0/1, detail = paste("Doing part", 1)) 
    end_time <- Sys.time()
    print(end_time - start_time)
    start_time <- Sys.time()
    
    })
    closeconnection(conInfo)
    return(myMap2)            
}

#Maschera il raster per togliere le nuvole, cirri e ombre delle nuvole dalla mappa
maskRaster<-function(uuidResDate, mappa, vettoreCoordinate){
  
      indice<-mappa
      
      bbox<-vettoreCoordinate
        
      conInfo<-myconnection()
      
      #extraggo il prodotto con le maschere
      s2_Scl<-uuidResDate
      
      uuidBand<-paste0(s2_Scl,spatialResolution[5])

      stringa<-c("s2",uuidBand)
     
      
      mioCloud<-rpostgis::pgGetRast(conInfo,as.character(stringa) , rast = "rast", bands=1, boundary=c(bbox@coords[1,2], bbox@coords[2,2],bbox@coords[2,1],bbox@coords[1,1]) ) 

      #disconnetto
      closeconnection(conInfo)
      
      
      #valori per nuovle cirri e ombre 
      #3 ombre nuvole 
      #8 nuvole media prob
      #9 nuvole alta prob
      #10 cirri
      #11 neve
      valoriMaschera<-c(0,NA,
                        1,NA,
                        2,NA,
                        4,NA,
                        5,NA,
                        6,NA,
                        7,NA,
                        10,NA,
                        11,NA
      )
      
      rmMatrix<-matrix(valoriMaschera, ncol=2, byrow=TRUE)
      
      mioCloud2 <- raster::reclassify(mioCloud, rmMatrix, include.lowest=TRUE)
      
      #ricampiono a 10 m
      
      mioCloud2<-raster::resample(mioCloud2,indice)
    

      mappaMascherata<-raster::mask(indice, mioCloud2, inverse=TRUE)

      return(mappaMascherata)
}


#Maschera il raster per togliere le nuvole, cirri e ombre delle nuvole Nel grafico
#La differenza dalla funzione Mask Raster è nel bounding-box
maskRasterGrafico<-function(uuidResDate, mappa, vettoreCoordinate){
  
  indice<-mappa
  
  conInfo<-myconnection()
  
  #extraggo il prodotto con le maschere
  s2_Scl<-uuidResDate
  
  uuidBand<-paste0(s2_Scl,spatialResolution[5])
  
  stringa<-c("s2",uuidBand)
  
  d32<-vettoreCoordinate
  
  mioCloud<-rpostgis::pgGetRast(conInfo,as.character(stringa) , rast = "rast", bands=1, boundary=c(bbox(d32)[2,2], bbox(d32)[2,1],bbox(d32)[1,2],bbox(d32)[1,1]) ) 
  
  #disconnetto
  closeconnection(conInfo)
  
  #plot(mioCloud)
  
  #valori per nuvole,  ombre prob media e altra 3 8 9  
  #no_data
  #saturated_or_defeective
  #dark_area_pixel
  #vegetation
  #not_vegetated
  #water
  #unclassified
  #this_cirrus
  #snow
  valoriMaschera<-c(0,NA, 
                    1,NA, 
                    2,NA, 
                    4,NA, 
                    5,NA, 
                    6,NA, 
                    7,NA, 
                    10,NA,
                    11,NA)
  
  rmMatrix<-matrix(valoriMaschera, ncol=2, byrow=TRUE)
  
  mioCloud2 <- raster::reclassify(mioCloud, rmMatrix, include.lowest=TRUE)
  
  #ricampiono a 10 m
  
  mioCloud2<-raster::resample(mioCloud2,indice)
  
  #plot(mioCloud2) 
  
  mappaMascherata<-raster::mask(indice, mioCloud2, inverse=TRUE)
  
  return(mappaMascherata)
}
