require(terra)


calcMapserverScale4index<-function(session, r){
  qq<- quantile(r[], probs = seq(0,1, by=0.1), 
                names = FALSE, na.rm=T )
  
  print("calcMapserverScale")
  
  qq.color<-RColorBrewer::brewer.pal( length(qq),"Spectral")
  
  colscale<-c()
  for(i.color in rev(1:(length(qq)-1)) ){
    colscale<-c(colscale, sprintf("
  CLASS
    EXPRESSION ([pixel] >= %f and [pixel] < %f)
    STYLE
      COLORRANGE  \"%s\"  \"%s\"
      DATARANGE %f %f
    END # STYLE                   
  END #CLASS", qq[[i.color]], 
                                  qq[[i.color+1]],
                                  qq.color[[i.color]], 
                                  qq.color[[(i.color+1)]], 
                                  qq[[i.color]], 
                                  qq[[i.color+1]]
    ) 
    )
  }
  
  colscale2<-paste(collapse = "

", colscale)
  
  if(!file.exists(session$userData$mapfile)){ initMapfile(session) }
  mapfile.text  <- readr::read_file( session$userData$mapfile )
  
  mapfile2<-gsub("##########&&&&&&&.*##########&&&&&&&", 
                 sprintf("##########&&&&&&&
  
  #PROCESSING \"SCALE=%f,%f\"

    %s

  ##########&&&&&&&",qq[[1]], qq[[( length(qq))]], colscale2), mapfile.text)
  
  readr::write_file(mapfile2, session$userData$mapfile   )
  
  
  leaflet::leafletProxy('mymap') %>%
    
    leaflet::clearGroup(group='INDICE') %>%
    leaflet::removeControl('myLegend') %>% 
    leaflet::showGroup(group='INDICE') %>%
    addWMSTiles(
      session$userData$wms.url,
      group="INDICE",
      layers = "300", 
      options = WMSTileOptions( format = "image/png", transparent = T, 
                                cache=as.character(Sys.time())  ),
      attribution = "CIRGEO-TESAF") %>%
    addLegend(
      colors = rgb(t(col2rgb(qq.color)) / 255),
      labels = sprintf("%.2f", qq), opacity = 1,
      layerId = "myLegend")
  
  
}

createIndexFile<-function(session){
   
   
  shinyjs::show("scaricaIndice")
  nT <- which(images.lut$dates == session$input$dayselected)

  date<- (images.lut[nT, ]$dates)
  bands<- (images.lut[nT, ]$bands)[[1]]
  folder<-images.lut[nT, ]$folder
  vrt<-file.path(folder, "mapservVRT_20m.vrt")
  if( !file.exists(vrt) ){
    shinyalert("VRT non trovato", "Contatta assistenza")
    return()
  }
  
  
  bands2use<- unique( stringr::str_extract_all(   session$input$indici , "B[018][0-9A]")[[1]] )
  
  withProgress(message = 'Calcolo Indice', value = 0, {    
    bands.raster<-list()
    cc<-1
    for(i in bands2use){
      setProgress(cc/(length(bands2use)+3), detail = sprintf("Estraggo banda %s...", i) ) 
      ii.init<-i
      ii<-list.files(folder, pattern= sprintf("_%s_20m\\.jp2$", i),  
                     recursive = T, full.names = T)
      if(length(ii)!=1){
        if(i=="B08") i<-"B8A"
        ii<-list.files(folder, pattern= sprintf("_%s_20m\\.jp2$", i),  
                       recursive = T, full.names = T)
      }
      if(length(ii)!=1){
        shinyalert("Banda %s in equazione non trovata nella cartella", i)
        return()
      }
 
      res<-0.149*2^(21-session$input$mymap_zoom)
      if(res<20) res<-20
      outRaster<-sprintf("%s/%s_%s.tif", pathsTemp, session$token, ii.init)
      gdalUtils::gdalwarp(
        ii[[1]],
        outRaster,
        r = "near",
        te = as.numeric(session$input$mymap_bounds[ c(4,3,2,1)]),
        te_srs = (st_crs(4326))$proj4string ,
        t_srs = st_crs(3857)$proj4string,
        tr = c(res,res),
        overwrite = T
      )
  
      bands.raster[[ii.init]] <- terra::rast(outRaster)
    }
    
    
    setProgress((cc+1)/(length(bands2use)+3), detail = sprintf("Calcolo %s", session$input$indici) )
    
    mYexpression<-gsub("(B[018][0-9A])",   "  bands.raster[['\\1']]  " ,    
                       session$input$indici) 
    r<-eval(parse(text=  mYexpression) )

  setProgress((cc+2)/(length(bands2use)+3), detail = sprintf("Scrivo...") )
  terra::writeRaster(r, session$userData$indexfile, datatype="FLT4S", overwrite = T )
  
  if(session$input$freezeScale){
    leaflet::leafletProxy('mymap') %>% 
      leaflet::clearGroup(group='INDICE') %>% 
      leaflet::showGroup(group='INDICE') %>%
      addWMSTiles(
        session$userData$wms.url,
        group="INDICE",
        layers = "300", 
        options = WMSTileOptions( format = "image/png", transparent = T, 
                                  cache=as.character(Sys.time())  ),
        attribution = "CIRGEO-TESAF") 
    return()
  }
  
  calcMapserverScale4index(session,r)
  
  #browser()
  ##########&&&&&&&
  })
  
}