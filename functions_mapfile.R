### 
createColorScale4index <- function(session, qq, qq.color){
  
  colscale<-c()
  
  for(i.color in rev(1:(length(qq)-1)) ){
   colscale<-c(colscale, sprintf("
  CLASS
    EXPRESSION ([pixel] >= %f and [pixel] < %f)
    STYLE
      COLORRANGE  \"%s\"  \"%s\"
      DATARANGE %f %f
    END # STYLE                   
  END #CLASS",  qq[[i.color]], 
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
    #leaflet::showGroup(group='INDICE') %>%
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

fixedScale4index<-function(session, r, index="NDVI"){
  qq <- c(-1, seq(0,1, by=0.1) )
  qq.color<-c('#0000FF', '#c7eae5', '#a50026','#d73027','#f46d43','#fdae61','#fee08b','#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837')
  createColorScale4index(session, qq, qq.color)
  
}

calcMapserverScale4index<-function(session, r){
  
  qq<- quantile(r[], probs = seq(0,1, by=0.1), 
                names = FALSE, na.rm=T )
  
  print("calcMapserverScale")
  
  qq.color<-RColorBrewer::brewer.pal( length(qq),"Spectral")
  
  createColorScale4index(session, qq, qq.color)
  
  
}

createIndexFile<-function(session){
   
  if( !shiny::isTruthy(session$input$dayselected) ){
    shinyalert("Warning", "Select an image using the date drop down menu in the 'Layers' panel on the left.")
    return()
  }
  
  shinyjs::show("scaricaIndice")
 
  nT <-  images.lut %>% filter(date == session$input$dayselected, tile==session$input$tile )
  if( nrow(nT)!=1 ){
    shinyalert("Problema qui")
    return()
  }
  date<- nT$date[[1]]
  bands<- (nT$bands[[1]])
  folder<-nT$folder[[1]]
  vrt<-file.path(folder, "mapservVRT_20m.vrt")
  if( !file.exists(vrt) ){
    shinyalert("VRT non trovato", "Contatta assistenza")
    return()
  }
  
  
  bands2use<- unique( stringr::str_extract_all(   session$input$indici , "B[018][0-9A]")[[1]] )
  
  withProgress(message = 'Calculating index in real time...', value = 0, {    
    bands.raster<-list()
    cc<-1
    for(i in bands2use){
      setProgress(cc/(length(bands2use)+3), detail = sprintf("Extracting band %s...", i) ) 
      ii.init<-i
      
      iitm<-list.files(folder, pattern= sprintf("_20m\\.jp2$", i),  
                     recursive = T, full.names = F)
      
      if(length(ii)!=1){
        if(i=="B08") {
          i<-"B8A"
          ii<-list.files(folder, pattern= sprintf("_%s_20m\\.jp2$", i),  
                       recursive = T, full.names = T)
        } else {
          ii<-list.files(folder, pattern= sprintf("_%s_60m\\.jp2$", i),  
                         recursive = T, full.names = T)
        }
      }
      
 
      
      if(length(ii)!=1){ 
        shinyalert(sprintf("Band %s in your equation not found anywhere!", i))
        return()
      }
 
      res<-0.149*2^(21 - session$input$mymap_zoom)
       
      if(res<20) res<-20 
      
      outRaster<-sprintf("%s/%s_%s.tif", pathsTemp, session$token, ii.init)
      gdalUtils::gdalwarp(
        ii[[1]],
        outRaster,
        r = session$input$resampling,
       # r = "bilinear",
        te = as.numeric(session$input$mymap_bounds[ c(4,3,2,1)]),
        te_srs = (st_crs(4326))$proj4string ,
        t_srs = st_crs(3857)$proj4string,
        tr = c(res,res),
        overwrite = T
      )
  
      bands.raster[[ii.init]] <- terra::rast(outRaster)
    }
    
    
    shinyjs::runjs( sprintf(' $("#infolog").append("<P>Risoluzione %d");', as.integer(res) ) )
    
    setProgress((cc+1)/(length(bands2use)+3), detail = sprintf("Calcolo %s", session$input$indici) )
    
    mYexpression<-gsub("(B[018][0-9A])",   "  bands.raster[['\\1']]  " ,    
                       session$input$indici) 
    r<-eval(parse(text=  mYexpression) )
    masks<-NULL
    if(isTruthy(session$input$mskCld) && session$input$mskCld!=0 ){

      outRaster<-sprintf("%s/%s_CLD.tif", pathsTemp, session$token, ii.init)
      gdalUtils::gdalwarp(
        bands$CLD$fpath,
        outRaster,
        te = as.numeric(session$input$mymap_bounds[ c(4,3,2,1)]),
        te_srs = (st_crs(4326))$proj4string ,
        t_srs = st_crs(3857)$proj4string,
        tr = c(res,res),
        overwrite = T
      )
      r<-  terra::mask(r,   (terra::rast(outRaster) > session$input$mskCld), maskvalues=1)
    }
    
    if(isTruthy(session$input$mskSnw)&& session$input$mskSnw!=0 ){
      
      outRaster<-sprintf("%s/%s_SNW.tif", pathsTemp, session$token, ii.init)
      gdalUtils::gdalwarp(
        bands$SNW$fpath,
        outRaster,
        te = as.numeric(session$input$mymap_bounds[ c(4,3,2,1)]),
        te_srs = (st_crs(4326))$proj4string ,
        t_srs = st_crs(3857)$proj4string,
        tr = c(res,res),
        overwrite = T
      ) 
      r<-  terra::mask(r,   (terra::rast(outRaster) > session$input$mskCld), maskvalues=1 )
    }
    
  setProgress((cc+2)/(length(bands2use)+3), detail = sprintf("Scrivo...") )
  terra::writeRaster(r, session$userData$indexfile, datatype="FLT4S", overwrite = T )
  
  if(session$input$freezeScale){
    
    fixedScale4index(session,r)
    return()
  }
  
  calcMapserverScale4index(session,r)
  
  })
  
}