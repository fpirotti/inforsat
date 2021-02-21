library(sf)
library(terra)
library(shinydashboard)
library(dplyr)
library(geojsonsf)
library(shinyjqui)
library(plotly)
## nome layer e ID in WMS mapserver
## NB versione con cloud mask= +1 e con snow mask +2 entrambi +3
satLayers<-list("Color Composite RGB"=c("100", ""), 
                "Color Composite NIR"=c("200",""),
                "INDICI"=c("300", "Indice vegetazionale scelto"),
                "Classificazione ESA"=c("300", "") )
## 1=nomask
## 2= maskcloud
## 3= maskSnow
## 4 = allMask
 

processingComposite<-list(
 "Colori Reali" = 'PROCESSING "BANDS=4,3,2,11"', 
 "SCL" = 'PROCESSING "BANDS=11,11,11,11"', 
 "NIR1 (8,4,3)" = 'PROCESSING "BANDS=10,4,3,11"',
 "NIR2 (7,4,3)" = 'PROCESSING "BANDS=7,4,3,11"',
 "NIR3 (6,4,3)" = 'PROCESSING "BANDS=6,4,3,11"',
 "NIR4 (5,4,3)" = 'PROCESSING "BANDS=5,4,3,11"',
 "Urban (12,11,4)" = 'PROCESSING "BANDS=9,8,4,11"',
 "Agriculture (11,8,2)" = 'PROCESSING "BANDS=8,10,2,11"',
 "Atmospheric penetration (12,11,8)" = 'PROCESSING "BANDS=9,8,10,11"',
 "Vegetazione sana (8,11,2)" = 'PROCESSING "BANDS=10,8,2,11"',
 "Suolo/Acqua (8,11,4)" = 'PROCESSING "BANDS=10,8,4,11"',
 "Colori naturali compensati con atmosfera (12,8,3)" = 'PROCESSING "BANDS=9,10,3,11"',
 "SWIR (12,8,4)" = 'PROCESSING "BANDS=9,10,3,11"'
)
processingMasking<-data.frame(
  a=c("PROCESSING \"LUT_4=0:255,1:255,2:255,3:255,4:255,5:255,6:255,7:255,8:255,9:255,10:255,11:255\"" ,  
      "PROCESSING \"LUT_4=0:0,1:0,2:0,3:0,4:255,5:255,6:255,7:0,8:0,9:0,10:0,11:255\"" ),
  b=c("PROCESSING \"LUT_4=0:0,1:0,2:0,3:255,4:255,5:255,6:255,7:255,8:255,9:255,10:255,11:0\"",
      "PROCESSING \"LUT_4=0:0,1:0,2:0,3:0,4:255,5:255,6:255,7:255,8:0,9:0,10:0,11:0\"")
)

pathsTemp<-"/archivio/tmp/ms_tmp"

get20mBandNames<-function(path){
  rasters.start<-list.files(path, "_20m.jp2$", recursive=T, full.names = T)
  bands<-gsub(".*_([ABTCSVW][0-9OCLINV][LPSWTVDI0-9A])_20m.jp2$", "\\1", basename(rasters.start))
  bands<-as.list(structure(1:length(bands), names=bands))
  browser()
  if(!is.element("B08", names(bands) ) ){
    bands[["B08"]]<- which(names(bands)=="B8A")
  }
  bands
}

makeVRT<-function(path){
  rasters.start<-list.files(path, "_20m.jp2$", recursive=T, full.names = T)
  rasters.scl<-list.files(path, "SCL_20m.jp2$", recursive=T, full.names = T)
  
  #save(bands, file="data.rda")
  #bands<-gsub(".*_([ABTCSVW][0-9OCLINV][LPSWTVDI0-9A])_20m.jp2$", "\\1", basename(rasters.start))
  #piani2012<-readRDS(file="shapefile/piani4326.rds")
  vrt<-file.path(path, "mapservVRT_20m.vrt")[[1]]
  gdalUtils::gdalbuildvrt(c(rasters.start), vrt, separate = T)
  c(vrt,rasters.scl)
}

compositeCreate<-function(session, onlyVRT=F){
  
  masking<-processingMasking[ as.integer(session$input$mskCld)+1, as.integer(session$input$mskSnow)+1 ]
  dateImage <- as.character(session$input$dayselected)
  composite <- session$input$composite
  
  mapserv.template<-readr::read_file("mapservTemplate.map")
  
  if(composite==1) composite<-processingComposite[[1]]
  if(is.null(dateImage)){
    last.date<-images.lut[ nrow(images.lut), ]
  } else {  
    last.date<-images.lut[ which(images.lut$dates==dateImage), ]
  }
  vrt<-makeVRT(last.date$folder)
  #if(!file.exists(vrt[[1]])) 
  
  
  if(onlyVRT) return()
  
  ## index file is global in server
  indexfile<<- file.path(pathsTemp, sprintf("%s.tif", session$token))
  mapserv.template<-gsub("nnnnnnnnnnnnnnnnn", vrt[[1]], mapserv.template)
  mapserv.template<-gsub("IIIIIIIIIIIII", indexfile, mapserv.template)
  
  mapserv.template<-gsub("sclsclsclscl", vrt[[2]], mapserv.template)
  mapserv.template<-gsub("PROCESSINGCOMPOSITE", composite, mapserv.template)
  mapserv.template<-gsub("PROCESSINGMASKING", masking, mapserv.template)
  
  
  if(!dir.exists(pathsTemp)){
    dir.create(pathsTemp, recursive = T, mode = "0777")
    Sys.chmod(pathsTemp, mode = "0777", use_umask = TRUE)
    Sys.umask(mode = NA)
  }
  
  mapfile  <- file.path(pathsTemp, sprintf("%s.map", session$token))
  readr::write_file(mapserv.template, mapfile )
  sprintf("https://www.cirgeo.unipd.it/cgi-bin/mapserv?cache=%s&map=%s", Sys.time(), mapfile)
}
 

###  OGGETTO LEAFLET DA METTERE SU SERVER.R
leaflet.object <-
  leaflet()%>%
  
#  addLogo(img,  url = "img/logow.png", width = 145, height= 50)%>%
  
  leaflet::addProviderTiles("Esri", group="ESRI")%>%
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
    baseGroups = c("OpenStreetMap","ESRI","Ortofoto 2015 Regione Veneto"),
    overlayGroups = c( "INDICE","Color Composite",  "Scene Classification"),
    layersControlOptions(autoZIndex = FALSE) 
  ) %>% 
  hideGroup("Color Composite") %>% 
  hideGroup("Scene Classification") %>% 
  hideGroup("INDICE") %>% 
  
  leaflet.extras::addDrawToolbar(
    targetGroup='draw',
    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
    polygonOptions = drawPolygonOptions(showArea = TRUE, metric = TRUE, 
                                        shapeOptions = drawShapeOptions(), repeatMode = FALSE),
    circleMarkerOptions = FALSE,
    markerOptions  = FALSE,
    circleOptions = FALSE,
    singleFeature = FALSE) %>%
  
  # leaflet.extras::addStyleEditor(position = "topleft", 
  #                                openOnLeafletDraw = TRUE)%>%
  
  enableMeasurePath()%>%
  
  leaflet.extras::addMeasurePathToolbar()%>%
  
  leaflet::addScaleBar("bottomleft")%>%
  
  leaflet::setView( lng=11.970140, lat=46.349963, zoom = 12) 
