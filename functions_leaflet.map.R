
overlayGroups.WMS.layerIDS <- list("100"="S2 Color Composite",
                                   "400"="S2 Scene Classification", 
                                   "500"="S2 CLOUD MSK", 
                                   "600"="S2 SNOW MSK", 
                                   "300"="INDICE")  

#as.character(overlayGroups.WMS.layerIDS)
###  OGGETTO LEAFLET DA METTERE SU SERVER.R ------


leaflet.object <-
  leaflet()%>%
  
#  addLogo(img,  url = "img/logow.png", width = 145, height= 50)%>%
  
  leaflet::addProviderTiles("Esri", group="ESRI")%>%
  leaflet::addTiles(group="OpenStreetMap")%>%
  addBingTiles("Satellite BING maps", group="BING", apikey = "AjvjPYuoA4IgNeooKvodDLcxbVL1F8RdIxXUeYsb6PgiVapURz_PbbWvOxVKmNps",
               imagerySet = c("Aerial") )%>%
  ## F.PIROTTI
  leaflet::addTiles(urlTemplate  ="//idt2.regione.veneto.it/gwc/service/wmts?&REQUEST=GetTile&contextualWMSLegend=0&crs=EPSG:900913&dpiMode=7&format=image/jpeg&layer=rv:OrthoPhoto_2015_pyramid&styles=&tileMatrixSet=EPSG:900913&TILEMATRIX=EPSG:900913:{z}&TILEROW={y}&TILECOL={x}&url=https://idt2.regione.veneto.it/gwc/service/wmts",
                    #layers = "rv:OrthoPhoto_2015_pyramid",
                    options = tileOptions(crs="EPSG:900913", format = "image/jpeg", transparent = FALSE),
                    group="Ortofoto 2015 Regione Veneto",
                    attribution = "© Regione Veneto"
  )%>%  
  leaflet::addTiles(urlTemplate  ="//idt2.regione.veneto.it/gwc/service/wmts?&REQUEST=GetTile&contextualWMSLegend=0&crs=EPSG:900913&dpiMode=7&format=image/png&layer=rv:Ortofoto2018_Veneto&styles=&tileMatrixSet=EPSG:900913&TILEMATRIX=EPSG:900913:{z}&TILEROW={y}&TILECOL={x}&url=https://idt2.regione.veneto.it/gwc/service/wmts",
                          #layers = "rv:OrthoPhoto_2015_pyramid",
                          options = tileOptions(crs="EPSG:900913", format = "image/jpeg", transparent = FALSE),
                          group="Ortofoto 2018 Regione Veneto",
                          attribution = "© Regione Veneto"
  )%>%
  leaflet::addTiles(urlTemplate  ='', group="Blank")%>%
  leaflet::addPolygons(
    data = as_Spatial(st_zm(tiles.geom))  , 
    color = "#FF0000",
    group="Sentinel-2 TILES",
    weight = 3,
    opacity = 0.8,
    fill = FALSE,  
    label = ~name,
    labelOptions =labelOptions(
      interactive = FALSE, 
      textsize = "14px", 
      permanent = TRUE),
    options = pathOptions(interactive=F, clickable = F) 
  ) %>% 
  leaflet::addLayersControl(
    position =("topright"),
    baseGroups = c("Blank", "OpenStreetMap","ESRI","Ortofoto 2015 Regione Veneto",  
                   "Ortofoto 2018 Regione Veneto", "BING"),
    overlayGroups = as.character(c(overlayGroups.WMS.layerIDS, "Sentinel-2 TILES")),
    layersControlOptions(autoZIndex = FALSE, collapsed = F) 
  ) %>% 
  
  hideGroup( as.character(overlayGroups.WMS.layerIDS) )   %>% 
  showGroup( "BING" )   %>% 
  
  showGroup( "INDICE" )   %>% 
  leaflet.extras::addDrawToolbar(
    targetGroup='draw',
    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
    polygonOptions = drawPolygonOptions(showArea = TRUE, metric = TRUE, 
                                        shapeOptions = drawShapeOptions(), repeatMode = FALSE),
    circleMarkerOptions = FALSE,
    markerOptions  = FALSE,
    circleOptions = FALSE , #drawCircleOptions(showRadius=T),
    singleFeature = FALSE) %>%
  
  # leaflet.extras::addStyleEditor(position = "topleft", 
  #                                openOnLeafletDraw = TRUE)%>%
  
  #enableMeasurePath()%>%
  
  leaflet.extras::addMeasurePathToolbar()%>%
  
  leaflet::addScaleBar("bottomright")%>%
  addMiniMap(tiles = "OpenStreetMap", toggleDisplay = TRUE,
             position = "topright") %>%
  htmlwidgets::onRender("
    function(el, x) {
      myMap = this;
      Shiny.setInputValue('leafletRendered',true, {priority: \"event\"});  
      myMap.on('baselayerchange', baselayerChanged)
      myMap.on('layerchange', baselayerChanged)
    }") %>%
  leaflet::setView( lng=11.970140, lat=46, zoom = 8)  %>%
  leafem::addMouseCoordinates( )


### FUNZIONI --------

updateMap<-function(session){
  
  for(i in as.character(overlayGroups.WMS.layerIDS) ){
    leaflet::clearGroup(leaflet::leafletProxy('mymap') , 
                        group=i)
  }
  
  for(i in names((overlayGroups.WMS.layerIDS)) ){
    addWMSTiles(leaflet::leafletProxy('mymap'), 
      session$userData$wms.url,
      group= overlayGroups.WMS.layerIDS[[i]],
      layers = i, 
      options = WMSTileOptions( format = "image/png", transparent = T,
                                zIndex = as.integer(i),
                                cache=as.character(Sys.time())  ),
      attribution = "Pirotti")
  }
    
}
