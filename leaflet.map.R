require(leaflet)
require(leaflet.extras)

 
overlayGroups.WMS.layerIDS <- list("100"="S2 Color Composite",
                                   "400"="S2 Scene Classification", 
                                   "500"="S2 CLOUD MSK", 
                                   "600"="S2 SNOW MSK", 
                                   "300"="INDICE")  

as.character(overlayGroups.WMS.layerIDS)
###  OGGETTO LEAFLET DA METTERE SU SERVER.R ------
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
    position =("topright"),
    baseGroups = c("OpenStreetMap","ESRI","Ortofoto 2015 Regione Veneto"),
    overlayGroups = as.character(overlayGroups.WMS.layerIDS),
    layersControlOptions(autoZIndex = F) 
  ) %>% 
  
  hideGroup( as.character(overlayGroups.WMS.layerIDS) )   %>% 
  
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
  
  #enableMeasurePath()%>%
  
  leaflet.extras::addMeasurePathToolbar()%>%
  
  leaflet::addScaleBar("bottomleft")%>%
  addMiniMap(tiles = "OpenStreetMap", toggleDisplay = TRUE,
             position = "bottomleft") %>%
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }") %>%
  leaflet::setView( lng=11.970140, lat=46.349963, zoom = 12)  



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
                                cache=as.character(Sys.time())  ),
      attribution = "Pirotti")
  }
    
}
