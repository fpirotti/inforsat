
#creo una lista di indici da calcolare sul database
calcoloIndice<-list()
calcoloIndice[1]<-"ST_MapAlgebra(ST_Union(rast), 4, ST_Union(rast), 3,  '( ([rast1]-[rast2])/([rast1]+[rast2]) )::float', '32BF')"
calcoloIndice[2]<-"ST_MapAlgebra(ST_Union(rast), 3, ST_Union(rast), 2,  '( ([rast1])/([rast2]) )::float', '32BF')"
calcoloIndice[3]<-"ST_MapAlgebra(ST_Union(rast), 3, ST_Union(rast), 5,  '( ([rast1]-[rast2])/([rast1]+[rast2]) )::float', '32BF')"
calcoloIndice[4]<-"ST_MapAlgebra(ST_Union(rast,4), 1, '32BF','[rast]*0.0001')"

#valori limite per il grafico
limiteY<-list()
limiteY[1]<-"-1"
limiteY[2]<-"-1"
limiteY[3]<-"-1"
limiteY[4]<-"0"

#risoluzione spaziale delle bande da cercare nel database
spatialResolution<-c("_10m","_10m","_20m","_10m", "_scl")

#analisi da compiere
nomeAnalisi<-c("NDVI","RGI","NDMI", "NIR", "MASCHERA NUVOLE")


#
#
# FUNZIONE CHE CALCOLA L'INDICE
#
#
getIndice <- function(polygonString, inizio, fine, nomeIndice){
  
  poligono<-isolate(polygonString)
  start<-isolate(inizio)
  end<-isolate(fine)
  
  conInfo<-myconnection()
  
  #qui estraggo uuid delle immagini in base alla data
  uuidDate<-paste(" SELECT beginposition, uuid FROM esa_catalogue.s2_loaded_mat where beginposition>='",start,"' AND endposition<='",end,"' AND processinglevel='Level-2A'  AND relativeorbitnumber=22 AND cloudcoverpercentage<20 AND identifier ~ 'T32TQS' ORDER BY beginposition ASC");
  uuidResDate<-DBI::dbGetQuery(conInfo, uuidDate)  
  
  #bande per ciclare il loop
  uuidBand<-as.vector(uuidResDate[,2])
  
  #leggo WKT
  d32<-rgeos::readWKT(paste("POLYGON((",poligono,"))"),p4s ="+init=epsg:4326")
  d32<-sp::spTransform(d32, sp::CRS("+init=epsg:32632"))
  
  #bande per ciclare la mappa
  uuidBandPG<-as.vector(lapply(uuidBand,paste0,spatialResolution[as.numeric(nomeIndice)]))
  
  myResult<-setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("count", "sum", "mean", "stddev", "min", "max","uuid"))
  
  shiny::withProgress(message = 'Making plot', value = 0, {  
    #ciclo su ogni granule
    for (i in 1:length(uuidBand[])) {
      
      switch(nomeIndice, 
             '1'={
               
               schema<-"s2"
               table<-as.character(uuidBandPG[i])
               stringa<-c(schema,table)
               
               NIR<-checkTC(rpostgis::pgGetRast(conInfo, as.character(stringa), rast = "rast", bands=4, boundary=c(bbox(d32)[2,2], bbox(d32)[2,1],bbox(d32)[1,2],bbox(d32)[1,1]) ) 
               )
               
               
               R<-checkTC(rpostgis::pgGetRast(conInfo, as.character(stringa), rast = "rast", bands=3, boundary=c(bbox(d32)[2,2], bbox(d32)[2,1],bbox(d32)[1,2],bbox(d32)[1,1]) ) 
               )
               
               myMap2<-(NIR-R)/(NIR+R)
               
               #controllo che sia un raster
               if(is(myMap2,"RasterLayer")){
                 myMap3<-maskRasterGrafico(uuidBand[i], myMap2, d32)
                 
                 tot<- ncell(myMap3)
                 somma<-cellStats(myMap3, stat='sum',na.rm=TRUE)
                 media<-cellStats(myMap3, stat='mean',na.rm=TRUE)
                 deviaz<-cellStats(myMap3, stat='sd',na.rm=TRUE)
                 minimo<-cellStats(myMap3, stat='min',na.rm=TRUE)
                 massimo<-cellStats(myMap3, stat='max',na.rm=TRUE)
                 
                 myResult[i, "uuid"]<-uuidBand[i];
                 
                 myResult[i, "count"]<-tot;
                 
                 myResult[i, "sum"]<-somma;
                 
                 myResult[i, "mean"]<-media;
                 
                 myResult[i, "stddev"]<-deviaz;
                 
                 myResult[i, "min"]<-minimo;
                 
                 myResult[i, "max"]<-massimo;
               }#chiudo if
             },
             '2'={                  
               schema<-"s2"
               table<-as.character(uuidBandPG[i])
               stringa<-c(schema,table)
               
               R<-checkTC(rpostgis::pgGetRast(conInfo, as.character(stringa), rast = "rast", bands =3, boundary=c(bbox(d32)[2,2], bbox(d32)[2,1],bbox(d32)[1,2],bbox(d32)[1,1]) ) 
               )
               
               G<-checkTC(rpostgis::pgGetRast(conInfo, as.character(stringa), rast = "rast", bands=2, boundary=c(bbox(d32)[2,2], bbox(d32)[2,1],bbox(d32)[1,2],bbox(d32)[1,1]) ) 
               )
               
               myMap2<-(R/G)
               
               #controllo che sia un raster
               if(is(myMap2,"RasterLayer")){
                 myMap3<-maskRasterGrafico(uuidBand[i], myMap2, d32)
                 
                 #controllo che sia un raster
                 tot<- ncell(myMap2)
                 somma<-cellStats(myMap3, stat='sum',na.rm=TRUE)
                 media<-cellStats(myMap3, stat='mean',na.rm=TRUE)
                 deviaz<-cellStats(myMap3, stat='sd',na.rm=TRUE)
                 minimo<-cellStats(myMap3, stat='min',na.rm=TRUE)
                 massimo<-cellStats(myMap3, stat='max',na.rm=TRUE)
                 
                 myResult[i, "uuid"]<-uuidBand[i];
                 
                 myResult[i, "count"]<-tot;
                 
                 myResult[i, "sum"]<-somma;
                 
                 myResult[i, "mean"]<-media;
                 
                 myResult[i, "stddev"]<-deviaz;
                 
                 myResult[i, "min"]<-minimo;
                 
                 myResult[i, "max"]<-massimo;
               }#chiudo il controllo del raster
             },
             '3'={              
               schema<-"s2"
               
               #la banda 11 è a 20m
               table11<-as.character(uuidBandPG[i])
               stringa11<-c(schema,table11)
               
               #la band a 8 è a 10m
               table8<-str_replace(table11,"_20m","_10m")
               stringa8<-c(schema,table8)
               
               NIR8<-checkTC(rpostgis::pgGetRast(conInfo, as.character(stringa8), rast = "rast", bands=4, boundary=c(bbox(d32)[2,2], bbox(d32)[2,1],bbox(d32)[1,2],bbox(d32)[1,1]) ) 
               )
               
               NIR11<-checkTC(rpostgis::pgGetRast(conInfo, as.character(stringa11), rast = "rast", bands=5, boundary=c(bbox(d32)[2,2], bbox(d32)[2,1],bbox(d32)[1,2],bbox(d32)[1,1]) )
               )
               
               NIR11<-checkTC(resample(NIR11,NIR8,method="ngb")
               )
               
               myMap2<-(NIR8-NIR11)/(NIR8+NIR11)
               
               #controllo che sia un raster
               if(is(myMap2,"RasterLayer")){
                 myMap3<-maskRasterGrafico(uuidBand[i], myMap2, d32)
                 
                 
                 #controllo che sia un raster
                 tot<- ncell(myMap2)
                 somma<-cellStats(myMap3, stat='sum',na.rm=TRUE)
                 media<-cellStats(myMap3, stat='mean',na.rm=TRUE)
                 deviaz<-cellStats(myMap3, stat='sd',na.rm=TRUE)
                 minimo<-cellStats(myMap3, stat='min',na.rm=TRUE)
                 massimo<-cellStats(myMap3, stat='max',na.rm=TRUE)
                 
                 myResult[i, "uuid"]<-uuidBand[i];
                 
                 myResult[i, "count"]<-tot;
                 
                 myResult[i, "sum"]<-somma;
                 
                 myResult[i, "mean"]<-media;
                 
                 myResult[i, "stddev"]<-deviaz;
                 
                 myResult[i, "min"]<-minimo;
                 
                 myResult[i, "max"]<-massimo;
               }#chiudo controllo raster
             },
             '4'={                  
               schema<-"s2"
               table<-as.character(uuidBandPG[i])
               stringa<-c(schema,table)
               
               NIR<-checkTC(rpostgis::pgGetRast(conInfo, as.character(stringa), rast = "rast", bands=4, boundary=c(bbox(d32)[2,2], bbox(d32)[2,1],bbox(d32)[1,2],bbox(d32)[1,1]) ) 
               )
               
               #controllo che sia un raster
               if(is(NIR,"RasterLayer")){
                 myMap2<-(NIR/10000)
                 
                 myMap3<-maskRasterGrafico(uuidBand[i], myMap2, d32)
                 
                 tot<- ncell(myMap3)
                 somma<-cellStats(myMap3, stat='sum',na.rm=TRUE)
                 media<-cellStats(myMap3, stat='mean',na.rm=TRUE)
                 deviaz<-cellStats(myMap3, stat='sd',na.rm=TRUE)
                 minimo<-cellStats(myMap3, stat='min',na.rm=TRUE)
                 massimo<-cellStats(myMap3, stat='max',na.rm=TRUE)
                 
                 myResult[i, "uuid"]<-uuidBand[i];
                 
                 myResult[i, "count"]<-tot;
                 
                 myResult[i, "sum"]<-somma;
                 
                 myResult[i, "mean"]<-media;
                 
                 myResult[i, "stddev"]<-deviaz;
                 
                 myResult[i, "min"]<-minimo;
                 
                 myResult[i, "max"]<-massimo;
               }#chiudo controll raster
             },
             stop("Enter something that switches me!"))   
      incProgress(1/length(uuidBand[]), detail = paste("Doing part", i))
    }#chiudo for
  })#chiudo progress
  
  #chiudo la connessione
  closeconnection(conInfo)
  
  #Controllo che il db sia valorizzato. Se è vuoto do un messaggio di errore e restiusco un dataframe nullo
  if(is.data.frame(myResult) && nrow(myResult)==0){
    shinyalert::shinyalert(title = "Non ci sono dati e non posso calcolare il grafico in questa zona", type = "warning")
  }
  
  #unisco i valori ottenuti alla data.
  mergeDF<- merge(uuidResDate,myResult,by="uuid", all = TRUE) 
  
  #ordino dopo il join
  finalResult<-mergeDF[order(mergeDF$beginposition),]
  
  #rimuovo il dataframe inutile
  rm(mergeDF)
  rm(myResult)
  rm(uuidResDate)
  
  #arrodtondo
  finalResult$mean<-round(as.numeric((finalResult$mean)),2)
  finalResult$stddev<-round(as.numeric((finalResult$stddev)),2)
  
  return(finalResult)
}  

#
#
# FUNZIONE CREA IL GRAFICO
#
#
getGrafico <- function(risultatoDF,nomeIndice){   
  finalResult<-risultatoDF
  
  pd <- position_dodge(0.1) # move them .05 to the left and right
  
  myplot<-ggplot2::ggplot(finalResult, aes(x=as.Date(beginposition), y=mean, group = 1)) + 
    ggplot2::geom_errorbar(position=pd,aes(ymin=mean-stddev, ymax=mean+stddev), colour="black", width=.1) +
    ggplot2::geom_line() +
    ggplot2::geom_path(position=pd,size = 1.5,colour="#CE2E32")+
    ggplot2::geom_point(position=pd,size=1.5, shape=21, fill="white")+
    #fill : the fill color for the rectangle
    #colour, color : border color
    #size : border size
    ggplot2::theme(panel.background = element_rect(fill="#B0A1A3", 
                                                   colour="#B49798", 
                                                   size=0.25, 
                                                   linetype="solid", 
                                                   color="#F9F1E3"))+
    
    ggplot2::xlab('Date')+
    ggplot2::scale_x_date(date_labels = "%b %d %Y")+
    ggplot2::ylab('Value')+
    ggplot2::ggtitle(nomeAnalisi[as.numeric(nomeIndice)]) +
    ggplot2::scale_y_continuous(limits = c(as.numeric(limiteY[as.numeric(nomeIndice)]), max(finalResult$mean)+0.1))+
    ggplot2::expand_limits(y=as.numeric(limiteY[as.numeric(nomeIndice)])) 
  
  
  
  #setMyPlot(myplotR,myplot)
  #disconnetto
  
  return(myplot)
}


