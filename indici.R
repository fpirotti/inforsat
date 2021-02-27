
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
getIndice <- function(session, myPolygons){
  
  area<-st_area(myPolygons$geometry)
  npixels<-as.integer(sum(area)/400)
  nPolygons<-nrow(myPolygons)
  nr<-nrow(images.lut)
  
  columns<-c("FID", "Date",  "n","mean","q10","q25" ,"q50","q75","q90", "max", "min")
  myResult<-setNames(data.frame(matrix(ncol = length(columns), nrow = nPolygons*nr)), 
                     columns)
  ## prendo uno per fare pre processing di alcune operazioni sui poligoni
  ## ed evitare che vengano rifatte nel loop
  vrt<-images.lut[1, ]$VRT
  terra.vrt<- terra::rast(vrt)
  terra.polys<-terra::vect( as_Spatial( st_transform( myPolygons, crs(terra.vrt) ) ) ) 
 
  maskPolys<-list()
  for (poly in 1:nPolygons) {
    maskPolys[[poly]]<-terra::rasterize( terra.polys[poly, ], 
                                         terra::crop(terra.vrt, terra.polys[poly, ]) )
  }
  rm(terra.vrt)
  shiny::withProgress(message =  sprintf("Analizzo: TOT=%d pixel (20m) nei poligoni", npixels), value = 0, {  
    #ciclo su ogni granule
      #terra::crop( terra::rast() ) 

    count<-1
    for (i in 1:nr) {
      date<- (images.lut[i, ]$dates)
      vrt<-images.lut[i, ]$VRT
      if( !file.exists(vrt) ){
        shinyalert("VRT non trovato", "Contatta assistenza")
        return()
      }
      terra.vrt<- terra::rast(vrt)
    
      bands.in.image<-
        sapply( images.lut[i, "bands"][[1]], '[[', "idx")
      
      
      ## subset solo bande che ci interessano
      expression.pre<- unique( c("CLD", "SNW", stringr::str_extract_all(   session$input$indici, "B[018][0-9A]")[[1]] ))
      idx<-(names(bands.in.image)%in%expression.pre)
      if(sum(idx)<4 ){
        shinyalert("Problema", "Bande per indice non trovate correttamente in VRT")
        return()
      }
      
      terra.vrt<-terra.vrt[[ as.integer( bands.in.image[expression.pre] ) ]]
      names(terra.vrt)<-expression.pre
      for (poly in 1:nPolygons) {
   
        setProgress( i/nr, detail = sprintf(" - Immagine del %s poligono n.%d", date, poly))
        
        terra.poly<-terra.polys[poly, ]
         
 
        terra.vrt.cropped<- terra::mask( terra::crop(terra.vrt, maskPolys[[poly]]), maskPolys[[poly]] )
        terra.vrt.cropped.masked<-terra.vrt.cropped
        mYexpression<-gsub("(B[018][0-9A])",   "  terra.vrt.cropped.masked[['\\1']]  " ,    
                           session$input$indici) 
        terra.vrt.cropped.index<-eval(parse(text=  mYexpression) )
         
        
        msk<-terra::rast(terra.vrt.cropped.index)
        if(session$input$mskCld){
          terra.vrt.cropped.index.msk<-terra::mask(terra.vrt.cropped.index,
                                               terra.vrt.cropped$CLD )
        } 
        if( session$input$mskSnw ){
          terra.vrt.cropped.index.msk<-terra::mask(terra.vrt.cropped.index,
                                                   terra.vrt.cropped$CLD )
        } 
        
        
        vrt.terra.df<-  terra::values(terra.vrt.cropped.index)
     
         
        
        myResult[count, ]<-
                 c(poly, as.numeric(date), (length(vrt.terra.df)), mean(vrt.terra.df,  na.rm=T), 
                  quantile(vrt.terra.df, c(0.1,0.25,0.5, 0.75,0.9), na.rm=T), 
                  min(vrt.terra.df), max(vrt.terra.df) ) 
        count<-count+1
      }#chiudo for
    }#chiudo for
  })#chiudo progress
    
  #Controllo che il db sia valorizzato. Se è vuoto do un messaggio di errore e restiusco un dataframe nullo
  if(is.data.frame(myResult) && nrow(myResult)==0){
    shinyalert::shinyalert(title = "Non ci sono dati e non posso calcolare il grafico in questa zona", type = "warning")
  }  
  return(myResult)
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


