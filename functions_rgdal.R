require(terra)

createIndexFile<-function(session){
  
  browser()
  
  i <- which(images.lut$dates == session$input$dayselected)
  
  date<- (images.lut[i, ]$dates)
  bands<- (images.lut[i, ]$bands)[[1]]
  vrt<-file.path(images.lut[i, ]$folder, "mapservVRT_20m.vrt")
  if( !file.exists(vrt) ){
    shinyalert("VRT non trovato", "Contatta assistenza")
    return()
  }
   

    
  expression.pre<- unique( stringr::str_extract_all(   session$input$indici , "B[018][0-9A]")[[1]] )
  
  idx<-(bands%in%expression.pre)
  if(sum(idx)<2 || !is.element("SCL", names(terra.vrt))){
    shinyalert("Problema", "Bande per indice non trovate correttamente in VRT")
    return()
  }
  terra.vrt<-terra.vrt[c(expression.pre, "SCL")]
  
  
  gdalwarp(inRaster1, indexfile)
  
  
}