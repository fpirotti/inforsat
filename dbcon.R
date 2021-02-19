library(RPostgreSQL)
library(sf)

library(rgdal)
library(sp)
library(ggplot2)

library(raster)
library(rpostgis)
library(postGIStools)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(leaflet.opacity)
library(leafem)
library(RColorBrewer)
library(shinyalert)
library(shinycssloaders)
library(png)

library(rgeos)
library(stringr)
library(shinyjs)
library(mapview)





#this completes installing packages

#connessione PostGres
#now start creating connection

myconnection<-function(){
  #connessione funzionanate a postGIS
 dbname = "esa"
 host = "localhost"
 user = "marco.piragnolo"
 pass = "piragnolo"
 name = "spatial_ref_sys" # Postgis table
 con<-dbConnect(dbDriver("PostgreSQL"), dbname=dbname, host=host, port=5432, user=user , password=pass)
 return(con)
 }
#this completes creating connection

closeconnection<-function(connection){
  #disconnetto
  connessione<-connection
  RPostgreSQL::dbDisconnect(connessione)
  DBI::dbDisconnect(connessione)
  #sapply(dbListConnections(PostgreSQL ()), dbDisconnect) 
}


#trycatch sulle immagini
checkTC<-function(code){
  result <- tryCatch({
     code
  }, warning = function(w) {
    print(w)
    #shinyalert(title = "L'area è troppo piccola, fai zoom out", type = "warning")
    #assegno un risultato numerico al posto del raster
    sapply(dbListConnections(PostgreSQL ()), dbDisconnect) 
    result<-1
  }, error = function(e) {
    #assegno un risultato numerico al posto del raster
    sapply(dbListConnections(PostgreSQL ()), dbDisconnect) 
    result<-1
  }, finally = {
   
  })
  
  return(result)
}
