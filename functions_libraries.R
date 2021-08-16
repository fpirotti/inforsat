library(sf)
library(terra)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(geojsonsf)
library(shinyjqui)
library(plotly)
require(terra)
require(leaflet)
require(leaflet.extras)
require(shinyWidgets) 
library(progress)
library(foreach)
library(doParallel)
library(sf)
library(shinyalert) 
library(waiter)
library(circlize)

radio2expression<- list( "NDVI" = "(B08-B04)/(B08+B04)", 
                         "RGI" = "B04/B03", 
                         "NDMI" = "(B08-B11)/(B08+B11)" )


## nome layer e ID in WMS mapserver
## NB versione con cloud mask= +1 e con snow mask +2 entrambi +3
satLayers<-list("Sentinel-2 Color Composite RGB"=c("100", ""), 
                "Sentinel-2 Color Composite NIR"=c("200",""),
                "Vegetatino INDEX"=c("300", "Indice vegetazionale scelto"),
                "Sentinel-2 Scene Classification"=c("300", "") )


processingComposite<-list(
  "Colori Reali" = paste("B04","B03","B02"),  
  "NIR1 (8,4,3)" = paste("B8A","B04","B03"),
  "NIR2 (7,4,3)" = paste("B07","B04","B03"),
  "NIR3 (6,4,3)" = paste("B06","B04","B03"),
  "NIR4 (5,4,3)" = paste("B05","B04","B03"),
  "Urban (12,11,4)" = paste("B12","B11", "B04"),
  "Agriculture (11,8,2)" = paste("B11","B8A","B02"),
  "Atmospheric penetration (12,11,8)" = paste("B12","B11","B8A"),
  "Vegetazione sana (8,11,2)" = paste("B8A","B11","B02"),
  "Suolo/Acqua (8,11,4)" = paste("B8A","B11","B04"),
  "Colori naturali compensati con atmosfera (12,8,3)" = paste("B12","B8A","B03"),
  "SWIR (12,8,4)" = paste("B12","B8A","B04")
)
processingMasking<-data.frame(
  a=c("PROCESSING \"LUT_4=0:255,1:255,2:255,3:255,4:255,5:255,6:255,7:255,8:255,9:255,10:255,11:255\"" ,  
      "PROCESSING \"LUT_4=0:0,1:0,2:0,3:0,4:255,5:255,6:255,7:0,8:0,9:0,10:0,11:255\"" ),
  b=c("PROCESSING \"LUT_4=0:0,1:0,2:0,3:255,4:255,5:255,6:255,7:255,8:255,9:255,10:255,11:0\"",
      "PROCESSING \"LUT_4=0:0,1:0,2:0,3:0,4:255,5:255,6:255,7:255,8:0,9:0,10:0,11:0\"")
)

pathsTemp<-"/archivio/tmp/ms_tmp"