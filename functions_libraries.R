library(sf)
library(terra)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(geojsonsf)
library(shinyjqui)
library(plotly)
require(terra)
require(leaflet)
require(leaflet.extras)


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