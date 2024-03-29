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
library(crosstalk)

radio2expression<- list( "ARVI2 - Atmospherically Resistant Vegetation Index 2" = "-0.18+1.17*(B08-B05)/(B08+B05)", 
                         "BWDRVI - Blue-wide dynamic range vegetation index" = "(0.1*B08-B01)/(0.1*B08+B01)", 
                         "EVI - Enhanced Vegetation Index " = "2.4*(B08-B05)/(B08+6*B05+1)", 
                         "GVMI - Global Vegetation Moisture Index" = "(B08+0.1-B12+0.02)/(B08+0.1+B12+0.02)",   
                         "NDMI - Normalized Difference Moisture Index" = "(B08-B11)/(B08+B11)", 
                         "NDVI - Normalized Difference Vegetation Index" = "(B08-B04)/(B08+B04)", 
                         "RGI - Red Green Index" = "B04/B03")


## nome layer e ID in WMS mapserver
## NB versione con cloud mask= +1 e con snow mask +2 entrambi +3
satLayers<-list("Color Composite RGB"=c("100", ""), 
                "Color Composite NIR"=c("200",""),
                "Vegetation Index"=c("300", "Indice vegetazionale scelto"),
                "Scene Classification"=c("300", "") )


processingComposite<-list(
  "Real" = paste("B04","B03","B02"),  
  "NIR1 (8,4,3)" = paste("B8A","B04","B03"),
  "NIR2 (7,4,3)" = paste("B07","B04","B03"),
  "NIR3 (6,4,3)" = paste("B06","B04","B03"),
  "NIR4 (5,4,3)" = paste("B05","B04","B03"),
  "Urban (12,11,4)" = paste("B12","B11", "B04"),
  "Agriculture (11,8,2)" = paste("B11","B8A","B02"),
  "Atmospheric penetration (12,11,8)" = paste("B12","B11","B8A"),
  "Healthy vegetation (8,11,2)" = paste("B8A","B11","B02"),
  "Soil/Water (8,11,4)" = paste("B8A","B11","B04"),
  "Real with atmospheric correction (12,8,3)" = paste("B12","B8A","B03"),
  "SWIR (12,8,4)" = paste("B12","B8A","B04")
)
processingMasking<-data.frame(
  a=c("PROCESSING \"LUT_4=0:255,1:255,2:255,3:255,4:255,5:255,6:255,7:255,8:255,9:255,10:255,11:255\"" ,  
      "PROCESSING \"LUT_4=0:0,1:0,2:0,3:0,4:255,5:255,6:255,7:0,8:0,9:0,10:0,11:255\"" ),
  b=c("PROCESSING \"LUT_4=0:0,1:0,2:0,3:255,4:255,5:255,6:255,7:255,8:255,9:255,10:255,11:0\"",
      "PROCESSING \"LUT_4=0:0,1:0,2:0,3:0,4:255,5:255,6:255,7:255,8:0,9:0,10:0,11:0\"")
)

statsFunction <- function(x){
  return( c(mean=mean(x, na.rm=T), sd=sd(x, na.rm=T), quantile(x, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm=T)))
}

pathsTemp<-"/archivio/tmp/ms_tmp"