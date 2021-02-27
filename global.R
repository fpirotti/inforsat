source("functions_libraries.R")
#source("dbcon.R")
source("indici.R")
source("mappe.R") 
source("functions_leaflet.map.R")
source("functions_rgdal.R")
source("functions_evolut.R")
library(plotly)
## absolute path with all S2 images
image.folder<-"/archivio/esa/s2"

bands2use <-  c("AOT" ,"B02", "B03" ,"B04",                                 
    "B05" ,"B06", "B07" ,"B11",                                 
    "B12" ,"B8A",  "SCL", "TCI", "CLD", "SNW", "WVP")         

load("data.rda")
##/S2A_MSIL2A_20170613T101031_N0205_R022_T32TPR_20170613T101608.SAFE/
## list con chiave il path del folder SAFE e valore della data
images.lut<-NULL

update.Image.LUT<-function(){
  imagelist<-list.files(path= image.folder, pattern="S2[AB]_MSIL2A.*T32TQS.*\\.SAFE", recursive = F, full.names = T )  
  dates<- as.POSIXlt( substr(imagelist, 29,43), format="%Y%m%dT%H%M%OS")
  #bands<- (lapply(imagelist, get20mBandNames ) )
  dd <- as.data.frame( (data.frame(folder=imagelist, dates=as.Date(dates)  ) )  %>% arrange(dates) %>% distinct(dates,  .keep_all=TRUE) )
  rownames(dd)<-dd$folder
  
  bandslist<-list()
  dd$VRT<-NA
    for(i in 1:nrow(dd)){
      
      dd[i, "VRT"]<- file.path(dd[i, "folder"], "mapservVRT_20m.vrt")
      bandslist[[ dd[i, "folder"] ]]<-makeVRT(dd[i, "folder"])
      #dd[i, "bands"]<-I( bands)
    }
    dd$bands<- I(bandslist)
  images.lut<<-dd
  save(images.lut, file="images.lut.rda")
}

load("images.lut.rda")
#update.Image.LUT()


