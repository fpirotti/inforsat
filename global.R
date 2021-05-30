source("functions_libraries.R")
#source("dbcon.R")
source("indici.R")
source("mappe.R") 
source("functions_leaflet.map.R")
 source("functions_mapfile.R")
source("functions_evolut.R" )
library(plotly)
library(progress)
## absolute path with all S2 images
image.folder<-"/archivio/esa/s2"

bands2use <-  c("AOT" ,"B02", "B03" ,"B04",                                 
    "B05" ,"B06", "B07" ,"B11",                                 
    "B12" ,"B8A",  "SCL", "TCI", "CLD", "SNW", "WVP")         

load("data.rda")
##/S2A_MSIL2A_20170613T101031_N0205_R022_T32TPR_20170613T101608.SAFE/
## list con chiave il path del folder SAFE e valore della data
#images.lut<-NULL

update.Image.LUT<-function(verbose=F){
  imagelist<-list.files(path= image.folder, pattern="S2[AB]_MSIL2A.*T32TQS.*\\.SAFE", recursive = F, full.names = T )  
  dates<- as.POSIXlt( substr(imagelist, 29,43), format="%Y%m%dT%H%M%OS")
  ##remove existing
  setdiff(imagelist, images.lut$folder)

  keep <- which(!( format(dates,"%Y-%m-%d" ) %in% as.character(images.lut$dates) ) )
  
  imagelist<- imagelist[keep]
  dates<- dates[keep]
  if(length(dates)<1){
    warning("Everything up to date")
    return(NULL)
  }
  #bands<- (lapply(imagelist, get20mBandNames ) )
  dd <- as.data.frame( (data.frame(folder=imagelist, dates=as.Date(dates)  ) )  %>% arrange(dates) %>% distinct(dates,  .keep_all=TRUE) )
  rownames(dd)<-dd$folder
  
  bandslist<-list()
  dd$VRT<-NA
  
  pb <- progress_bar$new(total = nrow(dd))
  
  for(i in 1:nrow(dd)){
    pb$tick()
    folder <- dd[i, "folder"]
    if( is.element(folder, rownames(images.lut)) ){
      next
    }
    
    dd[i, "VRT"]<- file.path(dd[i, "folder"], "mapservVRT_20m.vrt")
    bandslist[[ folder ]] <- makeVRT(dd[i, "folder"], verbose = verbose)
    #dd[i, "bands"]<-I( bands)
    #progress::progress_bar
    #pb$tick()
    }
  
  pb$terminate()
  
  dd$bands<- I(bandslist)
  images.lut.temp <- rbind(images.lut, dd)
  images.lut<<-images.lut.temp
  save(images.lut, file="images.lut.rda")
}

load("images.lut.rda")
update.Image.LUT()


