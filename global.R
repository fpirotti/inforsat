source("functions_rgdal.R")
source("functions_evolut.R")
source("dbcon.R")
source("indici.R")
source("mappe.R")
source("counter.R")

## absolute path with all S2 images
image.folder<-"/archivio/esa/s2"
load("data.rda")
##/S2A_MSIL2A_20170613T101031_N0205_R022_T32TPR_20170613T101608.SAFE/
## list con chiave il path del folder SAFE e valore della data
images.lut<-NULL

update.Images<-function(forceVRT=F){
  imagelist<-list.files(path= image.folder, pattern="S2[AB]_MSIL2A.*T32TQS.*\\.SAFE", recursive = F, full.names = T )  
  dates<- as.POSIXlt( substr(imagelist, 29,43), format="%Y%m%dT%H%M%OS")
  bands<-sapply(imagelist, get20mBandNames)
  images.lut<<- as.data.frame( (data.frame(folder=imagelist, dates=as.Date(dates), bands=I(bands) ) )  %>% arrange(dates) %>% distinct(dates,  .keep_all=TRUE) )
  if(forceVRT){
    for(i in imagelist){
      makeVRT(i) 
    }
  }
}

update.Images()

radio2expression<- list( "NDVI" = "(B08-B04)/(B08+B04)", "RGI" = "B04/B03", 
                         "NDMI" = "(B08-B11)/(B08+B11)" )

