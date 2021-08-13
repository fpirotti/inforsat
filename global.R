source("functions_libraries.R")

source("functions_leaflet.map.R")
source("functions_mapfile.R")
source("functions_evolut.R")
library(plotly)
library(progress)
## absolute path with all S2 images
image.folder <- "/archivio/esa/s2"

bands2use <-
  c(
    "AOT" ,
    "B02",
    "B03" ,
    "B04",
    "B05" ,
    "B06",
    "B07" ,
    "B11",
    "B12" ,
    "B8A",
    "SCL",
    "TCI",
    "CLD",
    "SNW",
    "WVP"
  )

#load("data.rda")
load("images.lut.rda")

##/S2A_MSIL2A_20170613T101031_N0205_R022_T32TPR_20170613T101608.SAFE/
## list con chiave il path del folder SAFE e valore della data
#images.lut<-NULL

update.Image.LUT <- function(verbose = F) {
  imagelist <-
    list.files(
      path = image.folder,
      pattern = "S2[AB]_MSIL2A.*.*\\.SAFE",
      recursive = F,
      full.names = T
    )
  ## naming convention changed so we find a robust finder
  folders <- c()
  folder.sizes <- c()
  dates <- c()
  tiles <- c()
  VRTs <- c()
  bandslist <- list()
  
  pb <- progress_bar$new(total = length(imagelist))
  
  for (x in imagelist) {
    pb$tick() 
    if(length(folders)>3) break
    ## skip if exists

    folder <- x
    folder.name <- tools::file_path_sans_ext(basename(x))
    x <- strsplit(folder.name, split = "_")[[1]]
    
    if (is.element(folder, images.lut$folder) ) {
      if(verbose) warning( sprintf(" %s esiste!! .... skipping ...\n", folder.name))
      next
    }
    ww <- which(nchar(x) == 15)
    if (length(ww) < 1) {
      warning( sprintf("Length of file bit does not match date, check folder %s!! .... skipping ...", folder))
      next
    }
    data <- as.POSIXct(x[[ww[[1]]]], format = "%Y%m%dT%H%M%OS")
    ww <- which(grepl("^T[A-Z0-9]{5}" , x))
    if (length(ww) != 1) {
      warning( sprintf("Length of TILE part in folder name does not match, check folder %s!! .... skipping ...", folder))
      next
    }
    tile <-  x[[ww[[1]]]]
    if (length(ww) != 1) {
      warning( sprintf("Length of TILE part in folder name does not match, check folder %s!! .... skipping ...", folder))
      next
    }
    
    size <- as.integer(strsplit(split = "\t", system(sprintf('du -ms %s', folder), intern = T))[[1]][[1]] )
    ints <- intersect(which(data == dates), which(tile==tiles))
    
    if( length(ints)!=0 ){
      ## duplicated! we keep the old one only if the size is larger in the older one 
      warning("Duplicated %s %s")
      if(size < folder.sizes[[ints]]) {
        next
      } else {
          bands   <- bands[-ints]   
          VRTs    <- VRTs[-ints]   
          tiles   <- tiles[-ints]   
          dates   <- dates[-ints]   
          folders <- folders[-ints]         
      } 
    }
    
    vrt<- file.path(folder, "mapservVRT_20m.vrt")
    bandslist[[folder]]  <- makeVRT(folder, verbose = verbose)
    
    folder.sizes <- c(folder.sizes,  size )
   # bands <- c(bands,  bandslist  )
    VRTs <- c(VRTs, vrt)
    tiles <- c(tiles, tile) 
    dates <- c(dates, data)
    folders <- c(folders, folder)
     
  } 
  imagelist2  <- data.frame(
    "date" = as.POSIXct(dates, origin=lubridate::origin),
    "tile" = factor(tiles),
    "folder" = folders,
    "folder.size" =  folder.sizes,
    "VRT"  = VRTs
  )
  
  imagelist2$bands = I(bandslist)
  pb$terminate() 
  
  if( !exists("images.lut") || is.null(images.lut) ||  nrow(images.lut)==0 ){
    images.lut <<- imagelist2    
  } else {
    images.lut <<- rbind(images.lut, imagelist2)
  }

  save(images.lut, file = "images.lut.rda")
}

update.Image.LUT(T)
