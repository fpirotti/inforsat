source("functions_libraries.R")

source("functions_mapfile.R")
source("functions_evolut.R")
## absolute path with all S2 images
options(shiny.reactlog=TRUE) 
image.folder <- "/archivio/esa/s2"

bands2use <-
  c(
    "AOT" ,
  #  "B01" ,
    "B02",
    "B03" ,
    "B04",
    "B05" ,
    "B06",
    "B07" ,
  #  "B09" ,
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
#images.lut2<-images.lut
#save(images.lut2, file="images.lut2.rda")
#images.lut2<-images.lut
##/S2A_MSIL2A_20170613T101031_N0205_R022_T32TPR_20170613T101608.SAFE/
## list con chiave il path del folder SAFE e valore della data
#images.lut<-NULL

tiles.geom <- st_read("tilesS2.gpkg", query = sprintf("SELECT * FROM \"tilesS2\" WHERE name in ('%s')", paste0(collapse = "','", levels(images.lut$tile) )  )  )

update.Image.LUT <- function(verbose = F) {
  imagelist <-
    list.files(
      path = image.folder,
      pattern = "S2[AB]_MSIL2A.*.*\\.SAFE",
      recursive = F,
      full.names = T
    )
  ## naming convention changed so we find a robust finder
  initData <- function(){
    folders <<- c()
    folder.sizes <<- c()
    dates <<- c()
    tiles <<- c()
    VRTs <<- c()
    bandslist <<- list()
  }
  
  finalizeData <- function(){
    imagelist2  <- data.frame(
      "date" = as.POSIXct(dates, origin=lubridate::origin),
      "tile" = factor(tiles ),
      "folder" = folders,
      "folder.size" =  folder.sizes,
      "VRT"  = VRTs
    ) 
    imagelist2$bands = I(bandslist)
 
    if( !exists("images.lut") || is.null(images.lut) ||  nrow(images.lut)==0 ){
      images.lut <<- imagelist2    
    } else {
      images.lut <<- rbind(images.lut, imagelist2)
    }
    
    save(images.lut, file = "images.lut.rda")
  }
  
  
  initData()
  pb <- progress_bar$new(total = length(imagelist))
  cc<-0 
  #registerDoParallel(5) 
  ff <- foreach (ii=1:length(imagelist)) %do%   {
    pb$tick() 
    x <- imagelist[[ii]]
    #if(length(folders)>3) break
    ## skip if exists
    
    
    folder <- x
    folder.name <- tools::file_path_sans_ext(basename(x))
    x <- strsplit(folder.name, split = "_")[[1]]

    if (is.element(folder, images.lut$folder) ) {
      if(verbose) message( sprintf("\n%s esiste!! .... skipping ...\n", folder.name))
      return(NULL)
    }

    
    if(verbose) message( sprintf("\n ---Faccio %s--\n", folder.name))
    ww <- which(nchar(x) == 15)
    if (length(ww) < 1) {
      warning( sprintf("\nLength of file bit does not match date, check folder %s!! .... skipping ...\n", folder))
      return(NULL)
    }
    data <- as.POSIXct(x[[ww[[1]]]], format = "%Y%m%dT%H%M%OS")
    ww <- which(grepl("^T[A-Z0-9]{5}" , x))
    if (length(ww) != 1) {
      warning( sprintf("\nLength of TILE part in folder name does not match, check folder %s!! .... skipping ...\n", folder))
      return(NULL)
    }
    tile <-  substr( x[[ww[[1]]]], 2, 9)
    if (nchar(tile) != 5) {
      warning( sprintf("\nLength of TILE nchar in folder name does not match, check folder %s!! .... skipping ...\n", folder))
      return(NULL)
    }
    
    size <- as.integer(strsplit(split = "\t", system(sprintf('du -ms %s', folder), intern = T))[[1]][[1]] )
    
   
 
    vrt<- file.path(folder, "mapservVRT_20m.vrt")
    bandslist[[folder]]  <- makeVRT(folder, verbose = verbose)
    
    folder.sizes <- c(folder.sizes,  size ) 
    VRTs <- c(VRTs, vrt)
    tiles <- c(tiles, tile) 
    dates <- c(dates, data)
    folders <- c(folders, folder)
    cc <- cc + 1 
    if(cc == 3){
      cc<-0 
      finalizeData()
      initData() 
      message("Writing table")
    }
    return(NULL)
  }  
   
  pb$terminate() 
  finalizeData()  
  images.lut.tmp <- images.lut %>% arrange( date, tile, desc(folder.size) ) %>%   distinct(date, tile,  .keep_all = TRUE)
  todelete <- setdiff(images.lut$folder, images.lut.tmp$folder )
  if(length(todelete)>0 && length(which(nchar(todelete)>50))==length(todelete)) {
    unlink(todelete, recursive = T)
  } 
  if(length(todelete)>0){
    warning("Could not delete directories! ", todelete)
  }
  images.lut <<- images.lut.tmp
  save(images.lut, file = "images.lut.rda")
  
}



source("functions_leaflet.map.R")

