library(RCurl)
library(jsonlite)
library(httr)
source("global.R")


imagelist <-
  list.files(
    path = image.folder,
    pattern = "S2[AB]_MSIL2A.*.*\\.SAFE",
    recursive = F,
    full.names = F
  )

existing <- tools::file_path_sans_ext(basename(imagelist))
CC<-0

getImage <- function(page=1, daysback=600, url=NA){
  if(is.na(url)) {
    # query <- sprintf("https://scihub.copernicus.eu/dhus/search?format=json&q=(platformname:Sentinel-2 AND footprint:\"Intersects(POLYGON((11.5 45.5, 12 45.5, 12 46,11.5 46,11.5 45.5)))\" AND ingestiondate:[NOW-%dDAYS TO NOW] AND cloudcoverpercentage:[0 TO 5] AND producttype:S2MSI2A   )", daysback )
    
    query <- sprintf("https://scihub.copernicus.eu/dhus/search?format=json&q=(platformname:Sentinel-2 AND footprint:\"Intersects(POLYGON((10.5 46.2, 11 46.2, 11 46.7,11 46.7, 10.5 46.2)))\" AND ingestiondate:[NOW-%dDAYS TO NOW] AND cloudcoverpercentage:[0 TO 10] AND producttype:S2MSI2A   )", daysback )
    
  }  else {
    query <- url 
  }
  res <- getURL(utils::URLencode(query), userpwd="fpirotti:libero", httpauth = 1L) 
  res.list <- jsonlite::fromJSON(res)
  message("\nTotal n. of items: ", res.list$feed$`opensearch:totalResults`)
  nres <- length(res.list$feed$entry$title)

  dups <- which(res.list$feed$entry$title %in% existing)
  if(length(dups)>0) {
    message("Duplicated: ", length(res.list$feed$entry$title[dups]) )
  }
  
  remaining <- setdiff(1:nres, dups)
  
  for(i in  remaining ) {
     
    name <- res.list$feed$entry$title[[i]]  

    lnk2download <- res.list$feed$entry$link[[i]] %>% filter(is.na(rel))
    lnk <- res.list$feed$entry$link[[i]] %>% filter(rel=="alternative")
    
    getinfo <- getURL(lnk$href, userpwd="fpirotti:libero", httpauth = 1L) 
    rxml <- xml2::as_list(xml2::read_xml(getinfo))
    rxml$entry$properties$ChildrenNumber[[1]]
    tile <- strsplit(x =  rxml$entry$properties$Name[[1]], split="_")[[1]][[6]]
    if(rxml$entry$properties$Online[[1]]=="false"){
 
      message("\nData ", rxml$entry$properties$CreationDate[[1]] ,"\t", tile, "\t not online, skipping\n")
 
      next
    } 
 
  
    outfile <- file.path(image.folder, paste0(name,".zip") )
    
    if(file.exists(outfile)){
      fsize<- file.size(outfile)
      
      if(is.na(fsize) || fsize < 100000000  ){
        message("File ", outfile, " exists but ", round(fsize/1000000, 0)," MB so removing. ")
        file.remove(outfile)
      } 
      
      outfolder <- outfile
      raster::extension(outfolder)<- "SAFE"
      if(dir.exists(outfolder)){ 
        message("\nFolder ", outfolder, " exists ... SKIPPING. ") 
        ## remove zip file in case it still exists
        file.remove(outfile)
        next
      } else {
        if(file.exists(outfile)) {
          message("\nExtracting  ", outfolder, " .... ") 
          unzip(outfile, overwrite = F, exdir = image.folder)
          file.remove(outfile)
          next
        }
      } 
    }
    

    
   message("\nDOWNLOADING ", rxml$entry$properties$CreationDate[[1]] ,"\t", tile)
     
   tryCatch( {
     GET(lnk2download$href, 
         authenticate("fpirotti", "libero"),
         write_disk(path = outfile, overwrite = T ) )
     
   }, error=function(e){
        
     print(e)
     
   }

   )
    
    if(file.exists(outfile))  {
      message("\nExtracting  ", outfile, " in ", image.folder) 
      if(file.size(outfile)<100){ 
        message("\n 0 size, skipping and removing ", outfile, " .... ") 
        Sys.sleep(3)
        file.remove(outfile) 
        next
      }
      unzip(outfile, overwrite = F, exdir = image.folder)
      file.remove(outfile) 
    }
 
  }
  
  CC<<-CC+1
  message("\nFINISHED LOOP  ",  CC, " of ",  ceiling(as.integer(res.list$feed$`opensearch:totalResults`)/10) ,  ".... ") 
  isnext <- res.list$feed$link %>% filter(rel=="next")
  if(nrow(isnext)==1){
    getImage(url=isnext$href)
  }
  
}

getImage()


update.Image.LUT(T)
