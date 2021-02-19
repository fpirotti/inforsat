contatorePath<-"contatore.rds"

updateContatore<-function(tipo){
  contatore<-readRDS(file = contatorePath)
  contatore[[tipo]]<-contatore[[tipo]]+1
  saveRDS(contatore, file = contatorePath)
  write(
    paste("data",Sys.time(),
          ", accessi",getContatore("accessi"),
          ", mappa: ",getContatore("mappa"), 
          ", grafico: ",getContatore("grafico"), 
          ", downdati: ",getContatore("downdati"),  
          ", downgrafico: ",getContatore("downgrafico")
          ),
    file='contatore.txt',  append = TRUE)
  rm(contatore)
}

getContatore<-function(tipo){
  contatore<-readRDS(file = contatorePath)
  valore<-contatore[[tipo]]
  rm(contatore)
  return(valore)
}

getContatoreTot<-function(){
  contatore<-readRDS(file = contatorePath)
  valore<-contatore
  rm(contatore)
  return(valore)
}

setContatore<-function(tipo, valore){
  contatore<-readRDS(file = contatorePath)
  contatore[[tipo]]<-valore
  saveRDS(contatore, file = contatorePath)
  rm(contatore)
}

createContatore<-function(){
  contatore <- data.frame("accessi"=100,"mappa"=100,"grafico"=100,"downdati"=100,"downgrafico"=100 )
  ## PIROTTI
  #saveRDS(contatore, file = "/var/log/shiny-server/contatore.rds")
  file <- "contatore.rds"
  saveRDS(contatore, file = file)
  Sys.chmod(file, "777", use_umask = FALSE)
  #rm(contatore) NON SERVE CREDO LO SCOPE SIA DENTRO LA FUNZIONE
}

#
#Example
#

#createContatore()

#updateContatore("accessi")

#setContatore("accessi", 103)

#getContatoreTot()
#getContatore("accessi")
#getContatore("mappa")
#getContatore("grafico")
#getContatore("downdati")
#getContatore("downgrafico")