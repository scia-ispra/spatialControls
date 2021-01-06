#revisione del 6 aprile 2020: sostituito il codice che utilizza mutate_
#23 febbraio 2018: cerca i file prodotti dai controlli spaziali (n+numerocodice+.txt)
#e applica i codici per invalidare i dati invalidi trovati dai controlli spazili (per la temperatura)
#I file originali con i flag vengono salvati in una sotto-directory
rm(list=objects())
library("tidyverse")
library("purrr")
library("lazyeval")
library("stringr")
options(error=recover,warn=2)

DIRECTORY_OUT<-"data_con_flag"

if(!dir.exists(DIRECTORY_OUT)){

  tryCatch({
    dir.create(DIRECTORY_OUT)
  },error=function(e){
    stop(sprintf("Non posso creare la directory %s",DIRECTORY_OUT))
  })
  
}
  
invalidaFlag<-function(param){
  
  paste0("corroboration.",param)->corName
  paste0("regression.",param)->regName
  
  function(x){
    
    #mutate_(x,.dots=setNames(list(interp(~(ifelse(y!=0 | z!=0,NA,x)),.values=list(x=as.name(param),y=as.name(corName),z=as.name(regName)))),param))
    names(x)<-stringr::str_replace(names(x),param,"XXX")
    mutate(x,XXX=ifelse((regression.XXX | corroboration.XXX) !=0,NA,XXX))->x
    names(x)<-stringr::str_replace(names(x),"XXX",param)
    x

  }
  
}#fine invalida flag

invalidaTmax<-invalidaFlag(param="tmax")
invalidaTmin<-invalidaFlag(param="tmin")

applicaFlag<-function(nomeFile)
{
  
  tryCatch({
    suppressWarnings(read_delim(nomeFile,delim=",",col_names=TRUE,col_types ="iiiddddddd"))
  },error=function(e){
    stop(sprintf("Errore lettura file %s",nomeFile))
  })->dati
  
  dati %>% 
    invalidaTmax %>%
      invalidaTmin %>%
        select(-contains(".tmax"),-contains(".tmin")) %>%
          write_delim(.,file=str_replace(nomeFile,"^n",""),delim=",",col_names=TRUE)
  
  
  system(sprintf("mv %s ./%s",nomeFile,DIRECTORY_OUT))
  
    
}#applicaFlag  


# INIZIO Programma --------------------------------------------------------

list.files(pattern="^n.+txt$")->ffile
if(!length(ffile)) stop("Non è stato trovato nessun file su cui lavorare")

purrr::map(ffile,applicaFlag)
