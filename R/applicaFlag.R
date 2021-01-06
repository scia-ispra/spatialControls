#23 febbraio 2018: cerca i file prodotti dai controlli spaziali (n+numerocodice+.txt)
#e applica i codici per invalidare i dati invalidi trovati dai controlli spazili (per la temperatura)
#I file originali con i flag vengono salvati in una sotto-directory
#6 gennaio 2021: eliminata dipendenza da lazyeval
rm(list=objects())
library("tidyverse")
library("purrr")
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
    
    which(x[[corName]]==1)->rigaCor
    if(length(rigaCor)) x[rigaCor,param]<-NA
    
    which(x[[regName]]==1)->rigaReg
    if(length(rigaReg)) x[rigaReg,param]<-NA
    
    x
    
  }
  
}#fine invalida flag

invalidaTmax<-invalidaFlag(param="tmax")
invalidaTmin<-invalidaFlag(param="tmin")

applicaFlag<-function(nomeFile)
{
  
  read_delim(nomeFile,delim=",",col_names=TRUE,
             col_types =cols(year=col_integer(),
                             month=col_integer(),
                             day=col_integer(),
                             .default = col_double()))->dati
  
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
