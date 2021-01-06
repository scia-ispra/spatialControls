#revisione del 9 Novembre 2016
#
#Programma riscritto in funzione degli oggetti ClimateData
#Riviste e migliorate le funzioni corroboration e regression per la temperatura
#NESSUNA MODIFICA INVECE PER la funzione corroboration della precipitazione: da fare<----
#Il file creaDirectorySpatial.R è stato rinominato utilitySpatial.R
#Sono state modificate le sottodirectory per la memorizzazione dei dati
#Prevista anche la possibilità di fare il debug dei program generando degli errori casuali
rm(list=objects())
library("readr")
library("magrittr")
library("dplyr")
library("stringr")
library("tidyr")
library("xts")
library("lubridate")
library("codetools")
library("foreach")
library("doParallel")
library("Rcpp")
source("ClimateData.R")
source("utilitySpatial.R",local=FALSE)
source("elaboraSpatialP.R",local=FALSE)
options(warn=2,error=recover,stringsAsFacotrs=FALSE)
set.seed(2)
#registerDoParallel(2)
#crea directory di output
list("debug","data_con_flag","more_output","grafici")->listaSubDirectory
creaDirOut(dir.risultati=DIR.RISULTATI,lista.directory=listaSubDirectory,cancella=TRUE)

#aggiustiamo i nomi delle directory con gli slash Se esiste la variabile
#"DIR.RISULTATI" le directory per memorizzare tutti i risultati saranno sotto directory
#della directoy in "DIR.RISULTATI"
#In questo modo possiamo semplificare la scrittura dei risultati, dei grafici senza preoccuparci
#se una directory ha slash o meno
lapply(listaSubDirectory,FUN=function(nomeDir){
  
  #get(nomeDir,envir = parent.env(environment()))->nuovoNomeDir
  if(exists(x="DIR.RISULTATI",envir=globalenv())) nomeDir<-paste0(DIR.RISULTATI,"/",nomeDir,"/")
  str_replace_all(nomeDir,"\\//","/")
})->nlistaSubDirectory
names(nlistaSubDirectory)<-listaSubDirectory

rm(listaSubDirectory)

#I nomi dei file di input servono a generare la lista dei codici delle stazioni
#da processare. Il formato deve essere: codice.txt
str_replace_all(list.files(pattern="^[a-zA-Z0-9].+\\.txt"),".txt","")->codici

stopifnot(length(codici)!=0)

#registerDoParallel(2)

system.time(
    elaboraSpatial(codici,dataConFlag=nlistaSubDirectory[["data_con_flag"]],
                   extraOut =nlistaSubDirectory[["more_output"]],
                   debug=nlistaSubDirectory[["debug"]],
                   grafici=nlistaSubDirectory[["grafici"]])
)->temp.trascorso

print(temp.trascorso)
