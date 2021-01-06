source("spatialChecks.R",local=FALSE)

#### funzione per creare lista.limitrofe
crea.lista.limitrofe=function(codici.limitrofe=NULL,parametro)
{
  
  messaggio.errore(argomento=match.call())
  
  if(parametro=="prcp"){
    cols_only(year="i",month="i",day="i",prcp="d")->stringaFormato 
  }else if(parametro=="tmax"){
    cols_only(year="i",month="i",day="i",tmax="d")->stringaFormato    
  } else{
    cols_only(year="i",month="i",day="i",tmin="d")->stringaFormato    
  } 
  
  
  #-1 perchè la lista contiene anche la stazione target
  lapply(codici.limitrofe,FUN=function(ff){

    #leggiamo senza flag
    tryCatch({
      read_delim(file=paste0("n",ff,".txt"),delim=",",col_names=TRUE) ->tmp

      try({
        which(tmp$corroboration.prcp!=0)->index
        if(length(index)) tmp[index,]$prcp<-NA
      })
      
      try({
        which(tmp$corroboration.tmax!=0)->index
        if(length(index)) tmp[index,]$tmax<-NA
      })
      
      try({
        which(tmp$corroboration.tmin!=0)->index
        if(length(index)) tmp[index,]$tmin<-NA
      })
      
      try({
        which(tmp$regression.tmax!=0)->index
        if(length(index)) tmp[index,]$tmax<-NA
      })
      
      try({
        which(tmp$regression.tmin!=0)->index
        if(length(index)) tmp[index,]$tmin<-NA
      })
	  		  		  
      
      tmp %>%select(year,month,day,prcp,tmax,tmin)
      
    },error=function(e){
      tryCatch({
        read_delim(file=paste0(ff,".txt"),delim=",",col_names=TRUE,col_types=stringaFormato)
      },error=function(e){
        NULL
      })
    })->dati.temp
    
    #file non trovato o limitrofa con solo valori pari a NA per il parametro in esame 	  
    if(is.null(dati.temp) || all(is.na(dati.temp[,c(4)]))) return(NULL)
    
    #xts(dati.temp[,c(4)],order.by=as.Date(paste(dati.temp$year,dati.temp$month,dati.temp$day,sep="-")))
    ClimateData(x=dati.temp[,c("year","month","day",parametro)],param=parametro)

  })->listaOut
  
  names(listaOut)<-codici.limitrofe
  Filter(Negate(is.null),listaOut)
  
}#fine crea.lista.limitrofe



#scriviOut: directory dove salvare l'output che si vuole rielaborare. Non si tratta dei risultati dell'invalidazione
#ma del materiale aggiuntivo per una successiva analisi dei risultati/grafici
elaboraSpatial<-function(codici=NULL,
                         nmin.limitrofe.tmp=3,
                         nmin.limitrofe.prcp=3,
                         max.dist.prcp=10, #originale
                         max.dist.tmp=50, #originale 35
                         max.gap.height.prcp=100,
                         max.gap.height.tmp=100,
                         corroborationt=TRUE,
                         regressiont=TRUE,
                         corroborationp=FALSE,
                         dataConFlag="",
                         extraOut="",
                         debug="",
                         grafici=""){
  

  #lettura angrafica stazioni
  tryCatch({
    read_delim(file="anagrafica.csv",delim=";",col_names=TRUE)->anagrafica
  },error=function(e){
    stop("Errore file anagrafica.csv")
  })  
  
  #La quota potrebbe comparire sotto le voci Elevation (prima scelta, in quanto valore fornito dal CF) o demElevation
  #(seconda scelta, quota stimata dal dem, quindi meno precisa).

  if( (("quota" %in% names(anagrafica)) && all(is.na(anagrafica$quota))) || !("quota" %in% names(anagrafica)) ){

	if(("Elevation" %in% names(anagrafica)) && !(all(is.na(anagrafica$Elevation))) ){
		if("quota" %in% names(anagrafica)) names(anagrafica)[names(anagrafica)=="quota"]<-"old.quota"
		names(anagrafica)[names(anagrafica)=="Elevation"]<-"quota"	
	}else{

		if(("demElevation" %in% names(anagrafica)) && !(all(is.na(anagrafica$demElevation)))){
			if("quota" %in% names(anagrafica)) names(anagrafica)[names(anagrafica)=="quota"]<-"old.quota"
			names(anagrafica)[names(anagrafica)=="demElevation"]<-"quota"
		}else{
			stop("Non riesco a gestire la variabile quota in anagrafica")
		}
		
	}

  }#if su quota		

  #Qui gestiamo possibili nomi alternativi per le variabili
  mapply(FUN=function(varName,altName){
	
	  #le colonne lon lat ci sono?
	  tryCatch({
		stopifnot(varName %in% names(anagrafica))
	  },error=function(e){
		stopifnot(altName %in% names(anagrafica))
		names(anagrafica)[names(anagrafica)==altName]<-varName
		anagrafica
	  })->out

	assign("anagrafica",out,envir=parent.env(environment()))		
		
  },varName=c("idstaz","lat","lon"),altName=c("SiteID","Latitude","Longitude"),SIMPLIFY=FALSE) 		

  stopifnot(length(names(anagrafica) %in% c("idstaz","lon","lat","quota"))!=2 && nrow(anagrafica))

  #prendiamo le colonne che ci servono e ricodifichiamole, 
  #prendiamo solo le righe che corrispondono ai codici dei file da elaborare
  anagrafica$idstaz<-as.character(anagrafica$idstaz)
  anagrafica[anagrafica$idstaz %in% codici,c("idstaz","lon","lat","quota","SiteName")]->anagrafica

  #se in anagrafica.csv mancano proprio i codici delle stazioni che ci interessano
  if(!nrow(anagrafica)) stop("Nessun codice stazione di 'codici' nel file anagrafica")

	#matrice delle distanze calcolata mediante il pacchetto fields con il GREAT CIRCLE DISTANCE
	fields::rdist.earth(x1=as.matrix(anagrafica[,c("lon","lat")]),miles=FALSE)->matrice.distanze

	#nomi righe e colonne alla matrice
	colnames(matrice.distanze)<-anagrafica$idstaz
	rownames(matrice.distanze)<-anagrafica$idstaz

	lapply(codici,FUN=function(idstaz){

		messaggi(sprintf("Running SPATIAL EXTRA, station %s",idstaz))

		paste0(idstaz,".txt")->nome.file

		tryCatch({
      #leggiamo i dati senza eventuali flag
		  suppressWarnings({
  		  read_delim(file=nome.file,delim=",",col_names=TRUE,col_types=cols_only(year="i",month="i",day="i",prcp="d",tmax="d",tmin="d"))
		  })  
		},error=function(e){
		  stop(paste0("Errore file di input: ",nome.file))
		})->mdatos
		  
    nrow(mdatos)->numeroRighe
		#cerca stazioni limitrofe per id.staz. e che non hanno una quota che differisce da quella della target
		#oltre i "max.gap.height" metri. I risultati di limitrofe.trovate sono ordinate secondo le distanze, con
		#la prima riga che rappresenta la stazione target (distanza 0km) 
	
		lista.nmin.limitrofe<-list("tmax"=nmin.limitrofe.tmp,"tmin"=nmin.limitrofe.tmp,"prcp"=nmin.limitrofe.prcp)
    lista.max.dist<-list("tmax"=max.dist.tmp,"tmin"=max.dist.tmp,"prcp"=max.dist.prcp)
    lista.max.gap<-list("tmax"=max.gap.height.tmp,"tmin"=max.gap.height.tmp,"prcp"=max.gap.height.prcp)

		###############################################TEMPERATURA
     foreach(nome.parametro=c("tmax","tmin"),.combine="cbind") %do% {

      print(nome.parametro)
      massima.distanza<-lista.max.dist[[nome.parametro]]
      massimo.gap<-lista.max.gap[[nome.parametro]]
      nmin.limitrofe<-lista.nmin.limitrofe[[nome.parametro]]
      
      vector(mode="list",length=2)->listaOut

      names(listaOut)<-paste(c("regression","corroboration"),nome.parametro,sep=".")
      
      if(corroborationt) listaOut[[paste0("corroboration.",nome.parametro)]]<-rep(0,numeroRighe)      
      if(regressiont) listaOut[[paste0("regression.",nome.parametro)]]<-rep(0,numeroRighe)      
      
		  #cerca limitrofe: le limitrofe devo ricercarle ogni volta per ogni parametro
		  #(o almeno due volte: una pr la temperatura una per la precipitazione)
		  cercaLimitrofe(idstaz,
		                 matrice=matrice.distanze,
		                 ana=anagrafica,
		                 distanza=massima.distanza,
		                 min.numero.limitrofe=3,
		                 max.gap.height=massimo.gap)->sub.ana


		  if(is.null(sub.ana) || !nrow(sub.ana)){
		    messaggi(paste0(toupper(nome.parametro),": Nessuna stazione limitrofa per ",idstaz))	
		    return(as_tibble(purrr::compact(listaOut)))
		  }#is.null sub.ana 

    	#sub.ana contiene per la stazione target e per le stazioni limitrofe le info quali: lat lon quota e distanze
    	#lapply va da 2 perchè il primo codice è quello della stazione target che non vogliamo 
    	#memorizzare in lista.stazioni.
    	#Dobbiamo però ricordare che sub.ana contiene una stazione in più (la stazione target che occupa la
    	#prima riga del data.frame) mentre lista.limitrofe contiene solo le limitrofe

	    #la prima è la target, togliamola. lista.limitrofe contiene i dati
    	crea.lista.limitrofe(sub.ana$idstaz[-1],parametro=nome.parametro)->lista.limitrofe

    	#numero delle limitrofe 
    	length(lista.limitrofe)->numero.limitrofe    	
    	#ho trovato un numero di stazioni >= NMIN.LIMITROFE?
    	if(numero.limitrofe < nmin.limitrofe){ 
    	  messaggi(paste0(toupper(nome.parametro),": No available neighbour stations for ",idstaz))	
    	  return(as_tibble(purrr::compact(listaOut)))
    	}#su num.limitrofe    	
  	
    	#Definiamo alcuni parametri che ci servono per la scrittura dell'output
    	names(lista.limitrofe)->codici.limitrofe #codici delle limitrofe effettivamente utilizzate
    	    	
    	#nel caso della temperatura
    	if(nome.parametro=="tmax" || nome.parametro=="tmin"){

    	  ClimateData(x=mdatos[,c("year","month","day",nome.parametro)],param=nome.parametro)->sub.dati
    	  class(sub.dati)<-c("temperature",class(sub.dati))

    	  xtsAttributes(sub.dati)<-list(codice=idstaz)

    	  if(corroborationt){
    	    
              messaggi(testo=sprintf("CONTROLLI SPAZIALI: corroboration test per temperatura %s",nome.parametro))
       	  
    	    
          	  corroborationTest(dati.xts=sub.dati,
          	                  lista.limitrofe.xts=lista.limitrofe,
          	                  nmin.limitrofe=nmin.limitrofe.tmp,
          	                  extraOut="",
          	                  debug="",
          	                  grafici=grafici)->giorniInvalidatiCorr
      	  
              #prima di eseguire i test successivo devo invalidare
          	  if(length(giorniInvalidatiCorr)){
          	    which(index(sub.dati) %in% giorniInvalidatiCorr)->indexCorr
          	    sub.dati[indexCorr,]<-NA
          	  } 
          	    
          	    
    	  }# su corroborationt  	  
  	    
    	  if(regressiont){

            	  messaggi(testo=sprintf("CONTROLLI SPAZIALI: regression test per temperatura %s",nome.parametro))
            	  tempRegression_test(
            	    dati.xts=sub.dati,
            	    lista.limitrofe.xts=lista.limitrofe,
            	    extraOut="",
            	    debug="",
            	    grafici=grafici)->giorniInvalidatiReg

            	  #############################################            	          
            	  if(length(giorniInvalidatiReg)){
        
            	    which(index(sub.dati) %in% giorniInvalidatiReg)->indexReg
            	    stopifnot(length(indexReg)!=0)
            	    sub.dati[indexReg,]<-NA            	    
            	    
            	  }    	      	  

    	  }#su regressiont    	  
            
    	  #aggiorniamo colonna flag per i test: dove abbiamo invalidati mettiamo 1
    	  if(corroborationt && length(giorniInvalidatiCorr)){
    	    listaOut[[paste0("corroboration.",nome.parametro)]][indexCorr]<-1
    	  }#fine if
    	  
    	  if(regressiont && length(giorniInvalidatiReg)){
    	    listaOut[[paste0("regression.",nome.parametro)]][indexReg]<-1	    
    	  }#fine if    	  

        return(as_tibble(purrr::compact(listaOut)))
    	  
  	}else if(nome.parametro=="prcp"){
    		  
      #attenzione: la serie di precipitazione per la stazione che sto testando potrebbe essere tutta
  	  #NA. All'interno del test però il primo passo è proprio eliminare tutti gli NA. Se la serie rimane vuota
  	  #il test non viene eseguito
  	  if(corroborationp){
  	    
  	      mdatos$corroboration.prcp<<-0
  	    
      	  xts(mdatos[,c(nome.parametro)],order.by=as.Date(paste(mdatos$year,mdatos$month,mdatos$day,sep="-")))->sub.dati
      	  class(sub.dati)<-c("precipitation",class(sub.dati))

      	  #test
      	  messaggi(testo=sprintf("CONTROLLI SPAZIALI: corroboration test per precipitazione %s",nome.parametro))
      	  corroborationTest(dati.xts=sub.dati,
      	                    lista.limitrofe.xts=lista.limitrofe,
      	                    nmin.limitrofe=nmin.limitrofe.tmp,
      	                    extraOut=extraOut)->giorniInvalidatiCorrp

      	  ####fine test corroborationp
      	  
      	  #ho trovato dei risultati?
      	  if(length(giorniInvalidatiCorrp)){
      	    
      	    which(index(sub.dati) %in% giorniInvalidatiCorrp)->indexCorrp
      	    stopifnot(length(indexCorrp)!=0)
      	    #invalidiamo la serie di precipitazione e risassegnamola a mdatos
      	    sub.dati[indexCorrp,nome.parametro]<-NA  
      	    
      	    #aggiorniamo mdatos: se sono qui dentro vuol dire che ho trovato almeno un risultato 
      	    mdatos[indexCorrp,nome.parametro]<<-NA      	    
            
      	    #aggiorniamo flag
      	    mdatos[indexCorrp,c(paste0("corroboration.",nome.parametro))]<<-1      	    
      	    
      	    return(1)
      	    
      	  }else{
      	    
      	    return(0)
      	    
      	  }#length(giorniInvalidatiCorrp)    	      	  
      	  
  	  }#corroborationp 
    	  
  	}else{
  	  stop(sprntf("Nome parametro non riconosciuto %s",nome.parametro))
  	}	  

    }->risultatiTest

    #riscriviamo mdatos solo se almeno un test è stato eseguito e ha identificato
    #almeno un risultato (somma di risultatiTest !=0). 
    if(is.null(risultatiTest)) return()
     
    if(any(apply(risultatiTest,2,FUN=sum,na.rm=TRUE)!=0)){
      as_tibble(cbind(mdatos,risultatiTest)) %>%
        write_delim(.,delim=",",file=paste0("n",idstaz,".txt"),col_names = TRUE)    
    }
    
	 })#fine lapply su codici

} #fine elaboraSpatial

