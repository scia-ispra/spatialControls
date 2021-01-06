#cercaLimitrofe
#ordina: restituisce i codici delle stazioni ordinate dalla più vicina alla più lontana.
#min.numero.limitrofe: se non ne trova almeno TOT restituisce un vettore nullo
#idstaz: idstaz della stazione targe
#distanza: massima distanza delle stazioni
#matrice: matrice delle distanze
#cercaLimitrofe restituisce un data.frame con una colonne di codici stazioni e una colonna di distanze
#La prima riga si riferisce alla stazione target (distanza 0.0km)
cercaLimitrofe<-function(idstaz,matrice,ana,distanza=75,min.numero.limitrofe=3,max.gap.height=100){

  stopifnot(is.character(idstaz) && nchar(idstaz))
  stopifnot(is.matrix(matrice) && length(rownames(matrice)) && length(colnames(matrice)))

  if(!is.numeric(max.gap.height) || max.gap.height<0){
    print("distanza quota non valida, uso valore di default: 100 m")
    max.gap.height<-100
  }

  if(!is.numeric(distanza) || distanza<0){
    print("distanza limitrofe non valida, uso valore di default: 75 km")
    distanza<-75
  }    
  
  if(!is.numeric(min.numero.limitrofe) || min.numero.limitrofe<0){
    print("numero minimo di limitrofe non valido, uso valore di default: 3")
    min.numero.limitrofe<-3
  }    
  
	messaggio.errore(argomento=match.call(),parametri.mai.nulli=c("idstaz","matrice","ana"))

 	which(rownames(matrice)==idstaz)->riga
	#questo nn si dovrebbe mai verificare!
	if(length(riga)!=1) stop("Errore rownames in matrice distanze")

	#Ricerca delle stazioni limitrofe (senza contare la stazione target che dista da se stessa 0km)
	#La distanza della target da se stessa non viene essattamente 0, basta porre 0.1 per escluderla dal 
	#calcolo dele limitrofe
	which(matrice[riga,]<=distanza)->colonne

	#non ho nessuna stazione o comunque non ne ho un numero sufficiente
	#min.numero.limitrofe lo sommiamo a 1 perchè la stazione target da se stessa verrà sempre inclusa.. el a vogliamo includere nell'output
	if(length(colonne) < (min.numero.limitrofe+1)) return(NULL)

	#restituisce i codici ordinati in base alla distanza
	lista<-data.frame("idstaz"=as.character(colnames(matrice)[colonne]),
	                  "distanze"=matrice[riga,][colonne],stringsAsFactors = FALSE)
	
	#importante porre sort=FALSE
	ana[ana$idstaz==idstaz,]$quota->targetQuota
  
	dplyr::left_join(lista,ana,by="idstaz") %>% 
	  arrange(distanze) %>%
	  filter(quota<=max.gap.height+targetQuota & quota>=targetQuota-max.gap.height)->mlista2

	#nuovo controllo su quante stazioni ho:nrow-1 perchè la prima riga si riferisce alla stazione target
	if(nrow(mlista2) < (1+min.numero.limitrofe) ) return(NULL)

	mlista2

}#fine cercaLimitrofe


##############################################################
##############################################################
trovaIndici<-function(indice,mese,giorno,numero.dati,winSize=7){

	messaggio.errore(argomento=match.call(),parametri.mai.nulli=c("indice","mese","giorno"))
	
	if(is.null(winSize)) winSize<-1 

	indice-winSize->indice.left
	indice+winSize->indice.right

	length(indice.right)->len

	#togliamo i negativi

	while(indice.left[1]<=0){

		indice.left[1]<-indice.left[1]+1

	}#fine while


	while(indice.right[len]>numero.dati){

		indice.right[len]<-indice.right[len]-1

	}#fine while				


	vettore.indici<-""

	for(ii in 1:len){

		paste0(indice.left[ii],":",indice.right[ii])->vettore.indici[ii]

	}#ciclo su for

	#la forma più facile per estrarre i dati sulla base degli indici in indici.left e indici.right
	#è costruire un file di testo come quello in str.indici
	paste0("c(",paste(vettore.indici,collapse=","),")")->str.indici

	#è una stringa di testo
	return(str.indici)


}#fine trova indici


######################################################################
#PESI per BIWEIGHT MEAN
######################################################################

	pesi<-function(x,c=7.5,mediana){

		mad(x)->my.mad
		u<-(x-mediana)/(c*my.mad)

		which(abs(u)>=1)->index.u
    u[index.u]<-0

		u

	}#fine pesi

#######################################################################
#BIWEIGHT MEAN and STD
#biweight mean: vedi Lanzante 1996, resistant tobust and non parametric techniques..
#######################################################################


biw.mean<-function(x,pesi,mediana){
  pesi*pesi->pp
  ( 1-pp)*(1-pp)->ppp
	mediana+((sum((x-mediana)*ppp) )/( sum(ppp) ))
}#fine biweighted mean

cppFunction('double biwmeancpp(NumericVector x,NumericVector pesi,double mediana){

  int pesiSize=pesi.size();
  NumericVector ppp(pesiSize);

  double numeratore=0;
  double denominatore=0;
  double ris=0;

  for(int i=0;i< pesiSize;++i){
    ppp[i]=pow(1-pow(pesi[i],2.0),2.0);
    numeratore=numeratore+((x[i]-mediana)*ppp[i]);
    denominatore=denominatore+ppp[i];
  }// fine ciclo for

  ris=mediana+(numeratore/denominatore);

  return ris;

}')

biw.std<-function(x,pesi,mediana){

	len<-length(x)
	pesi*pesi->pp
	
	numeratore<-sqrt(len*sum(((x-mediana)*(x-mediana))*(1-(pp))^4 ))
	denominatore<-abs(sum((1-(pp))*(1-5*(pp))))					

	numeratore/denominatore
						
}#fine biweighted std


cppFunction('double biwstdcpp(NumericVector x, NumericVector pesi,double mediana){
            
  int len=x.size();
  double somma=0;
  double numeratore=0;
  double denominatore=0;
  double pp=0;
  double ris=0;

  for(int i=0;i<len;++i){
    pp=pow(pesi[i],2);
    somma= pow(x[i]-mediana,2)*pow((1-pp),4)+somma;
    denominatore=denominatore+((1-5*pp)*(1-pp));
  }//fine ciclo for

  numeratore= sqrt(len*somma);

  ris=numeratore/abs(denominatore);

  return ris;

}')


######################################################################
######################################################################

calcolaMediaBI<-function(valori,NUMERO.DATI=100){

	messaggio.errore(argomento=match.call(),parametri.mai.nulli=c("valori"))

	valori[!is.na(valori)]->nvalori

	#se sono meno di 100 i valori non NA  non posso calcolare media
	if(length(nvalori)<NUMERO.DATI) return(NA)

	#valori ora contiene tutti i valori nella serie che corrispondono a un determinato giorno e mese
	#e tutti i giorni che rientrano in una finestra di dimensione winSize

	#sulla base di questi dati calcolo la media e la std con cui poi calcolare lo z-score
	median(nvalori)->mediana

	####
	pesi(x=nvalori,mediana=mediana)->uuu
	
#	biw.mean(x=nvalori,pesi=uuu,mediana=mediana)
  biwmeancpp(x=nvalori,pesi=uuu,mediana=mediana)

}#calcolaAnomalie



######################################################################
#CORROBORATION TEST
######################################################################

corroborationTest<-function(dati.xts,...){
  UseMethod("corroborationTest",dati.xts)
}

#numero.dati.media: quanti dati devo avere (non nulli) per costruire la media di lungo periodo (per il calcolo delle anomalie)?
#scriviOut: se una stringa di testo, viene utilizzata come directory dove salvare output
#utile per successive rielaborazioni dati.Se NULL non viene scritto nulla.
corroborationTest.temperature<-function(dati.xts,
                                        lista.limitrofe.xts,
                                        nmin.limitrofe=3,
                                        numero.dati.media=100,
                                        gap.anomalie=10,
                                        finestra=7,
                                        extraOut="",
                                        debug="",
                                        grafici=""){

  stopifnot(is.ClimateData(dati.xts) && (length(lista.limitrofe.xts)!=0))

  ###
  #per verificare di avere un numero di dati suff per il test aggreghiamo a livello annuale
  #utilizziamo i valori di default per aggregare.
  aggregaCD(aggregaCD(dati.xts,max.na = 10,rle.check = FALSE,max.size.block.na=10),max.na=0,ignore.par=FALSE,rle.check=TRUE,max.size.block.na=3,seasonal=TRUE)->serieAnnuale

  #quanti anni ho che non sono NA? Sono abbastanza?
  year(serieAnnuale[!is.na(serieAnnuale),])->anni.serieAnnuale
  length(anni.serieAnnuale)->numero.anni.annuali

	#se il numero di anni è troppo ridotto, il controllo per calcolare la media fallirà
	#sempre, inutile in questo caso procedere.
  #Questo controllo evita di perdere tempo con serie che sono già di partenza troppo corte.
	(2*finestra+1)->fff
	
	if((numero.anni.annuali*fff)< numero.dati.media){
	  message("Numero di anni insuf. per procedere al controllo, esco")
	  return()
	}
	
  rm(serieAnnuale)

  #eliminiamo gli NA: stiamo lavorando con oggetti xts quindi nn dobbiamo preoccuparci di buchi
  
  #salviamo in target i dati.xts mantenendo anche gli NA; target serve solo se vogliamo fare i grafici delle serie
  #target ci serve perchè contiene anche gli NA (se dobbiamo disegnare la serie non dibbiamo avere buchi)
  if(nchar(grafici)) dati.xts->target
  dati.xts[!is.na(dati.xts[,1]),]->dati.xts
 
  xtsAttributes(dati.xts)$codice->codice
  xtsAttributes(dati.xts)$parametro->parametro
  
  if(nchar(debug)){
    
    if(!file.exists(debug)) dir.create(debug)
    numeroAlterare<-200 #numero di dati da alterare

    nrow(dati.xts)->mmax
    stopifnot(numeroAlterare<mmax)
    sample(x=1:mmax,size=numeroAlterare)->righeInvalidare
    dati.xts[righeInvalidare,]->originali

    dati.xts[righeInvalidare,]<-dati.xts[righeInvalidare,]+round(runif(numeroAlterare,min=-15,max=15),1)
    
    data.frame(coredata(originali)) %>%
      mutate(time=index(originali),alterati=round(as.vector(dati.xts[righeInvalidare,]),1) ) %>%
        select(time,alterati,everything()) %>%
          write_delim(.,file=paste0(debug,codice,"_",parametro,"_valoriAlterati.csv"),delim=";",col_names=TRUE)
   
   rm(originali) 

   #se dobbiamo fare i grafici allora a target dobbiamo assegnare i valori alterati
   #che vogliamo vedere nei grafici
   if(nchar(grafici)) target[index(target) %in% index(dati.xts[righeInvalidare,]),]<-dati.xts[righeInvalidare,]
     
  }#fine debug
  
  #numero.anni potrebbe differire da numero.anni.annuali in quanto
  #un anno potrebbe essere presente ma risultare NA una volta aggregato (criteri stretti per aggregare)
  #ma comunque avere dati
  unique(year(dati.xts))->anni
  length(anni)->numero.anni  
 
	#lista.xts: sono le serie limitrofe
	lapply(lista.limitrofe.xts,FUN=function(serie){
	  
	  stopifnot(is.ClimateData(serie))
	  #febbraio 2018 aggregaCD(aggregaCD(serie,max.na = 10,rle.check = FALSE),max.na=0)->lserieAnnuale
	  aggregaCD(aggregaCD(serie,max.na = 10,rle.check = FALSE,max.size.block.na=10),max.na=0,ignore.par=FALSE,rle.check=TRUE,max.size.block.na=3,seasonal=TRUE)->lserieAnnuale
	  
	  year(lserieAnnuale[!is.na(lserieAnnuale),])->anni.serieAnnualeLimitrofa
	  length(anni.serieAnnualeLimitrofa[anni.serieAnnualeLimitrofa %in% anni.serieAnnuale])*fff->numero.dati.utili

	  #ifelse con NULL da problemi
	  if(numero.dati.utili<numero.dati.media){
	    return(NULL)
	  }else{
	    return(numero.dati.utili)
	  }
	
	})->lista.numero.anni.limitrofe
	
	rm(anni.serieAnnuale)
	
	#names(lista.numero.anni.limitrofe)<-names(lista.limitrofe.xts)
	Filter(Negate(is.null),lista.numero.anni.limitrofe)->lista.numero.anni.limitrofe

	length(lista.numero.anni.limitrofe)->quante.limitrofe
	
	if( quante.limitrofe < nmin.limitrofe){
	  message("Numero di anni insuf. per procedere al controllo, esco")
	  return()
	} 
	
	#da questo momento in poi names(lista.numero.anni.limitrofe) mi permette di acquisire i codici delle
	#limitrofe da utilizzare effettivamente per fare il controllo "corroboration"
	
	#vettore dome memorizziamo i flag
	nrow(dati.xts)->totaleRighe
	
	#oggetto xts per i flag
	xts(rep(0,totaleRighe),order.by=index(dati.xts))->flag.corroboration

	#media target
	media.target.lim<-matrix(rep(-999,length=366*(quante.limitrofe+1)),ncol=quante.limitrofe+1,nrow=366)
  colnames(media.target.lim)<-c("target",names(lista.numero.anni.limitrofe))
  
  #i row.names servono solo per visualizzare i giorni e mesi dove posizioniamo le medie calcolate
  #volendo si può otgliere. Se si utilizza row.names utilizzare un anno bisestile perche la matrice
  #ha 366 righe
	row.names(media.target.lim)<-as.character(seq.Date(from=as.Date("1904-01-01"),to = as.Date("1904-12-31"),by="day"))
  
	#lista con il numero di anni distinti di ciascuna serie. 
	lapply(1:totaleRighe,FUN=function(riga){

	  #abbiamo già escluso gli NA
	  #if(is.na(dati.xts[riga,1])) return()

	  index(dati.xts[riga,])->giornoMeseAnno

	  giornoMeseAnno-days(1)->giornoMeseAnno_meno1
	  giornoMeseAnno+days(1)->giornoMeseAnno_piu1
	  
	  day(giornoMeseAnno)->giorno
	  month(giornoMeseAnno)->mese
	  
	  if(giorno==29 && mese==2) return()
	  #partendo da giorno e mese devo prendere la finestra per ogni anno tra quelli
	  #della serie target

	  #l'anno sarà sempre il primo anno (anni[1]) perche la finestra di 15 giorni la prendo su
	  #tutta la serie tra il primo e l'ultimo anno.
	  giornoMeseAnnoUpdated<-as.Date(paste(anni[1],str_pad(mese,width =2,pad ="0",side="left"),giorno,sep="-"))
    #print(giornoMeseAnno)
    #posizione all'interno di un anno completo: yday, giorno all'interno di un anno di 366 giorni
    #serve per identificare la riga della matrice media.target.lim. 
    yday(as.Date(giornoMeseAnno))->posizione
    
    #Siccome dall'analisi escludiamo il 29 febbraio, nel caso di un anno bisestile devo
    #incrementare posizione di 1 appena supero il 29 febbraio
    if(!leap_year(giornoMeseAnno) && mese>2) posizione<-posizione+1

############################################################    
    
    #Perchè faccio questo controllo? Perchè se calcolo la media di un determinato
    #mese e giorno (quando ho abbastanza dati) non serve ricalcolarla ogni volta che
    # "riga" ripassa sopra quel determinato giorno e mese. Potrebbe però succedere
    #che per quel "giorno-mese" non ho abbastanza dati per il calcolo della media,
    #in questo caso in matrice[posizione,"target"] metto NA. Se trovo in matrice[posizione,"target"]
    #un NA inutile che vado a perdere cicli e tempo: per quel "mese-giorno" controllo non possibile
    if(is.finite(media.target.lim[posizione,1]) && media.target.lim[posizione,1]< -900){

      	  lapply(1:numero.anni,FUN=function(ii){
      	    window(x=dati.xts,start=giornoMeseAnnoUpdated+years(ii-1)-days(finestra),
      	           end=giornoMeseAnnoUpdated+years(ii-1)+days(finestra))
      	  })->giorniMeseInWindow

      	  #do.call("rbind",giorniMeseInWindow)[,1]->df.giorniMeseInWindow
          do.call("rbind",giorniMeseInWindow)->df.giorniMeseInWindow
          #se df.giorniMeseInWindow  ha zero righe 
          if(!nrow(df.giorniMeseInWindow)) return()
      	  calcolaMediaBI(as.vector(df.giorniMeseInWindow),NUMERO.DATI=numero.dati.media)->media
      	  
      	  media.target.lim[posizione,1]<<-media

    }#fine if su is.null media.target   	  

    #se per la target ho NA (numero dati esiguo) inutile che vado a esaminare le limitrofe
    if(!is.finite(media.target.lim[posizione,1])) return()
############################################################# 
    
    #ora devo calcolare le anomalie per ciascuna delle stazioni limitrofe. Posso utilizzare "str.indici" 
    #perchè so che tutte le serie iniziano stesso giorno mese e anno (almeno formalmente con gli NA nel
    #caso di anni nulli).
    
    #la parte che segue fa affidamento sul fatto che in lista.limitrofe le stazioni siano ordinate dalla
    #più vicina alla più lontana, ovvero da aver utilizzato ordina=TRUE nella funzione cercaLimitrofe
    			
    #ogni elemento di indice identifica un determinato giorno/mese nei diversi anni. Ad esempio: il primo
    #elemento di indice ci permette di trovare all'interno di ciascuna serie la posizione del 1 Aprile nel 1961
    #la seconda posizione ci dice dove si trova il 1 Aprile nel 1962, la terza posizione 1 Aprile 1963 e cosi via

					#ciclo su stazioni limitrofe: 
          lapply(names(lista.numero.anni.limitrofe),FUN=function(nome.limitrofa){

##########################################################################            

            if(is.finite(media.target.lim[posizione,nome.limitrofa]) && media.target.lim[posizione,nome.limitrofa]< -900){
    						#per ogni stazione limitrofa calcoliamo la media sul lungo periodo.La media la vogliamo
    					  #calcolare sullo stesso periodo della stazione target!
    					  as.vector(lista.limitrofe.xts[[nome.limitrofa]][.indexDate(lista.limitrofe.xts[[nome.limitrofa]]) %in% .indexDate(df.giorniMeseInWindow),1])->vLimitrofa 
    					  calcolaMediaBI(vLimitrofa,NUMERO.DATI=numero.dati.media)->media.limitrofa
    					  media.target.lim[posizione,nome.limitrofa]<<-media.limitrofa
            }
            
            #restituisco 0: mi serve per calcolare per quante limitrofe ho calcolato la media
						if(!is.finite(media.target.lim[posizione,nome.limitrofa])) return(0)
            
############################################################################            

						#calcolo di anomalie: per la stazione limitrofa devo calcolare l'anomalia
						#non solo per il giorno/mese/anno ma anche per il giorno che segue e quello precedente
						#per il primo gennaio del primo anno e per il 31 dicembre dell'ultimo anno
						#il test salterà

		        #estraiamo giorno prima e giorno dopo del giorno annoMeseGiorno
		        #utilizzando gli oggetti xts non dobbiamopreoccuparci di verificare se il giorno
		        #èeffettivamente presente nella serie, possiamo evitare l'assunzione che le serie
		        #siano complete nel calendario (cosa di cui dovevamo preoccuparci nell'implementazione precedente)

					  as.vector(window(lista.limitrofe.xts[[nome.limitrofa]],start=giornoMeseAnno_meno1,end =giornoMeseAnno_piu1)[,1])->valori.limitrofa
		        if(length(valori.limitrofa)<3 || any(is.na(valori.limitrofa))) return(0)

            abs(valori.limitrofa-media.target.lim[posizione,nome.limitrofa])->anomalie.limitrofe
            #la media potrebbe essere NA e invalidare anomalie.limitrofe
            #8 Novembre: tolto questo controllo, inutile non sarà mai NA
            #if(any(is.na(anomalie.limitrofe))) return(0) <-8 Novembre tolto controllo 
               
            #anomalia del valore rispetto alla propria media
            abs(as.vector(dati.xts[riga,1])-media.target.lim[posizione,1])->anomalia.target		

						ifelse(all(abs(anomalia.target-anomalie.limitrofe)>=gap.anomalie),1,0)
						
          })->listaOut

					#se somma >=3 allora il dato va flaggato e invalidato
					if(sum(unlist(listaOut))>=3){
					  flag.corroboration[riga,1]<<-1
					  dati.xts[riga,1]<<-NA #il valore aggiornato parteciperà "aggiornato" alla media di lungo periodo
					}#su sum
					
		  }) #lapply su riga
	
    	#parte grafica da terminare

    	if(nchar(extraOut)){
    	  as.data.frame(media.target.lim) %>%
    	    mutate(time=row.names(media.target.lim)) %>%
        	  write_delim(.,delim=";",col_names =TRUE,file=paste0(extraOut,codice,"_",parametro,"_matriceMedie.corroboration.csv"))
    	} #su nchar(extraOut)
	
      if(nchar(grafici) && length(index(flag.corroboration[flag.corroboration[,1]>0,]))){

        merge(target,do.call("merge",lista.limitrofe.xts))->dati.per.grafico
        names(dati.per.grafico)[1]<-"target"
        names(dati.per.grafico)[2:ncol(dati.per.grafico)]<-names(lista.limitrofe.xts)
        rm(lista.limitrofe.xts)        
        rm(target)
        
        pdf(paste0(grafici,codice,"_",parametro,"_corroborationTest.pdf"))
        lapply(index(flag.corroboration[flag.corroboration[,1]>0,]),FUN=function(giorno){
          window(dati.per.grafico,start=giorno-days(15),end=giorno+days(15))->dati.finestra
          plot.zoo(dati.finestra,nc=1,screens=1,col=c("red",rep("#333333",ncol(dati.finestra)-1)),main=giorno)
        })
        dev.off()
      }#su nchar(grafici)	
	
	    #restituiamo i giorni invalidati (con flag !=0) se ce ne sono
	    index(flag.corroboration[flag.corroboration[,1]>0,])
	

}#fine corroboration_test

###############################
#TEMP REGRESSION TEST
###############################

#sub.ana: è il data.frame con idstaz lat lon quota delle stazioni limitrofe. 
#Attenzione: la prima riga si riferisce alla stazione target (distanza da ase stessa nulla)
#NUMERO.DATI: quanti dati devo avere (non nulli) per costruire la media di lungo periodo (per il calcolo delle anomalie)?
tempRegression_test<-function(dati.xts,
                              lista.limitrofe.xts,
                              nmin.limitrofe=3,
                              extraOut="",
                              debug="",
                              grafici=""){


  stopifnot(is.ClimateData(dati.xts) && (length(lista.limitrofe.xts)!=0))
  
  xtsAttributes(dati.xts)$parametro->parametro
  xtsAttributes(dati.xts)$codice->codice  
  
  #vettore dome memorizziamo i flag
  nrow(dati.xts)->totaleRighe

  xts(rep(0,totaleRighe),order.by = index(dati.xts))->flag.regression

  #numero limitrofe
  numero.limitrofe<-length(lista.limitrofe.xts)

	#numero di anni
  unique(year(dati.xts))->anni
  length(anni)->numero.anni

  #creiamo le date per inizio e fine mese, poi ci sommiamo/sottraiamo la finestra dei quindici giorni 
  endpoints(dati.xts,on="months")->indiciMesi
  index(dati.xts[indiciMesi,])->fineMese
  update(fineMese,days=1)->inizioMese

  mapply(FUN=function(inizio.mese,fine.mese){
    
    window(dati.xts,start=inizio.mese-days(15),end=fine.mese+days(15))->dati.finestra
    dati.finestra[!is.na(dati.finestra[,1]),]->subDati

    nrow(subDati)->nsub
    if(nsub<40) return()

    if(nchar(debug)) dati.finestra->originali
    rm(dati.finestra)
     
    #ciclo limitrofe
    #ciclo su stazioni limitrofe
    lapply(1:numero.limitrofe,FUN=function(indice.staz.lim){
      
      stopifnot(is.ClimateData(lista.limitrofe.xts[[indice.staz.lim]]))
      
      window(lista.limitrofe.xts[[indice.staz.lim]],
             start=inizio.mese-days(15),
             end=fine.mese+days(15))->limitrofa.finestra

      #verifichiamo che laddove i dati della stazione target non sono NA
      #anche quelli della stazione limitrofa non sono NA
      limitrofa.finestra[!is.na(limitrofa.finestra[,1]),]->subLimitrofa
      if(nrow(subLimitrofa)<40) return(NULL)

      subLimitrofa
      
    })->listaLimitrofeInWindow #fine lapply su limitrofe    
      
    names(listaLimitrofeInWindow)<-names(lista.limitrofe.xts)
    Filter(Negate(is.null),listaLimitrofeInWindow)->limitrofe.filtrate
    rm(listaLimitrofeInWindow)

    #inutile andare avanti con i cicli giornalieri per questo mese se ho meno di 3 stazioni limitrofe
    #su cui calcolare l'index of agreement e il modello di regressione	
    if(length(limitrofe.filtrate)<nmin.limitrofe) return(NULL)

    ##########debug
    if(nchar(debug)){
      
      nrow(subDati)->mmax
      numeroAlterare<-2

      sample(x=1:mmax,size=numeroAlterare,replace = FALSE)->righeAlterare

      subDati[righeAlterare,]<-subDati[righeAlterare,]+round(runif(n = numeroAlterare,min = 0,max=30),1)
      
      data.frame(tempo=index(subDati[righeAlterare,]),
                 originali=coredata(originali[.indexDate(originali) %in% index(subDati[righeAlterare,]),]),
                 alterati=as.vector(subDati[righeAlterare,]))->df.per.debug

      #df.per.debug non lo scriviamo qui. Infatti le limitrofe potrebbero non soddisfare le condizioni per il test.
      #Prima vediamo se sono rispettati tutti i vincoli per il test e allora scriviamo il file
      
    }#fine su debug
    
    #calcolo index of agreement
    lapply(limitrofe.filtrate,FUN=function(limitrofa){
      
      #calcolo index of agreement
      #Devo fare il merge perchè non è detto che le due serie abbiano lo stesso numero di giorni
      #Ad esempio una serie potrebbe avere il 22 del mese mancante e l'altra no. Non posso
      #fare una semplice differenza tra vettori, devo fare il merge prima dei timestamp
      merge(subDati,limitrofa)->unione

      numeratore<-sum(abs(unione[,1]-unione[,2]),na.rm=TRUE)
      mean(unione[,2],na.rm=TRUE)->limitrofa.monthlyMean

      prima.somma<-abs(unione[,1]-limitrofa.monthlyMean)
      seconda.somma<-abs(unione[,2]-limitrofa.monthlyMean) 
      
      #index of agreement
      1-(numeratore/sum(prima.somma+seconda.somma,na.rm=TRUE))->indexAgreement

      #modello di regressione lineare: limitrofa fa da independent variable
      #la target fa da dependent
      as.vector(unione[,1])->dipendente
      as.vector(unione[,2])->indipendente      
      lm(dipendente~indipendente)->modello  

      intercetta<-coefficients(modello)[1]
      pendenza<-coefficients(modello)[2]
      
      c(indexAgreement,pendenza,intercetta)
      
    })->listaParametri
    
    #dfParametri: rownames sono i codici delle limitrofe
    do.call("rbind",listaParametri)->dfParametri
    colnames(dfParametri)<-c("agreement","slope","interc")

    #se ho più di sette stazioni limitrofe, ordiniamole per index of agreemnt
    #dfParametri contiene per ciascuna limitrofa la stima dell'agreement index, della pendenza e dell'intercetta
    #del modello lineare

    numMaxPar<-7
    if(nrow(dfParametri)>numMaxPar){
      dfParametri[order(dfParametri[,1],decreasing=TRUE),][1:numMaxPar,]->dfParametri
      #prendiamo le corrispondenti limitrofe
      limitrofe.filtrate[rownames(dfParametri)]->limitrofe.filtrate
    } 
 
    inizio.mese->dataRif
    paste0(codice,"_",parametro,"_",dataRif)->prefisso    
     
    #per il rapporto volendo scrivere dfParametri
    if(nchar(extraOut))
      write.table(as.data.frame(dfParametri),sep=";",col.names =TRUE,row.names = TRUE,file = paste0(extraOut,prefisso,".dfParametriRegression.csv"))      

    #subDati è già privo di NA
    lapply(1:nsub,FUN=function(rrr){
      
      index(subDati[rrr,])->giornoMeseAnno
      as.vector(subDati[rrr,1])->valore.target

      #ciclo su stazioni limitrofe filtrate, So già che hanno abbastanza dati per procedere
      lapply(1:length(limitrofe.filtrate),FUN=function(indice.staz.lim){

        #ripeschiamo i valori della limitrofa
        window(limitrofe.filtrate[[indice.staz.lim]],start=giornoMeseAnno-days(1),end=giornoMeseAnno+days(1))->valori.limitrofa

        if(nrow(valori.limitrofa)<3) return(NULL)
        as.vector(valori.limitrofa)->valori.limitrofa
 
        #ricordare che 	valori.in.monthlyWindow è un dataframe mentre valori.limitrofa.in.monthlyWindow
        #è un vettore
        abs(valore.target-valori.limitrofa)->pairwise.diff
        
        #i tre valori devono essere tutti differenti da NA, altrimenti per questo giorno di questo mese
        #passiamo alla prossima stazione imitrofa
        if(any(is.na(pairwise.diff))) return(NULL)
        
        #Se arrivo qui i pairwise.diff sono tutti e tre diversi da NA.
        #minimo limitrofa lo uso per stimare y^		
        valori.limitrofa[which.min(pairwise.diff)]->minimo.limitrofa		

        # vet.minimo[indice.staz.lim]<<-minimo.limitrofa				
        
        #ora calcoliamo il valore yi mediante la funzione di regressione. 
        return(dfParametri[indice.staz.lim,3]+(dfParametri[indice.staz.lim,2]*minimo.limitrofa))*dfParametri[indice.staz.lim,1] 
        
      })->listaStimaTarget #lapply su indice.staz.lim  

      names(listaStimaTarget)<-names(limitrofe.filtrate)
      Filter(Negate(is.null),listaStimaTarget)->listaStimaTarget

      if(length(listaStimaTarget)<3) return(list("stimaFinale"=NA,"nomiLF"=names(limitrofe.filtrate)))
     
      #ora per il dato di giorno/mese/anno abbiamo la stima calcolata su tutte le stazioni
      #Calcoliamo la stima definitiva come media delle singole stime delle singole limitrofe
      
      #La stima finale è: numeratore, somma delle stime
      #Denominatore: somma degli index of agreement per quelle stazioni per cui è stato possibile
      #calcolare la stima (cioè che avevano tre giorni disponibili: giornoMeseAnno e il giorno successivo/precedente)

      sum(unlist(listaStimaTarget))->numeratore
      sum(dfParametri[rownames(dfParametri) %in% names(listaStimaTarget),1])->denominatore
      #stima.target.finale
      return(list("stimaFinale"=numeratore/denominatore,"nomiLF"=names(limitrofe.filtrate)))					
      
    })->listaStimaFinale #fine lapply su rrr
    
    #nomi limitrofe: ci serve in seguito per il grafico tra la serie target e le limitrofe effettivamente utilizzate
    listaStimaFinale[[1]]$nomiLF->nomiLF
    #stime
    unlist(lapply(listaStimaFinale,FUN=function(x) x$stimaFinale))->vstime
    rm(listaStimaFinale)

    #vet.yi.finale contiene la stima di tutti i valori non NA in subDati
    xts(vstime,order.by=index(subDati))->vet.yi.finale
    
    #controlliamo le lunghezze
    if(nrow(subDati)!=nrow(vet.yi.finale)) stop("Qualcosa è andato storto!")

    #vet.yi contiene la stima per tutti i giorni del mese in esame più 15 giorni prima e dopo	
    if(nrow(vet.yi.finale[!is.na(vet.yi.finale),1]) <40) return()
    
    #verifichiamo che la correlazione tra  i valori stimati per la stazione target e i valori effettivamente osservati
    #abbiano una correlazione di almeno 0.8. Se è così allora posso andare avanti

    #correlazione: se inferiore a 0.8 passiamo al mese successivo
    round(cor(as.vector(subDati),as.vector(vet.yi.finale),use="pairwise.complete.obs"),2)->correlazione

    if(correlazione<0.8) return() 
 
    #per il rapporto stampare questo grafico e fornire correlazione
    if(nchar(extraOut)){
      png( paste0(grafici,prefisso,".regressionScatter.png"),1024,768)
        plot(as.vector(subDati),as.vector(vet.yi.finale),xlab="serie osservata",ylab="serie stimata",main="")
        abline(a=0,b=1)   
      dev.off()
      
      sink(paste0(extraOut,codice,".",parametro,".correlazione.txt"),append=TRUE)
        cat(paste0(dataRif,";",correlazione,"\n")) 
      sink()      

    }#if su nchar    
    
    #calcolo residui.
    residui<-subDati-vet.yi.finale
    names(residui)<-"residui"
    
    #calcola media e std per calcolare residui standardizzati
    mean(residui[,1],na.rm=TRUE)->media.residui
    sqrt(var(as.numeric(residui[,1]),na.rm=TRUE))->std.residui
    
    ((residui-media.residui)/std.residui)->residui.std
    
    which(abs(residui)>=8)->index.residui	#8
    which(abs(residui.std)>=5)->index.residui.std #4
    
    #indici degli elementi che hanno residui e residui std che superano entrambi i threshold
    intersect(index.residui,index.residui.std)->index.comuni	

    #queste sono le date dei valori anomali trovati
    index(residui[index.comuni,])->timestampOutliers    

    #per rapporto la seguente figura
    if(nchar(grafici) && length(timestampOutliers)){
      merge(subDati,vet.yi.finale)->unione
      
      ####grafico delle serie rispetto alla serie stimata
      pdf(paste0(grafici,prefisso,"_regressionStima.pdf"))
                  
        lapply(timestampOutliers,FUN=function(ggmmyy){  
          
          myPanel<-function(x,...){
            lines(x,...)
            abline(v=as.Date(ggmmyy),col="red",lty=2)
          }          
          
          plot.zoo(unione,plot.type = "single",col=c("red",rep("#333333",ncol(unione)-1)),ylab="°C",xlab="",
                   main=paste0("Giorno: ",ggmmyy," Correlation: ",correlazione),panel=myPanel) #<- serie originale e serie ricostruita
        })#fine lapply su timestampOutliers
        
      dev.off()
      rm(unione)

      ####grafico delle serie rispetto alle serie limitrofe utilizzate per il test (limitrofe filtrate)
      pdf(paste0(grafici,prefisso,"_regressionTargetLimitrofe.pdf"))
      
      do.call("merge",lista.limitrofe.xts[nomiLF])->unioneLF
      merge(subDati,unioneLF[index(unioneLF) %in% index(subDati),])->unione
      rm(unioneLF)
      
      lapply(timestampOutliers,FUN=function(ggmmyy){  
        
        myPanel<-function(x,...){
          lines(x,...)
          abline(v=as.Date(ggmmyy),col="red",lty=2,lwd=0.5)
        }          
        
        plot.zoo(unione,plot.type = "single",col=c("red",rep("#333333",ncol(unione)-1)),ylab="°C",xlab="",
                 main=paste0("Giorno: ",ggmmyy," Correlation: ",correlazione),panel=myPanel) #<- serie originale e serie ricostruita
      })#fine lapply su timestampOutliers            
      dev.off()
      rm(unione)
      
    }#if su nchar     
    
    #non ci sono valori che verificano entrambi i flag, passo al controllo del mese successivo
    if(!length(index.comuni)) return()    
    
    if(nchar(debug)){
      df.per.debug %>% 
        write_delim(.,delim=";",col_names=TRUE,file=paste0(debug,prefisso,".regressionAlterati.csv"))
    }


    dati.xts[.indexDate(dati.xts) %in% timestampOutliers,1]<<-NA
    flag.regression[.indexDate(dati.xts) %in% timestampOutliers,1]<<-1  
    
    #flag.regression e dati.xts hanno lo stesso index
    
  },inizio.mese=inizioMese,fine.mese=fineMese,SIMPLIFY = FALSE)

  #restituisco i giorni di flag.regression laddove maggiore di 0
  #restituiamo i giorni invalidati oppure NULL

  index(flag.regression[flag.regression[,1]>0,])

  
}#fine regression_test


################################################################
#Corroboration precipitation
################################################################

corroborationTest.precipitation<-function(dati.xts,
                                          lista.limitrofe.xts,
                                          nmin.limitrofe=3,
                                          numero.dati.media=100,
                                          finestra=14,
                                          extraOut=NULL,
                                          soglia.base=269.24,
                                          ...){

  stopifnot(is.xts(dati.xts) && is.list(lista.limitrofe.xts))
  
  #POICHÈ sto lavorando con oggetti xts (e quindi sono le varie funzioni associate che cercando i dati
  #in base al timestamp e dove mancano non sono riportati --> l'alternativa sarebbe riempire tutto il calendario
  #e gestire i dati in base alla prossimita. Il 15 luglio verrà dopo il 14<-- ma così non è efficente)
  #posso tranquillamente elimiare le righe di dati NA. Questo comporta ridurre il numero di cicli inutili
  
  dati.xts[!is.na(dati.xts[,c(1)]),]->dati.xts
  if(!nrow(dati.xts)) return(NULL)
  
  #alcuni dati outili se scriviOut è una stringa (directory)
  xtsAttributes(dati.xts)$codice->codice
  xtsAttributes(dati.xts)$parametro->parametro	      
  
  nrow(dati.xts)->totaleRighe
  
  #vettore dome memorizziamo i flag
  xts(rep(0,totaleRighe),order.by=index(dati.xts))->flag.corroboration  
  
  #lista con il numero di anni distinti di ciascuna serie
  unique(year(dati.xts))->anni
  length(anni)->numero.anni
  
  #se il numero di anni è troppo ridotto, il controllo per calcolare la media fallirà
  #sempre, inutile in questo caso procedere
  (2*finestra+1)->fff
  if((numero.anni*fff)< numero.dati.media){
    message("Numero di anni insuf. per procedere al controllo, esco")
    return()
  }
  
  #le medie di lungo periodo delle limitrofe le devo calcolare all'interno del periodo di dati
  #della stazione target. Ad esempio: se per la stazione target la media di lungo periodo copre
  #il periodo 51-80, la media della limitrofa nn la vado a calcolare dal 91 al 2015.
  #Quindi devo verificare: che le limitrofe abbiano all'interno del periodo della target
  #un numero suff. di dati per calcolare la media di lungo periodo. Se non è soddisfatta questa condizione
  #per le limitrofe inutile fare il controllo ..e aspettare inutilmente un sacco di tempo che 
  #il controllo finisca
  #lista in cui ogni elemento contiene gli anni (non ripetuti) presenti nelle serie.
  
  lapply(lista.limitrofe.xts,FUN=function(serie){
    
    stopifnot(is.xts(serie))
    unique(year(serie))->serie.anni
    length(serie.anni[serie.anni %in%anni])*fff->numero.dati.utili
    if(numero.dati.utili<numero.dati.media) return(NULL)
    
    numero.dati.utili    
    
  })->lista.numero.anni.limitrofe
  

  names(lista.numero.anni.limitrofe)<-names(lista.limitrofe.xts)
  Filter(Negate(is.null),lista.numero.anni.limitrofe)->lista.numero.anni.limitrofe
  
  if(length(lista.numero.anni.limitrofe) <nmin.limitrofe){
    message("Numero di anni insuf. per procedere al controllo, esco")
    return()
  } #<--- variabile globale, modificare!!!
  
  
  ################MEMOISE DATA FOR RANKING CALCULATION
  vector(mode="list",length=366)->listaTarget

  #da questo momento in poi names(lista.numero.anni.limitrofe) mi permette di acquisire i codici delle
  #limitrofe da utilizzare effettivamente per fare il controllo "corroboration"
  lapply(1:totaleRighe,FUN=function(riga){
    
    #if(is.na(dati.xts[riga,1])) return() #non necessario, dati già filtrati
    
    index(dati.xts[riga,])->giornoMeseAnno
    
    giornoMeseAnno-days(1)->giornoMeseAnno_meno1
    giornoMeseAnno+days(1)->giornoMeseAnno_piu1
    
    day(giornoMeseAnno)->giorno
    month(giornoMeseAnno)->mese
    
    if(giorno==29 && mese==2) return()
    #partendo da giorno e mese devo prendere la finestra per ogni anno tra quelli
    #della serie target
    giornoMeseAnnoUpdated<-as.Date(paste(anni[1],str_pad(mese,width =2,pad ="0",side="left"),giorno,sep="-"))
    #print(giornoMeseAnno)
    
    ####memoise
    yday(as.Date(giornoMeseAnno))->posizione
    if(!leap_year(giornoMeseAnno) && mese>2) posizione<-posizione+1
    ####memoise
    
    #se trovo NA allora calcolo giorniMeseInWindow e la memorizzo nella
    #lista in modo di non dover ripeter il calcolo ogni volta
    if(is.null(listaTarget[[posizione]])){
          lapply(1:numero.anni,FUN=function(ii){
            window(x=dati.xts,start=giornoMeseAnnoUpdated+years(ii-1)-days(finestra),end=giornoMeseAnnoUpdated+years(ii-1)+days(finestra))
          })->giorniMeseInWindow
          
          #df.giorniMeseInWindow contiene: giorno mese, finestra di 14 giorni (avanti e dietro),per ognuno degli
          #anni della serie
          do.call("rbind",giorniMeseInWindow)[,1]->>listaTarget[[posizione]]
    }#fine if su listaTarget

    df.giorniMeseInWindow<-listaTarget[[posizione]]
    
    #ok calcolo rank di lungo periodo. Target è il valore che corrisponde a indice, valori sono
    #tutti i valori su una finestra di 29 giorni nel lungo periodo 
    calcolaPercentRank(target=as.vector(dati.xts[riga,1]),valori=as.vector(df.giorniMeseInWindow[,1]),nmin.rank=20)->target.rank
    if(is.na(target.rank)) return() 
    
    #ciclo su stazioni limitrofe
    lapply(names(lista.numero.anni.limitrofe),FUN=function(nome.limitrofa){
      
      #estraiamo giorno prima e giorno dopo del giorno annoMeseGiorno
      #utilizzando gli oggetti xts non dobbiamopreoccuparci di verificare se il giorno
      #èeffettivamente presente nella serie, possiamo evitare l'assunzione che le serie
      #siano complete nel calendario (cosa di cui dovevamo preoccuparci nell'implementazione precedente)
      as.vector(window(lista.limitrofe.xts[[nome.limitrofa]],start=giornoMeseAnno_meno1,end = giornoMeseAnno_piu1)[,1])->valori.limitrofa
      if(length(valori.limitrofa)<3 || any(is.na(valori.limitrofa))) return(NULL)
      
      #se sono arrivato qui, posso pensare che la stazione limitrofa è buona
      #per fare il confronto, cioè posso calcolare le differenze pairwise
      #tra il valore target e il valore della limitrofa, con il suo giorno prima
      #e il suo giorno dopo. Che possa calcolare anche il rank non è detto perchè
      #i valori della serie limitrofa potrebbero essere 0 e quindi restituire un 
      #NA
      
      #confronto valore target con valore limitrofa su tre giorni
      (as.vector(dati.xts[riga,1])-valori.limitrofa)->pairwise.difference      
      
      #se tutte queste differenze sono positive o negative
      #significa che il valore è al di fuori del range dei valori
      #delle tre limitrofe. Se invece le differenze presentano segno più
      #e segno meno allora il valore target rientra nel range delle limitrofe
      
      #i controlli fatti prima escludono che minTargetNB possa essere NA
      ifelse(all(pairwise.difference>0) || all(pairwise.difference<0),min(abs(pairwise.difference)),0)->minTargetNB 
      
      #per ogni stazione limitrofa calcoliamo il rank sul lungo periodo.Lo stesso lungo periodo
      #su cui abbiamo calcolato il rank per la stazione target
      as.vector(lista.limitrofe.xts[[nome.limitrofa]][.indexDate(lista.limitrofe.xts[[nome.limitrofa]]) %in% .indexDate(df.giorniMeseInWindow)][,1])->valori.limitrofa.long
      #calcolo dei rank per ciascuno deitre valori
      unlist(lapply(valori.limitrofa,calcolaPercentRank,valori=valori.limitrofa.long,nmin.rank=20))->limitrofa.rank
      

      corrThreshold<-soglia.base      
      #se i rank sono tutti differenti da NA, aggiorna il threshold
      if(all(!is.na(limitrofa.rank))){
        (target.rank-limitrofa.rank)->pairwise.difference.rank
        min(abs(pairwise.difference.rank))->minTargetNB.rank
        corrThreshold<-(soglia.base-45.72*log(minTargetNB.rank))
      }#per calcolo threshold
      
      return(list(minTargetNB,corrThreshold))
      
    })->listaOut #fine lapply su limitrofe
    
    lapply(listaOut,function(x)x[[1]])->listaMinTargetNb
    lapply(listaOut,function(x)x[[2]])->listaCorrThreshold
    names(listaMinTargetNb)<-names(lista.limitrofe.xts)
    names(listaCorrThreshold)<-names(lista.limitrofe.xts)   
   
    Filter(Negate(is.null),listaMinTargetNb)->listaMinTargetNb
    Filter(Negate(is.null),listaCorrThreshold)->listaCorrThreshold
    stopifnot(length(listaMinTargetNb)==length(listaCorrThreshold))
    
    if(length(listaMinTargetNb)<3) return() #non abbiamo abbastanza stazioni limitrofe per il test

    if(min(unlist(listaMinTargetNb)) <= min(unlist(listaCorrThreshold))) return() #controllo fallito    
 
    dati.xts[riga,1]<<-NA
    flag.corroboration[riga,1]<<-1
    if(nchar(extraOut)){
      sink(paste0(extraOut,"/corroborationp.txt"),append=TRUE)
        cat(paste0(codice,";",min(unlist(listaMinTargetNb)),";",min(unlist(listaCorrThreshold)),"\n"))
      sink()
    }#fine if su scriviOut
      
  }) #lapply su riga
  
  #restituiamo i giorni invalidati oppure NULL
  index(flag.corroboration[flag.corroboration[,1]>0,])
  
}#fine corroboration_test

######################################################################
# CALCOLA RANK
######################################################################

calcolaPercentRank<-function(target,valori,nmin.rank=20){ 
  
  messaggio.errore(argomento=match.call(),parametri.mai.nulli=c("valori","target"))

  if(target<0.1 || is.na(target)) return(NA)
  
  valori[!is.na(valori) & valori >=0.1]->valori
  if(!length(valori)) return(NA)
  
  #rank solo su valori di precipitazione maggiore di 0
  
  #se sono meno di 100 i valori non NA  non posso calcolare rank
  #se il valore è pari a 0 restituisco NA: non faccio il corroboration test sui dati nulli
  if(length(valori)<nmin.rank) return(NA)
  
  #valori: vettore con valori nella serie che corrispondono a un determinato giorno e mese
  #e tutti i giorni che rientrano in una finestra di dimensione winSize
  
  rank(valori,ties.method="average")->vettore.rank
  which(valori==target)->index.target
  
  #deve per forza essere un valore
  #stopifnot(length(index.target)>0) #### <-15 luglio
  #In realtà, se prendiamo le limitrofe nello stesso periodo in cui sono disponibili
  #i dati della stazione target, potrebbe nonessere valido. La limitrofa potrebbeavere valido un giorno ma la target no.
  #Che succederebbe? Che se vado a prendere i dati nel lungo periodo quel valore che manca alla target non lo prendo
  #ma questo valore potrebbe essere presente quando prendo un valore e i suoi due giorni adiacenti. Avrei cosi: target
  #un valore dicuicalcolo il rank (valore della limitrofa,disponibile) non presente nel vettore valori (valori di lungo periodo)
  #dove non compare perchè nella stazione target questo valore non è disponibile.
  
  #prendiamo dei rank quello che corrisponde a "target" il valore della stazione. Siccome possoono esserci "ties" il rank
  #viene calcolato come media dei rank risultato. Tutti i valori corrispondenti a "target" avranno lo stesso rank, per questo
  #restituisco solo il primo elelmento di index.target
  return((vettore.rank[index.target[1]]/length(vettore.rank))*100)
  
}#calcolo rank




