#crea directory necessarie per memorizzare i risultati
#se cancella ==TRUE cancella le directory esistenti
DIR.RISULTATI<<-"./extraqc_spatial/"

creaDirOut<-function(dir.risultati=NULL,lista.directory=NULL,cancella=TRUE){

	messaggio.errore(argomento=match.call(),parametri.mai.null=c("dir.risultati","lista.directory"))

	crea<-function(dir.base){

		lapply(lista.directory,FUN=function(nome){
		
			nome<-paste0(dir.base,nome)

        		try(dir.create(nome))
		
			message(sprintf("CREO DIRECTORY %s:\n",nome))

		}) #fine lapply

	}#fine crea


 	if(cancella) try( unlink(dir.risultati,recursive=TRUE) )

	dir.create(dir.risultati)
	crea(dir.base=dir.risultati)

}#fine funzione


#questa funzione evrifica i parametri. Se esiste il parametro "parametri.mai.nulli"
#conterrà la lista dei parametri che non dovranno essere mai nulli. Se "parametri.mai.nulli" non esiste
#allora verranno controllati tutti i parametri della funzione
messaggio.errore<-function(argomento,parametri.mai.nulli=NULL){

	argomento[[1]]->nome.funzione
	
	if(is.null(parametri.mai.nulli)){

		#qui ottengo la lista di tutti i parametri (grazie a formals)
		names(formals(as.character(nome.funzione)))->tutti.i.parametri

		for(iii in 1:length(tutti.i.parametri)){

			tutti.i.parametri[iii]->nome.parametro
			#se il parametro ha un valore preimpostato (default) anche se non compare 
			#in argomento non dovrà generare un errore
			if(!is.null(formals(as.character(nome.funzione)))) next
			if(!(nome.parametro %in% names(argomento))) stop(sprintf(paste0("Errore funzione '",nome.funzione,"' parametro '%s' mancante"),nome.parametro))

		}

	}else{ #fine if

		for(iii in 1:length(parametri.mai.nulli)){

			parametri.mai.nulli[iii]->nome.parametro
			if(!(nome.parametro %in% names(argomento))) stop(sprintf(paste0("Errore funzione '",nome.funzione,"' parametro '%s' mancante"),nome.parametro))

		}

	}

}#fine messaggio.errore

################àoutput generico messaggio
messaggi<-function(testo){
	message("##############################################")	
	message(testo)
	message("##############################################")	
}

