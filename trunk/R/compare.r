# Bildet Vergleichsmuster von Datensätzen
# Argumente:
#   dataset     data frame mit Datensätzen, zeilenweise
#   blockfld    Liste von Vektor mit Feldern, die zum Blocking verwendet werden.
#               Ein Vektor enthält jeweils eine Menge von Feldern, die 
#               gleichzeitig übereinstimmen müssen (Und-Verknüpfung). Für mehrere
#               Vektoren in der Liste wird das Blocking mehrmals mit den jeweiligen
#               Kriterien durchgeführt und alle so erhaltenen Mengen von 
#               Datensatzpaaren in die Ausgabe übernommen (Oder-Verknüpfung).
#               Wird nur ein Vektor oder Wert eingegeben, so wird dieser in
#               eine List mit einem Element (der Eingabe selbst) umgewandelt
#   blockfun    Benutzerdefiniertes Blockingkriterium. Eine Funktion,
#               die als Argumente zwei Zeilen von dataset hat und TRUE zurückgibt,
#               falls diese als Paar in die Vergleichsmuster übernommen werden
#               sollen, ansonsten FALSE.
#  ids          TRUE, falls die erste Spalte ids enthält oder ein Vektor der Länge
#               nrow(dataset) mit ids. Standardmäßig werden die 
#               Zeilen durchnummeriert.                                     
#  phonetic     Entweder ein Vektor mit Indizes von Spalten, die vor der Bildung der 
#               Vergleichsmuster in einen phonetischen Code transformiert werden
#               sollen, oder TRUE, falls dies für alle Felder geschehen soll, oder
#               FALSE, falls keine Phonetik verwendet wird.
#  strcmp       Entweder ein Vektor mit Indizes von Feldern, für die bei der
#               Bildung der Vergleichsmuster eine Stringmetrik verwendet werden
#               soll, oder TRUE, falls dies für alle Felder geschehen soll.
#
#  strcmpfun    Funktion, die als Argumente zwei Strings hat und einen 
#               Vergleichswert im Interval [0,1] zurückgibt. Standard: Jaro-Winkler


# Rückgabe: Stringvergleich gibt bei "NA" 0 zurück!

# blockfun, phonetic: noch nicht implementiert 

# Binär und Fuzzy in Ergebnisliste? (auf Wunsch)


# externe IDs gehen noch nicht! (Es wird mit id indiziert)




# Weitere mögliche Parameter:
#  keep            Vektor von Feldern, die unverändert in die Paare übernommen werden
#                  sollen, oder TRUE, falls dies für alle geschehen soll (damit können
#                  Paare gebildet werden, für die erst später Vergleichsmuster erstellt
#                  werden sollen)
#  strcmpfun       Funktion, die als Argumente zwei Strings hat und einen
#                  Vergleichswert im Intervall [0,1] zurückgibt. Default: jarowinkler
#  phonfun         Funktion, die als Argument eine String hat und dessen phonetische
#                  Codierung zurückgibt.


# Rückgabe: Liste mit Elementen patterns (Tabelle aus Vergleichsmustern), 
# frequencies (durchschnittliche relative Häufigkeiten der Attribute)




compare <- function(dataset, blockfld=FALSE, blockfun=NULL,ids=1:nrow(dataset),phonetic=FALSE,strcmp=FALSE,strcmpfun=F)
{
    ndata=nrow(dataset)
    if (isTRUE(ids))
    {
        # ids in Tabelle gegeben: Nehme erste Spalte aus den Daten heraus
        ext_ids=dataset[,1]
        ids=1:ndata
        dataset=as.matrix(dataset[,-1])
        if (!isFALSE(blockfld))
            blockfld=lapply(blockfld,"-",1)
        if (is.numeric(phonetic))
            phonetic=phonetic-1
        if (!isFALSE(strcmp))
            strcmp=strcmp-1
    } else if (length(ids)==ndata) # use external ids via argument
    {
        ext_ids=ids
        ids=1:ndata    
    } else 
    {
        ext_ids=1:ndata
        ids=ext_ids
    }

    dataset[dataset==""]=NA
        
    if (!isFALSE(phonetic)) # true, if phonetic is T or not a logical value
    {
        if (isTRUE(phonetic)) # true, if phonetic is a logical value and T
        {    
            dataset=pho_h(dataset)
        } else # phonetic is not a logical value
        dataset[,phonetic]=pho_h(dataset[,phonetic])
    }
    
    if (!is.function(strcmpfun))
        strcmpfun=jarowinkler
    
    if (!isFALSE(blockfld))
    {
        keys=ids   # ist das überhaupt nötig ?
        pairs=list()
        if (!is.list(blockfld))
            blockfld=list(blockfld)
        
        for (blockelem in blockfld)
        {
#           blockstr=apply(dataset[,blockelem,drop=F],1,paste,collapse="")
#           names(keys)=blockstr
          
          blocktable=list() # Liste: Blockingstring -> Vektor von Indizes mit
                            # diesem Blockingstring
          
          for (i in 1:ndata)
          { 
              blockstr=paste(dataset[i,blockelem],collapse="")
              x=blocktable[[blockstr,exact=T]]
              blocktable[[blockstr]]=c(x,keys[i])
          }
         # blocktable ist jetzt gefüllt
                  
          for (e in blocktable)
          {
              if (length(e) > 1)
              {
                  pairs[[length(pairs)+1]]=ordered_pairs(e)    
              }
          }  
        }
        if (length(pairs)==0)
            return (NULL)
        pairs=unlist(pairs)
        dim(pairs)=c(2,length(pairs)/2)  
        pairs=t(pairs)
    } else # blockfld==FALSE
    {
        pairs=t(ordered_pairs(ids))
    }    

    if (isTRUE(strcmp))
    {
       patterns=unique(cbind(pairs,strcmpfun(dataset[pairs[,1],,drop=F],
                                               dataset[pairs[,2],,drop=F])))
    } else 
    {
        patterns=unique(cbind(pairs,dataset[pairs[,1],,drop=F]==dataset[pairs[,2],,drop=F]))
        if (!identical(strcmp,F))
            patterns[,strcmp+2]=strcmpfun(dataset[pairs[,1],strcmp,drop=F],
                                               dataset[pairs[,2],strcmp,drop=F])
    }                    
    
    if (nrow(patterns)>1)
        patterns=patterns[order(patterns[,1],patterns[,2]),] # muss nicht unbedingt sein, evtl Argument
    patterns=as.data.frame(patterns)
        patterns[,1:2]=cbind(ext_ids[patterns[,1]],ext_ids[patterns[,2]])
    colnames(patterns)=c("id1","id2",colnames(dataset))
    rownames(patterns)=NULL
    # berechne Häufigkeiten für die Ausgabe
    frequencies=apply(dataset,2,function(x) 1/length(unique(x)))
    ret=list()
    ret$data=cbind(ids,dataset)
    ret$pairs=patterns
    ret$frequencies=frequencies
    class(ret)="RecLinkPairs"
    return(ret)
}


 