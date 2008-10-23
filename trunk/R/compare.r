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
#  phonfun      Funktion, die für einen String einen phonetischen Code berechnet
#               Standard: "Hannoveraner" Phonetik
#
#               strcmpfun und phonfun müssen vektorisierbar sein. 
#
#  exclude      Vektor mit Indizes von Spalten, die von Paarbildung und Matching
#               ausgenommen sind. Erlaubt zum Beispiel, externe identifier zu
#               verwenden.
#
#               Indizes, die in exclude erscheinen, sollten nicht in 
#               phonetic, strcmp und blockfld verwendet werden. Für strcmp
#               und blockfld werden solche Indizes stillschweigend entfernt,
#               in blockfld verursachen sie einen Fehler.

# Rückgabe: Stringvergleich gibt bei "NA" 0 zurück!

# blockfun, phonetic: noch nicht implementiert 

# Binär und Fuzzy in Ergebnisliste? (auf Wunsch)






# Weitere mögliche Parameter:
#  phonfun         Funktion, die als Argument eine String hat und dessen phonetische
#                  Codierung zurückgibt.


# Rückgabe: Liste mit Elementen patterns (Tabelle aus Vergleichsmustern), 
# frequencies (durchschnittliche relative Häufigkeiten der Attribute)



compare <- function(dataset, blockfld=FALSE, phonetic=FALSE,
                    phonfun=F, strcmp=FALSE,strcmpfun=FALSE, exclude=F, 
                    identity=NA,num_non=0, des_prop=0.05, adjust=F)
{
    # various catching of erronous input
    if (!is.data.frame(dataset) && !is.matrix(dataset))
        stop ("Illegal format of dataset")
    ndata=nrow(dataset) # number of records
    nfields=ncol(dataset)
    if (ndata<2) 
        stop ("dataset must contain at least two records")
    if (!is.numeric(strcmp) && !is.logical(strcmp))
        stop ("strcmp must be numeric or a single logical value")
    if (!is.numeric(phonetic) && !is.logical(phonetic))
        stop ("phonetic must be numeric or a single logical value")
    if (max(strcmp)>nfields|| any(is.na(strcmp)))
        stop ("strcmp contains out of bounds index")
    if (max(phonetic)>nfields || any(is.na(phonetic)))
        stop ("phonetic contains out of bounds index")
    if (!is.numeric(exclude) && !isFALSE(exclude))
        stop ("exclude must be numeric or FALSE")
    if (max(exclude)>nfields)
        stop ("exclude contains out of bounds index")

    n_matches <- round(des_prop*(num_non))
    n_train <- num_non+n_matches 
    if (n_train > ndata)
        stop("Inconsistent values for training data!")
    if (des_prop<0 || des_prop >=1)
        stop("Inconsistent value for link proportion!")
    
    ret=list()  # return object
    # rownames(ret$data)=1:ndata
    # handle excluded columns
    full_data=as.matrix(dataset)


    # keep phonetics for blocking fields
    if (is.numeric(phonetic))
    {
        phonetic_block=intersect(phonetic,unlist(blockfld))
    }
    if (is.numeric(exclude))
    {        
        dataset=dataset[,-exclude]  # remove excluded columns
        # adjust indices to list of included fields
        if (is.numeric(phonetic)) 
        {
            phonetic=setdiff(phonetic,exclude)
            phonetic=sapply(phonetic,function(x) return (x-length(which(exclude<x))))
        }
        if (is.numeric(strcmp))
        {
            strcmp=setdiff(strcmp,exclude)
            strcmp=sapply(strcmp,function(x) return (x-length(which(exclude<x))))       
        }
        # no longer neccessary:
        #blockfld=lapply(blockfld,function(x) return (x-length(which(exclude<x))))
    }
    # issue a warning if both phonetics and string metric are used on one field
    if ((length(intersect(phonetic,strcmp))>0 && !isFALSE(strcmp) && !isFALSE(phonetic)) ||
         (isTRUE(strcmp) && !isFALSE(phonetic)) ||
         (isTRUE(phonetic) && !isFALSE(strcmp)))
    {
        warning(sprintf("Both phonetics and string metric are used on some fields",length(intersect(phonetic,strcmp))))
    }
    dataset[dataset==""]=NA # label missing values
    full_data[full_data==""]=NA # label missing values
    dataset=as.matrix(dataset)        


    if (!is.function(phonfun))
        phonfun=pho_h

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

        
    
    pair_ids=matrix(0,nrow=0,ncol=2) # each row holds indices of one record pair
    if (!is.list(blockfld)) blockfld=list(blockfld)
    for (blockelem in blockfld) # loop over blocking definitions
    {
      if (isTRUE(phonetic))
      {
        block_data=phonfun(full_data)
      } else if (is.numeric(phonetic))
      {
        block_data=full_data
        block_data[,phonetic_block]=phonfun(full_data[,phonetic_block])
      } else
      {
        block_data=full_data
      }
      # for each record, concatenate values in blocking fields
      blockstr=apply(block_data,1,function(x) paste(x[blockelem],collapse=" "))
      # delete.NULLs(tapply(...)) gives for each value of the blocking string
      # indices of matching records. From these lapply builds record pairs via
      # unordered_pairs (defined in tool.r). unlist makes a vector from the
      # resulting list which is reshaped as a matrix in the following line
      id_vec=unlist(lapply(delete.NULLs(tapply(1:ndata,blockstr,function(x) if(length(x)>1) return(x))),unordered_pairs))
      # reshape vector and attach to matrix of record pairs
      if (!is.null(id_vec))
        pair_ids=rbind(pair_ids,t(matrix(id_vec,ncol=length(id_vec)/2,nrow=2)))
    }
    # return empty data frame if no pairs are obtained
    if (length(pair_ids)==0)
    {
        ret$pairs=data.frame() 
    } else
    {
      pair_ids=unique(matrix(as.integer(pair_ids),ncol=ncol(pair_ids),nrow=nrow(pair_ids)))
      pair_ids=cbind(1:nrow(pair_ids),pair_ids) # add index for each pair
      colnames(pair_ids)=c("pair_id","id_1","id_2")
      
      # get data for left hand side of record pairs    
      left=merge(pair_ids[,c(1,2),drop=F],cbind(1:nrow(dataset),dataset),by.x=2,by.y=1,sort=F)
      left=left[order(left[,"pair_id"]),] # merge scrambles rows, reorder
      # get data for right hand side of record pairs
      right=merge(pair_ids[,c(1,3),drop=F],cbind(1:nrow(dataset),dataset),by.x=2,by.y=1,sort=F)
      right=right[order(right[,"pair_id"]),] # merge scrambles rows, reorder
      # matrix to hold comparison patterns
      patterns=matrix(0,ncol=ncol(left)-2,nrow=nrow(left)) 
      if (isTRUE(strcmp))
      {
          patterns=strcmpfun(as.matrix(left[,-(1:2)]),as.matrix(right[,-(1:2)]))
      } else if (is.numeric(strcmp)) 
      {
          patterns[,-strcmp]=(as.matrix(left[,-c(1,2,strcmp+2)])==as.matrix(right[,-c(1,2,strcmp+2)]))*1
          patterns[,strcmp]=strcmpfun(as.matrix(left[,strcmp+2]),as.matrix(right[,strcmp+2])) #*1
      } else
      {
          patterns=(as.matrix(left[,-c(1,2)])==as.matrix(right[,-c(1,2)]))*1    
      }                    
      patterns=cbind(pair_ids[,-1,drop=F],as.data.frame(patterns))
      patterns=patterns[order(patterns[,1],patterns[,2]),] # muss nicht unbedingt sein, evtl Argument
      colnames(patterns)=c("id1","id2",colnames(dataset))
      rownames(patterns)=NULL
    }
    # Trainingsdaten ziehen
    is_match=identity[patterns[,1]]==identity[patterns[,2]] # Matchingstatus der Paare
    match_ids=which(is_match) # Indizes von Matchen
    non_match_ids=which(!is_match) # Indizes von Non-Matchen
    if (length(match_ids) < n_matches)
        warning("Only ", length(match_ids), " Links!")
    if (length(non_match_ids) < num_non)
        warning("Only ", length(non_match_ids), " Non-Links!")
#      print(match_ids)
#     print(non_match_ids)
    # resample: safe for vector of length 1
    train_ids_match=resample(match_ids,min(n_matches,length(match_ids)))
    train_ids_non_match=resample(non_match_ids,min(num_non,length(non_match_ids)))
#     print(train_ids_match)
#     print(train_ids_non_match)
#     if (length(train_ids_match)==0)
#         stop ("No matches in training set!")
#     if (length(train_ids_non_match)==0)
#         stop ("No non-matches in training set!")
#     print("Debug")
#     print(summary(train_ids_match))
#     print(summary(patterns))
#     print(summary(patterns[train_ids_match,,drop=F]))
#     print(summary(cbind(patterns[train_ids_non_match,,drop=F],is_match[train_ids_non_match])))
#     print(summary(cbind(patterns[train_ids_match,,drop=F],is_match[train_ids_match])))
    train=rbind(cbind(patterns[train_ids_match,,drop=F],is_match=is_match[train_ids_match]), # Matche
                cbind(patterns[train_ids_non_match,,drop=F],is_match=is_match[train_ids_non_match])) # Non-Matche
    valid_ids=setdiff(1:nrow(patterns), c(train_ids_match,train_ids_non_match))
#     print(valid_ids)
    valid=rbind(cbind(patterns[valid_ids,,drop=F],is_match=is_match[valid_ids]))                                         
    ret$data=as.data.frame(full_data)
    frequencies=apply(dataset,2,function(x) 1/length(unique(x)))
    ret$train=train
    ret$valid=valid
    ret$frequencies=frequencies
    #ret$identity=identity
    class(ret)="RecLinkData"
    return(ret)
}


 