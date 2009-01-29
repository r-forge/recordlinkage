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



compare_dedup <- function(dataset, blockfld=FALSE, phonetic=FALSE,
                    phonfun=F, strcmp=FALSE,strcmpfun=FALSE, exclude=F, 
                    identity=NA ,num_non=0, des_prop=0.05, adjust=F)
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

        
# print("blocking beginnt")
   pair_ids=matrix(as.integer(0),nrow=0,ncol=2) # each row holds indices of one record pair
   if (!is.list(blockfld)) blockfld=list(blockfld)
   if (isFALSE(blockfld))
   {
   	pairs_ids=t(unordered_pairs(nrow(dataset)))
   } else
   { 
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
#       print("blockstr")
      blockstr=apply(block_data,1,function(x) paste(x[blockelem],collapse=" "))
      rm(block_data)
      # delete.NULLs(tapply(...)) gives for each value of the blocking string
      # indices of matching records. From these lapply builds record pairs via
      # unordered_pairs (defined in tools.r). unlist makes a vector from the
      # resulting list which is reshaped as a matrix in the following line
#       print("tapply")
#       ta_res=tapply(1:ndata,blockstr,function(x) if(length(x)>1) return(x))
#       print("delete.NULLs")
#       del_res=delete.NULLs(ta_res)
#       print("lapply")
#       la_res=lapply(del_res,unordered_pairs)
#       print("unlist")
#       id_vec=unlist(la_res)
#    browser()

#    browser()

#      print ("tapply")
     id_vec=tapply(1:ndata,blockstr,function(x) if(length(x)>1) return(x))
#      print("delete.NULLs")
     id_vec=delete.NULLs(id_vec)
#      print("lapply")
     id_vec=lapply(id_vec,unordered_pairs)
#      print("unlist")
     id_vec=unlist(id_vec)
#     id_vec=unlist(lapply(delete.NULLs(tapply(1:ndata,blockstr,function(x) if(length(x)>1) return(x))),unordered_pairs))

# alternativ: eine Klammer weniger, spart aber keine Zeit
#      f=function(x)
#      {
#         l=length(x)
#         if(l>1)
#             return (unordered_pairs(x))
#      }
#      id_vec=unlist(delete.NULLs(tapply(1:ndata,blockstr,f)))
     
      rm(blockstr)
      # reshape vector and attach to matrix of record pairs
#       print("pair_ids")
      if (!is.null(id_vec))
       pair_ids=rbind(pair_ids,matrix(id_vec,nrow=length(id_vec)/2,ncol=2,byrow=T))
       rm(id_vec)
    }
    
#   print("blocking beendet")
  ret$data=as.data.frame(full_data)
  rm(full_data)
  # return empty data frame if no pairs are obtained
  if (length(pair_ids)==0)
  {
      stop("No pairs generated. Check blocking criteria.")
  }
  
#     print("vor unique")
    pair_ids=as.matrix(unique(as.data.frame(pair_ids)))  # runs faster with data frame
   } # end else
   
#     print("nach unique")
#     print(nrow(pair_ids))
#     print("merge")
    left=dataset[pair_ids[,1],]
    right=dataset[pair_ids[,2],]
#     print("nach merge")

#     print("Vergleich")
    # matrix to hold comparison patterns
    patterns=matrix(0,ncol=ncol(left),nrow=nrow(left)) 
    if (isTRUE(strcmp))
    {
        patterns=strcmpfun(as.matrix(left),as.matrix(right))
    } else if (is.numeric(strcmp)) 
    {
        patterns[,-strcmp]=(as.matrix(left[,-strcmp])==as.matrix(right[,-strcmp]))*1
        patterns[,strcmp]=strcmpfun(as.matrix(left[,strcmp]),as.matrix(right[,strcmp])) #*1
    } else
    {
       patterns=(left==right)*1
#          patterns=(as.matrix(left)==as.matrix(right))*1    
    }
    rm(left)
    rm(right)
#      return(patterns)
                        
    #p=as.data.frame(pair_ids)
    #rm(pair_ids)

#     print("Trainingsdaten ziehen")
    # Trainingsdaten ziehen
    is_match=identity[pair_ids[,1]]==identity[pair_ids[,2]] # Matchingstatus der Paare
    match_ids=which(is_match) # Indizes von Matchen
    non_match_ids=which(!is_match) # Indizes von Non-Matchen
    if (length(match_ids) < n_matches)
        warning("Only ", length(match_ids), " Links!")
    if (length(non_match_ids) < num_non)
        warning("Only ", length(non_match_ids), " Non-Links!")
    # sample training data
    train_ids_match=resample(match_ids,min(n_matches,length(match_ids)))
    train_ids_non_match=resample(non_match_ids,min(num_non,length(non_match_ids)))
    valid_ids=setdiff(1:nrow(patterns), c(train_ids_match,train_ids_non_match))
#     if (length(train_ids_match)+length(train_ids_non_match)!=0)
#     {
#         valid_ids=1:nrow(patterns)[-c(train_ids_match,train_ids_non_match)]
#     } else valid_ids=1:nrow(patterns)

#     p=as.data.frame(pair_ids)
# 
#     p[,3:(2+ncol(patterns))]=patterns
#    patterns=p[order(p[,1],p[,2]),] # muss nicht unbedingt sein, evtl Argument
#     patterns=p
#    rm(p)
#      patterns=cbind(as.data.frame(pair_ids[,-1,drop=F]),as.data.frame(patterns))
#      patterns=patterns[order(patterns[,1],patterns[,2]),] # muss nicht unbedingt sein, evtl Argument
#     print("Daten zusammenführen")
    train_ids=c(train_ids_match,train_ids_non_match)
    ret$train=as.data.frame(
					cbind(pair_ids[train_ids,,drop=F],
                    patterns[train_ids,,drop=F],
                    is_match[train_ids])) # Matche

#     print(valid_ids)
    
    ret$valid=as.data.frame(
					cbind(pair_ids[valid_ids,,drop=F],
                    patterns[valid_ids,,drop=F],
                    is_match[valid_ids]))



#     ret$valid=ret$valid[order(ret$valid[,1],ret$valid[,2]),]
#     ret$train=ret$train[order(ret$train[,1],ret$train[,2]),]
    colnames(ret$train)=c("id1","id2",colnames(dataset),"is_match")
    colnames(ret$valid)=colnames(ret$train)
    rownames(ret$train)=NULL
    rownames(ret$valid)=NULL

#    rm(patterns)                                         
    ret$frequencies=apply(dataset,2,function(x) 1/length(unique(x)))
    #ret$identity=identity
    ret$type="deduplication"
    class(ret)="RecLinkData"
    return(ret)
}


# Version für zwei Datenmengen

# Bedingung: Format der Datenmengen gleich

# einfache Belegung für identity zum Beispiel: ersten Datensatz durchnummerieren,
# zweiter kriegt Index des passenden Datums oder 0

compare_linkage <- function(dataset1, dataset2, blockfld=FALSE, phonetic=FALSE,
                    phonfun=F, strcmp=FALSE,strcmpfun=FALSE, exclude=F, 
                    identity1=NA, identity2=NA, num_non=0, des_prop=0.05, adjust=F)
{
    # various catching of erronous input
    if (!is.data.frame(dataset1) && !is.matrix(dataset1))
        stop ("Illegal format of dataset1")
    if (!is.data.frame(dataset2) && !is.matrix(dataset2))
        stop ("Illegal format of dataset2")
    if (ncol(dataset1) != ncol(dataset2))
        stop ("Data sets have different format")
    ndata1=nrow(dataset1) # number of records
    ndata2=nrow(dataset2)
    nfields=ncol(dataset1)
    if (ndata1<1 || ndata2<1) 
        stop ("empty data set")
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
#     if (n_train > ndata)
#         stop("Inconsistent values for training data!")
    if (des_prop<0 || des_prop >=1)
        stop("Inconsistent value for link proportion!")
    
    ret=list()  # return object
    # rownames(ret$data)=1:ndata
    # handle excluded columns
    full_data1=as.matrix(dataset1)
    full_data2=as.matrix(dataset2)



    # keep phonetics for blocking fields
    if (is.numeric(phonetic))
    {
        phonetic_block=intersect(phonetic,unlist(blockfld))
    }
    if (is.numeric(exclude))
    {        
        dataset1=dataset1[,-exclude]  # remove excluded columns
        dataset2=dataset2[,-exclude]  # remove excluded columns
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
    dataset1[dataset1==""]=NA # label missing values
    dataset2[dataset2==""]=NA # label missing values
    full_data1[full_data1==""]=NA # label missing values
    full_data2[full_data2==""]=NA # label missing values
    dataset1=as.matrix(dataset1)        
    dataset2=as.matrix(dataset2)        

    if (!is.function(phonfun))
        phonfun=pho_h

    if (!isFALSE(phonetic)) # true, if phonetic is T or not a logical value
    {
        if (isTRUE(phonetic)) # true, if phonetic is a logical value and T
        {    
            dataset1=pho_h(dataset)
            dataset2=pho_h(dataset)
        } else # phonetic is not a logical value
        dataset1[,phonetic]=pho_h(dataset1[,phonetic])
        dataset2[,phonetic]=pho_h(dataset2[,phonetic])
    }
    
    if (!is.function(strcmpfun))
        strcmpfun=jarowinkler

        
# print("blocking beginnt")
   pair_ids=matrix(as.integer(0),nrow=0,ncol=2) # each row holds indices of one record pair
   if (!is.list(blockfld)) blockfld=list(blockfld)
   if (isFALSE(blockfld))
   {  # full outer join
  	 pairs_ids=merge(1:nrow(dataset1),1:nrow(dataset2),all=T)
   } else
   {
    for (blockelem in blockfld) # loop over blocking definitions
    {
      if (isTRUE(phonetic))
      {
        block_data1=phonfun(full_data1)
        block_data2=phonfun(full_data2)
      } else if (is.numeric(phonetic))
      {
        block_data1=full_data1
        block_data1[,phonetic_block]=phonfun(full_data1[,phonetic_block])
        block_data2=full_data2
        block_data2[,phonetic_block]=phonfun(full_data2[,phonetic_block])
      } else
      {
        block_data1=full_data1
        block_data2=full_data2
      }
      # for each record, concatenate values in blocking fields
#       print("blockstr")
      blockstr1=apply(block_data1,1,function(x) paste(x[blockelem],collapse=" "))
      blockstr2=apply(block_data2,1,function(x) paste(x[blockelem],collapse=" "))
      rm(block_data1)
      rm(block_data2)
#	browser()
      id_vec=merge(data.frame(id1=1:ndata1,blockstr=blockstr1),
                   data.frame(id2=1:ndata2,blockstr=blockstr2))[,-1]

      rm(blockstr1)
      rm(blockstr2)
      # reshape vector and attach to matrix of record pairs
#       print("pair_ids")
      if (!is.null(id_vec))
        pair_ids=rbind(pair_ids,id_vec)
      rm(id_vec)
    }
    
#   print("blocking beendet")
  ret$data1=as.data.frame(full_data1)
  ret$data2=as.data.frame(full_data2)
  rm(full_data1,full_data2)
  # return empty data frame if no pairs are obtained
  if (length(pair_ids)==0)
  {
      stop("No pairs generated. Check blocking criteria.")
  }
  
#     print("vor unique")
    pair_ids=unique(as.data.frame(pair_ids))  # runs faster with data frame
  } # end else
#     print("nach unique")
#     print(nrow(pair_ids))
#     print("merge")
    left=dataset1[pair_ids[,1],,drop=F]
    right=dataset2[pair_ids[,2],,drop=F]
#     print("nach merge")

#     print("Vergleich")
    # matrix to hold comparison patterns
    patterns=matrix(0,ncol=ncol(left),nrow=nrow(left)) 
    if (isTRUE(strcmp))
    {
        patterns=strcmpfun(as.matrix(left),as.matrix(right))
    } else if (is.numeric(strcmp)) 
    {
        patterns[,-strcmp]=(as.matrix(left[,-strcmp])==as.matrix(right[,-strcmp]))*1
        patterns[,strcmp]=strcmpfun(as.matrix(left[,strcmp]),as.matrix(right[,strcmp])) #*1
    } else
    {
       patterns=(left==right)*1
#          patterns=(as.matrix(left)==as.matrix(right))*1    
    }
    rm(left)
    rm(right)
#      return(patterns)
                        
    #p=as.data.frame(pair_ids)
    #rm(pair_ids)

#     print("Trainingsdaten ziehen")
    # Trainingsdaten ziehen
    is_match=identity1[pair_ids[,1]]==identity2[pair_ids[,2]] # Matchingstatus der Paare
    match_ids=which(is_match) # Indizes von Matchen
    non_match_ids=which(!is_match) # Indizes von Non-Matchen
    if (length(match_ids) < n_matches)
        warning("Only ", length(match_ids), " Links!")
    if (length(non_match_ids) < num_non)
        warning("Only ", length(non_match_ids), " Non-Links!")
    # sample training data
	    train_ids_match=resample(match_ids,min(n_matches,length(match_ids)))
	    train_ids_non_match=resample(non_match_ids,min(num_non,length(non_match_ids)))
	    valid_ids=setdiff(1:nrow(patterns), c(train_ids_match,train_ids_non_match))
#     if (length(train_ids_match)+length(train_ids_non_match)!=0)
#     {
#         valid_ids=1:nrow(patterns)[-c(train_ids_match,train_ids_non_match)]
#     } else valid_ids=1:nrow(patterns)

#     p=as.data.frame(pair_ids)
# 
#     p[,3:(2+ncol(patterns))]=patterns
#    patterns=p[order(p[,1],p[,2]),] # muss nicht unbedingt sein, evtl Argument
#     patterns=p
#    rm(p)
#      patterns=cbind(as.data.frame(pair_ids[,-1,drop=F]),as.data.frame(patterns))
#      patterns=patterns[order(patterns[,1],patterns[,2]),] # muss nicht unbedingt sein, evtl Argument
#     print("Daten zusammenführen")
    train_ids=c(train_ids_match,train_ids_non_match)
    ret$train=as.data.frame(
					cbind(pair_ids[train_ids,,drop=F],
                    patterns[train_ids,,drop=F],
                    is_match[train_ids])) # Matche

#     print(valid_ids)
    
    ret$valid=as.data.frame(
					cbind(pair_ids[valid_ids,,drop=F],
                    patterns[valid_ids,,drop=F],
                    is_match[valid_ids]))



#     ret$valid=ret$valid[order(ret$valid[,1],ret$valid[,2]),]
#     ret$train=ret$train[order(ret$train[,1],ret$train[,2]),]
    colnames(ret$train)=c("id1","id2",colnames(dataset1),"is_match")
    colnames(ret$valid)=colnames(ret$train)
    rownames(ret$train)=NULL
    rownames(ret$valid)=NULL

#    rm(patterns)                                         
    ret$frequencies=apply(rbind(dataset1,dataset2),2,function(x) 1/length(unique(x)))
    #ret$identity=identity
    ret$type="linkage"
    class(ret)="RecLinkData"
    return(ret)
}

 