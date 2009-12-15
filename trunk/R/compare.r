# compare.r: Functions to group records to pairs.

compare.dedup <- function(dataset, blockfld=FALSE, phonetic=FALSE,
                    phonfun=pho_h, strcmp=FALSE,strcmpfun=jarowinkler, exclude=FALSE, 
                    identity=NA, n_match=NA, n_non_match=NA)
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

    ret=list()  # return object
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

    if (!isFALSE(phonetic)) # true, if phonetic is TRUE or not a logical value
    {
        if (isTRUE(phonetic)) # true, if phonetic is a logical value and TRUE
        {    
            dataset=pho_h(dataset)
        } else # phonetic is not a logical value
        dataset[,phonetic]=pho_h(dataset[,phonetic])
    }
    
    if (!is.function(strcmpfun))
        strcmpfun=jarowinkler

        
# print("blocking beginnt")
   pair_ids=matrix(as.integer(0),nrow=0,ncol=2) # each row holds indices of one record pair
   if (isFALSE(blockfld))
   {
    if (is.na(n_match) || is.na(n_non_match))
   	{
	   pair_ids=t(unorderedPairs(nrow(dataset)))
	 } else
	 {
		tempdat=data.frame(id=1:ndata,identity=identity)
		
		# Matche bestimmen durch Join auf identity-Vektor 
		pairs=merge(x=tempdat,y=tempdat,by=2)
		# auf ungeordnete Matche beschränken 
		match_ids=as.matrix(pairs[as.integer(pairs[,2])<as.integer(pairs[,3]),2:3])
		# benötigte Anzahl ziehen
		if (n_match>nrow(match_ids))
		{
			warning(sprintf("Only %d matches!",nrow(match_ids)))			
		} else
		{
			s=sample(nrow(match_ids),n_match)
			match_ids=match_ids[s,]
		}
		

   	   # bereits gezogene Paare werden in A markiert
		A=list()
		for (i in 1:n_non_match)
		{
			if (i %% 100 == 0)
			repeat
		 	{
		  		d1=sample(ndata,1)
		  		d2=sample((1:ndata)[-d1],1)
				# Wenn ein Match gezogen wurde, nochmal versuchen
				if (identical(identity[d1],identity[d2]))
				{
				 	next
				}
				# Wenn das gezogene Paar schon gezogen wurde, nochmal versuchen 
				if (!is.null(A[[paste(d1,d2)]]))
			  	{
			    	next
			  	}
			  	# markiere das Paar in der Liste als bereits gezogen
			  	A[[paste(d1,d2)]]=c(d1,d2)
			  	break
			}
		}
		cat("\n")
		non_match_ids=matrix(unlist(A),ncol=2,nrow=n_non_match,byrow=TRUE)
		pair_ids=rbind(match_ids,non_match_ids)
	 	rm(match_ids,non_match_ids,A)
	 
	 }
   } else
   { 
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
	  # exclude pairs with NA in blocking variable
	  # (paste just converts to "NA")
      for (i in blockelem)
      {
        is.na(blockstr)=is.na(block_data[,i])
      }
#    	blockstr=gsub("NA", NA, blockstr)
      rm(block_data)
     id_vec=tapply(1:ndata,blockstr,function(x) if(length(x)>1) return(x))
     id_vec=delete.NULLs(id_vec)
     id_vec=lapply(id_vec,unorderedPairs)
     id_vec=unlist(id_vec)
#     id_vec=unlist(lapply(delete.NULLs(tapply(1:ndata,blockstr,function(x) if(length(x)>1) return(x))),unorderedPairs))

      rm(blockstr)
      # reshape vector and attach to matrix of record pairs
      if (!is.null(id_vec))
       pair_ids=rbind(pair_ids,matrix(id_vec,nrow=length(id_vec)/2,ncol=2,byrow=TRUE))
       rm(id_vec)
    }
   } # end else
    
  ret$data=as.data.frame(full_data)
  rm(full_data)
  # return empty data frame if no pairs are obtained
  if (length(pair_ids)==0)
  {
      stop("No pairs generated. Check blocking criteria.")
  }
  
    pair_ids=as.matrix(unique(as.data.frame(pair_ids)))  # runs faster with data frame
   
    left=dataset[pair_ids[,1],]
    right=dataset[pair_ids[,2],]
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
    }
    rm(left)
    rm(right)

    is_match=identity[pair_ids[,1]]==identity[pair_ids[,2]] # Matchingstatus der Paare
    ret$pairs=as.data.frame(
					cbind(pair_ids,
                    patterns,
                    is_match)) # Matche

    colnames(ret$pairs)=c("id1","id2",colnames(dataset),"is_match")
    rownames(ret$pairs)=NULL

    ret$frequencies=apply(dataset,2,function(x) 1/length(unique(x)))
    ret$type="deduplication"
    class(ret)="RecLinkData"
    return(ret)
}


# Version für zwei Datenmengen

# Bedingung: Format der Datenmengen gleich

# einfache Belegung für identity zum Beispiel: ersten Datensatz durchnummerieren,
# zweiter kriegt Index des passenden Datums oder 0

compare.linkage <- function(dataset1, dataset2, blockfld=FALSE, phonetic=FALSE,
                    phonfun=pho_h, strcmp=FALSE,strcmpfun=jarowinkler, exclude=FALSE, 
                    identity1=NA, identity2=NA, n_match=NA, n_non_match=NA)
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

    
    ret=list()  # return object
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

    if (!isFALSE(phonetic)) # true, if phonetic is TRUE or not a logical value
    {
        if (isTRUE(phonetic)) # true, if phonetic is a logical value and TRUE
        {    
            dataset1=pho_h(dataset1)
            dataset2=pho_h(dataset2)
        } else # phonetic is not a logical value
        dataset1[,phonetic]=pho_h(dataset1[,phonetic])
        dataset2[,phonetic]=pho_h(dataset2[,phonetic])
    }
    
    if (!is.function(strcmpfun))
        strcmpfun=jarowinkler

        
   pair_ids=matrix(as.integer(0),nrow=0,ncol=2) # each row holds indices of one record pair
   if (isFALSE(blockfld))
   { 
     if (is.na(n_match) || is.na(n_non_match))
   	 {
        # full outer join
    	 pair_ids=merge(1:nrow(dataset1),1:nrow(dataset2),all=TRUE)
	}   else
	 {
		tempdat1=data.frame(id=1:ndata1,identity=identity1)
		tempdat2=data.frame(id=1:ndata2,identity=identity2)
		
		# Matche bestimmen durch Join auf identity-Vektor 
		pairs=merge(x=tempdat1,y=tempdat2,by=2)
		match_ids=as.matrix(pairs[,2:3])
		# benötigte Anzahl ziehen
		if (n_match>nrow(match_ids))
		{
			warning(sprintf("Only %d matches!",nrow(match_ids)))			
		} else
		{
			s=sample(nrow(match_ids),n_match)
			match_ids=match_ids[s,]
		}
		

   	   # bereits gezogene Paare werden in A markiert
		A=list()
		for (i in 1:n_non_match)
		{
			repeat
		 	{
		  		d1=sample(ndata1,1)
		  		d2=sample(ndata2,1)
				# Wenn ein Match gezogen wurde, nochmal versuchen
				if (identical(identity1[d1],identity2[d2]))
				{
				 	next
				}
				# Wenn das gezogene Paar schon gezogen wurde, nochmal versuchen 
				if (!is.null(A[[paste(d1,d2)]]))
			  	{
			    	next
			  	}
			  	# markiere das Paar in der Liste als bereits gezogen
			  	A[[paste(d1,d2)]]=c(d1,d2)
			  	break
			}
		}
		
		non_match_ids=matrix(unlist(A),ncol=2,nrow=n_non_match,byrow=TRUE)
		pair_ids=rbind(match_ids,non_match_ids)
	 	rm(match_ids,non_match_ids,A)
	 
	 }	
	
   } else
   {
    if (!is.list(blockfld)) blockfld=list(blockfld)
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
      blockstr1=apply(block_data1,1,function(x) paste(x[blockelem],collapse=" "))
      blockstr2=apply(block_data2,1,function(x) paste(x[blockelem],collapse=" "))
      rm(block_data1)
      rm(block_data2)
  	  # exclude pairs with NA in blocking variable 
      for (i in blockelem)
      {
        is.na(blockstr1)=is.na(block_data1[,i])
        is.na(blockstr2)=is.na(block_data2[,i])
      }
	  id_vec=merge(data.frame(id1=1:ndata1,blockstr=blockstr1),
                   data.frame(id2=1:ndata2,blockstr=blockstr2))[,-1]

      rm(blockstr1)
      rm(blockstr2)
      # reshape vector and attach to matrix of record pairs
      if (!is.null(id_vec))
        pair_ids=rbind(pair_ids,id_vec)
      rm(id_vec)
    }
  if (length(pair_ids)==0)
  {
      stop("No pairs generated. Check blocking criteria.")
  }
  
    pair_ids=unique(as.data.frame(pair_ids))  # runs faster with data frame
  } # end else
    
  ret$data1=as.data.frame(full_data1)
  ret$data2=as.data.frame(full_data2)
  rm(full_data1,full_data2)
    left=dataset1[pair_ids[,1],,drop=FALSE]
    right=dataset2[pair_ids[,2],,drop=FALSE]
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
    }
    rm(left)
    rm(right)

    is_match=identity1[pair_ids[,1]]==identity2[pair_ids[,2]] # Matchingstatus der Paare
    ret$pairs=as.data.frame(cbind(pair_ids, patterns, is_match)) # Matche


    colnames(ret$pairs)=c("id1","id2",colnames(dataset1),"is_match")
    rownames(ret$pairs)=NULL

    ret$frequencies=apply(rbind(dataset1,dataset2),2,function(x) 1/length(unique(x)))
    ret$type="linkage"
    class(ret)="RecLinkData"
    return(ret)
}

 