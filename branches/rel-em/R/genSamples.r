genSamples = function (dataset, num.non, des.prop=0.1)
{   

	
    # run clustering algorithm?
    # res_clust <- bclust(x=ddmat, centers=3,base.center=2, base.method="kmeans", iter.base=100, verbose=TRUE)
    # kmendundet<-kmeans(x=ddmat, centers=nclasses); pairs$pairs[kclusr$cluster==2,]
    # Use EM algorithm instead    
#     ddpairs <- emWeights(datapairs)
# 	ddclass <- emClassify(ddpairs)
	ddclass=classifyUnsup(dataset,method="bclust")    
  return(splitData(ddclass, use.pred=TRUE, num.non=num.non, des.prop=des.prop))
}
    
    


splitData <- function(dataset,prop,keep.prop=FALSE,num.non=0,des.prop=0,use.pred=FALSE)
{
	train=dataset
	valid=dataset
  pairs=dataset$pairs
	n_data=nrow(pairs)
	pairs$is_match=as.logical(pairs$is_match)
	

	if (use.pred)
	{
		if (is.null(dataset$prediction))
			stop ("No prediction vector in dataset! Call with use.pred=FALSE.")
		pairs$is_match=dataset$prediction=="L"
	}

	if (!missing(num.non) && !missing(des.prop))
	{
   	 	#ids=as.matrix(ret$pairs[,c(1:2)]) # any filters will be set
    	ids <- seq(from=1,to=n_data)
	    nlink <- round(des.prop*(num.non)) 
	    ngesamt <- num.non+nlink 
	    if (ngesamt > n_data)
			stop("Inconsistent values for training data!")
	    if (des.prop<0 || des.prop >=1)
			stop("Inconsistent value for link proportion!")		

    	linksid <- ids[pairs$is_match]
    	nonlinksid <- ids[!pairs$is_match]
    
	    alidn <- length(linksid)
   		anolidn <- length(nonlinksid)

	    nmark=FALSE; mmark=FALSE;
    	if(nlink > alidn) { warning("Only ", alidn, " Links!"); nlink=alidn; nmark=TRUE }
    	if(num.non > anolidn) { warning("Only ", anolidn, " Non-Links!"); num.non=anolidn; mmark=TRUE }
    	salid <- resample(linksid, size=nlink)
    	sanolid <- resample(nonlinksid, size=num.non)
    
	    train=dataset
    	valid=dataset
    	trainid <- c(salid,sanolid)
    	trainhelp <- ids %in% trainid
    	linkhelp <- ids %in% salid
    	nonlinkhelp <- ids %in% sanolid
	    valid$pairs <- dataset$pairs[!trainhelp,]
    	train$pairs <- dataset$pairs[trainhelp,] 
		
		# split prediction vector, if present
		if (!is.null(dataset$prediction))
		{
			valid$prediction <- dataset$prediction[!trainhelp]
			train$prediction <- dataset$prediction[trainhelp]
		}
							  						
		# split weight vector, if present
		if (!is.null(dataset$Wdata))
		{
			valid$Wdata=dataset$Wdata[!trainhelp]
			train$Wdata=dataset$Wdata[trainhelp]

			valid$M=dataset$M
			valid$U=dataset$U
			valid$W=dataset$W
			train$M=dataset$M
			train$U=dataset$U
			train$W=dataset$W
		}
    	return(list(train=train,valid=valid))
	}

	if (isFALSE(keep.prop))
	{
		s=sample(1:n_data,n_data*prop)
		train$pairs=dataset$pairs[s,]
		valid$pairs=dataset$pairs[-s,]

		# split prediction vector, if present
		if (!is.null(dataset$prediction))
		{
			train$prediction <- dataset$prediction[s]
			valid$prediction <- dataset$prediction[-s]
		}
							  						
		# split weight vector, if present
		if (!is.null(dataset$Wdata))
		{
			train$Wdata=dataset$Wdata[s]
			valid$Wdata=dataset$Wdata[-s]
			valid$M=dataset$M
			valid$U=dataset$U
			valid$W=dataset$W
			train$M=dataset$M
			train$U=dataset$U
			train$W=dataset$W
		}

		return (list(train=train, valid=valid))
	}
	
	if (isTRUE(keep.prop))
	{
		match_ind=which(as.logical(pairs$is_match))
		n_match=length(match_ind)
		if (n_match==0)
			stop("No matches found! Call with keep.prop=FALSE.")
		s_match=sample(1:n_match,n_match*prop)
		s_non_match=sample(1:(n_data-n_match),(n_data-n_match)*prop)
		train$pairs=rbind(dataset$pairs[match_ind[s_match],],
						  dataset$pairs[-match_ind,][s_non_match,])
		valid$pairs=rbind(dataset$pairs[match_ind[-s_match],],
						  dataset$pairs[-match_ind,][-s_non_match,])

		# split prediction vector, if present
		if (!is.null(dataset$prediction))
		{
			train$prediction=c(dataset$prediction[match_ind[s_match]],
						  	   dataset$prediction[-match_ind][s_non_match])
			valid$prediction=c(dataset$prediction[match_ind[-s_match]],
						  	   dataset$prediction[-match_ind][-s_non_match])
		}
							  						
		# split weight vector, if present
		if (!is.null(dataset$Wdata))
		{
			train$Wdata=c(dataset$Wdata[match_ind[s_match]],
						  	   dataset$Wdata[-match_ind][s_non_match])
			valid$Wdata=c(dataset$Wdata[match_ind[-s_match]],
						  	   dataset$Wdata[-match_ind][-s_non_match])
			valid$M=dataset$M
			valid$U=dataset$U
			valid$W=dataset$W
			train$M=dataset$M
			train$U=dataset$U
			train$W=dataset$W
		}

		return (list(train=train,valid=valid))
	}
	
}


getMinimalTrain <- function(rpairs, nEx=1)
{
  p=rpairs$pairs
  # Zeilen markieren, um Paare identifizieren zu können
  rownames(p)=1:nrow(p)
  p[is.na(p)]=0
  # pro vorhandenem Vergleichsmuster werden bis zu nEx
  # Repräsentanten gezogen
  trainind=unlist(tapply(1:nrow(p),p[,-c(1,2,ncol(p))],
    function(x) if (length(x) > 0) return (x[sample(1:length(x),
      min(length(x),nEx))])
    else return (NULL),
    simplify=FALSE))
  train=rpairs
  train$pairs=p[trainind,]
  train$Wdata=rpairs$Wdata[trainind]
  train$prediction=rpairs$prediction[trainind,]
  return(train)
}


#splitXValSets <- function (dataset, nXVal=10, keep.prop=TRUE)
#{
#  if (length(intersect(class(dataset),c("RecLinkData","RecLinkResult")))==0)
#    stop("Dataset has illegal class!")
#  pairs=dataset$pairs
#	n_data=nrow(pairs)
#	pairs$is_match=as.logical(pairs$is_match)
#
#  retlist=list()
#
#	if (isTRUE(keep.prop))
#	{
#    # Matche bestimmen und zufällige Permutation erzeugen
#		match_ind=which(pairs$is_match)
#		n_matches=length(match_ind)
#    if (n_matches==0)
#			stop("No matches found! Call with keep.prop=FALSE.")
#    match_ind=sample(match_ind,n_matches)
#
#    # Non-Matche bestimmen und zufällige Permutation erzeugen
#		non_match_ind=which(!pairs$is_match)
#    n_non_matches=length(non_match_ind)
#		non_match_ind=sample(non_match_ind,n_non_matches)
#    xval_size_matches=n_matches/nXVal
#    xval_size_non_matches=n_non_matches/nXVal
#	  for (i in 1:nXVal)
#	  {
#	    xval_ind_matches=match_ind[(1+round((i-1)*xval_size_matches)):round(i*xval_size_matches)];
#      xval_ind_non_matches=non_match_ind[(1+round((i-1)*xval_size_non_matches)):round(i*xval_size_non_matches)];  
#    retlist[[i]]=dataset
#    retlist[[i]]$pairs=dataset$pairs[c(xval_ind_matches,xval_ind_non_matches),];
#    if (!is.null(dataset$Wdata))  
#      retlist[[i]]$Wdata=dataset$Wdata[c(xval_ind_matches,xval_ind_non_matches)]
#	  }
#	}
#  return(retlist)
#}
#