# getPairs.r: functions to view and edit record pairs

getPairs <- function(rpairs,threshold.upper=Inf,threshold.lower=-Inf,
					single.rows=FALSE, show="all",
					sort=!is.null(rpairs$Wdata))
{
    if (rpairs$type=="deduplication")
    {   
        data1=rpairs$data
        data2=data1
    } else
    {
        data1=rpairs$data1
        data2=rpairs$data2
    }
	if (!is.null(rpairs$Wdata))
  {
    ind=which(rpairs$Wdata<threshold.upper & rpairs$Wdata>=threshold.lower)
    weights <- rpairs$Wdata
  } 
  else
  {
    ind <- 1:nrow(rpairs$pairs)
    weights <- rep(NA, nrow(rpairs$pairs))
  }
	if (!is.null(rpairs$prediction))
	{
		show.ind=switch(show,links=which(rpairs$prediction[ind]=="L"),
						nonlinks=which(rpairs$prediction[ind]=="N"),
               			possible=which(rpairs$prediction[ind]=="P"),
						FP=which(rpairs$prediction=="L" &
							rpairs$pairs$is_match==FALSE),
						FN=which(rpairs$prediction=="N" &
							rpairs$pairs$is_match==TRUE),
							
							TRUE)
		ind=ind[show.ind]		
	} else if (show != "all" && is.null(rpairs$prediction))
		{
      warning("No prediction vector found, returning all data pairs!")
    }
    pairs=data.frame(Weight=weights[ind],
                    id1=rpairs$pairs[ind,1],
                    data1[rpairs$pairs[ind,1],],
                    id2=rpairs$pairs[ind,2],
                    data2[rpairs$pairs[ind,2],])
	if (sort)
	{
    	o=order(pairs$Weight,decreasing=TRUE)
    	pairs=pairs[o,]
    }
    
	if (single.rows) 
	{
		colnames(pairs)=c("Weight", "id1", paste(colnames(data1),".1",sep=""),
								   "id2", paste(colnames(data2),".2",sep=""))
		return (pairs)
	}

	printfun=function(x)
    {
        c(x[1:((length(x)+1)/2)],c("",x[((length(x)+3)/2):length(x)]))
        
    }
    m=apply(pairs,1,printfun)
    m=as.data.frame(matrix(m[TRUE],nrow=ncol(m)*2,ncol=nrow(m)/2,byrow=TRUE))
    colnames(m)=c("Weight", "id", colnames(data1))
    return(m)
}

editMatch <- function (rpairs)
{
  if (rpairs$type=="deduplication")
  {
      data1=rpairs$data
      data2=data1
  } else
  {
      data1=rpairs$data1
      data2=rpairs$data2
  }
  p=data.frame(data1[rpairs$pairs[,1],],
                   data2[rpairs$pairs[,2],],
                   matrix("",nrow=nrow(rpairs$pairs),
                      ncol=ncol(rpairs$data)))

  # die komplizierten Umwandlungen sind so begründet: Um in der Spalte
  # "is_match" Leerzeichen zu haben (ein Wert bezieht sich immer auf zwei
  # Zeilen, dazu kommt eine Leerzeile), muss der Wertetyp Faktor sein. Dieser
  # muss am Ende wieder auf TRUE/FALSE umgewandelt werden.
  p=matrix(as.matrix(t(p))[TRUE],nrow=nrow(p)*3,byrow=TRUE)
  # unlist(lapply) statt sapply, weil man sonst eine Matrix bekommt
  p=data.frame(p,is_match=unlist(lapply(rpairs$pairs$is_match,function(x) c(x,"",""))))
  colnames(p)=c(colnames(data1),"is_match")
  p=edit(p)
  is_match=p[seq(1,nrow(p)-2,3),"is_match"]
  is_match=as.integer(levels(is_match)[as.integer(is_match)])
  rpairs$pairs$is_match <- is_match
  return(rpairs)
}

