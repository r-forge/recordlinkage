
print.results <- function (object, show="all", show_prediction=T, sort="prediction",num=nrow(object$valid),skip=0)
{
    ids=switch(show,links=which(object$prediction),nonlinks=which(!object$prediction),
               possible=which(is.na(object$prediction)),TRUE)
    #print(ids)               
    if (object$type=="deduplication")
    {   
        data1=object$data
        data2=data1
    } else
    {
        data1=object$data1
        data2=object$data2
    }
    if (!is.null(object$Wdata))
    {
        result=cbind(Weight=object$Wdata[ids],
                     data1[object$valid[ids,1],],
                     data2[object$valid[ids,2],])
        o=order(result$Weight,decreasing=T)
        ind=o[(skip+1):min(skip+num,length(o))]
#        if (show_prediction)
#            result$prediction=object$prediction[ind]
      printfun=function(x)
      {
          c(x[1:((length(x)+1)/2)],c("",x[((length(x)+3)/2):length(x)]))
          
      }
      m=apply(result[ind,],1,printfun)
      
      return(as.data.frame(matrix(m[T],nrow=ncol(m)*2,ncol=nrow(m)/2,byrow=T)))
    } else
    {
        result=cbind(data1[object$valid[,1],],
                 data2[object$valid[,2],])
    }
#     result=merge(merge(object$valid[,1:2],cbind(seq(1,nrow(object$data)), object$data),
#                 by.x=1, by.y=1),
#                 cbind(seq(1:nrow(object$data)),object$data),by.x=2,by.y=1)
    # Sortiermechanismus nochmal �berdenken, man hat ja noch keine Vorstellung
    # von der Paartabelle!
#     if (is.numeric(sort_cols))
#         sort_cols=result[,sort+2] # +2 because of ids
#     if (is.logical)
#     if (!is.list(sort_cols))
#         sort_cols=list(sort_cols)
#    return(result[do.call(order,sort_cols),])
    return(result[ids,])
}
        


print.range <- function(object,threshold_upper=Inf,threshold_lower=-Inf,
					single.rows=F, show="all",
					sort=T)
{
    if (object$type=="deduplication")
    {   
        data1=object$data
        data2=data1
    } else
    {
        data1=object$data1
        data2=object$data2
    }
	ind=which(object$Wdata<threshold_upper & object$Wdata>=threshold_lower)
	if (!is.null(object$prediction))
	{
		show.ind=switch(show,links=which(object$prediction[ind]),
						nonlinks=which(!object$prediction[ind]),
               			possible=which(is.na(object$prediction[ind])),TRUE)
		ind=ind[show.ind]		
	} else if (!missing(show) && is.null(object$prediction))
		warning("No prediction vector found, returning all data pairs!")


    pairs=cbind(Weight=object$Wdata[ind],
                    data1[object$valid[ind,1],],
                    data2[object$valid[ind,2],])
	if (sort)
	{
    	o=order(pairs$Weight,decreasing=T)
    	pairs=pairs[o,]
    }
    
	if (single.rows) 
	{
		colnames(pairs)=c("Weight",paste(colnames(data1),".1",sep=""),
								   paste(colnames(data2),".2",sep=""))
		return (pairs)
	}
	printfun=function(x)
    {
        c(x[1:((length(x)+1)/2)],c("",x[((length(x)+3)/2):length(x)]))
        
    }
    m=apply(pairs,1,printfun)
    m=as.data.frame(matrix(m[T],nrow=ncol(m)*2,ncol=nrow(m)/2,byrow=T))
    colnames(m)=c("Weight",colnames(data1))
    return(m)
}