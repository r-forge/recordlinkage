getPairs <- function(object,threshold_upper=Inf,threshold_lower=-Inf,
					single.rows=FALSE, show="all",
					sort=TRUE)
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
		show.ind=switch(show,links=which(object$prediction[ind]=="L"),
						nonlinks=which(object$prediction[ind]=="N"),
               			possible=which(object$prediction[ind]=="P"),TRUE)
		ind=ind[show.ind]		
	} else if (!missing(show) && is.null(object$prediction))
		warning("No prediction vector found, returning all data pairs!")


    pairs=cbind(Weight=object$Wdata[ind],
                    data1[object$pairs[ind,1],],
                    data2[object$pairs[ind,2],])
	if (sort)
	{
    	o=order(pairs$Weight,decreasing=TRUE)
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
    m=as.data.frame(matrix(m[TRUE],nrow=ncol(m)*2,ncol=nrow(m)/2,byrow=TRUE))
    colnames(m)=c("Weight",colnames(data1))
    return(m)
}
