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
        return(result[ind,])
#        if (show_prediction)
#            result$prediction=object$prediction[ind]
    } else
    {
        result=cbind(data1[object$valid[,1],],
                 data2[object$valid[,2],])
    }
#     result=merge(merge(object$valid[,1:2],cbind(seq(1,nrow(object$data)), object$data),
#                 by.x=1, by.y=1),
#                 cbind(seq(1:nrow(object$data)),object$data),by.x=2,by.y=1)
    # Sortiermechanismus nochmal überdenken, man hat ja noch keine Vorstellung
    # von der Paartabelle!
#     if (is.numeric(sort_cols))
#         sort_cols=result[,sort+2] # +2 because of ids
#     if (is.logical)
#     if (!is.list(sort_cols))
#         sort_cols=list(sort_cols)
#    return(result[do.call(order,sort_cols),])
    return(result[ids,])
}

        

print.range <- function(object,threshold_upper=Inf,threshold_lower=0)
{
    o=order(object$Wdata,decreasing=T)
		first=head(which(object$Wdata[o]<threshold_upper),1)
		last=tail(which(object$Wdata[o]>=threshold_lower),1)
    pairs=cbind(W=object$Wdata[o[first:last]],print.results(object,skip=first-1,num=last-first+1))
    printfun=function(x)
    {
        as.data.frame(rbind(x[1:((length(x)+1)/2)],c("",x[((length(x)+3)/2):length(x)])))
        
    }
    apply(pairs,1,printfun)
}
